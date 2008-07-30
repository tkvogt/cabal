-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Install
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- High level interface to package installation.
-----------------------------------------------------------------------------
module Distribution.Client.Install (
    install,
    upgrade,
  ) where

import Data.List
         ( unfoldr )
import Control.Exception as Exception
         ( handle, Exception )
import Control.Monad
         ( when, unless )
import System.Directory
         ( getTemporaryDirectory, doesFileExist )
import System.FilePath ((</>),(<.>))

import Distribution.Client.Dependency
         ( resolveDependenciesWithProgress, PackagesVersionPreference(..)
         , upgradableDependencies )
import Distribution.Client.Dependency.Types (Progress(..), foldProgress)
import Distribution.Client.Fetch (fetchPackage)
-- import qualified Distribution.Client.Info as Info
import Distribution.Client.IndexUtils as IndexUtils
         ( getAvailablePackages, disambiguateDependencies )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.InstallPlan (InstallPlan)
import Distribution.Client.Setup
         ( InstallFlags(..), configureCommand, filterConfigureFlags )
import Distribution.Client.Tar (extractTarGzFile)
import Distribution.Client.Types as Available
         ( UnresolvedDependency(..), AvailablePackage(..)
         , AvailablePackageSource(..), Repo, ConfiguredPackage(..)
         , BuildResult(..) )
import Distribution.Client.SetupWrapper
         ( setupWrapper, SetupScriptOptions(..), defaultSetupScriptOptions )
import Distribution.Client.Reporting
         ( writeInstallPlanBuildReports )
import Distribution.Client.Logging
         ( writeInstallPlanBuildLog )
import Paths_cabal_install (getBinDir)

import Distribution.Simple.Compiler
         ( Compiler(compilerId), PackageDB(..) )
import Distribution.Simple.Program (ProgramConfiguration, defaultProgramConfiguration)
import Distribution.Simple.Configure (getInstalledPackages)
import qualified Distribution.Simple.Setup as Cabal
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Simple.Setup
         ( flagToMaybe )
import Distribution.Simple.Utils
         ( defaultPackageDesc, inDir, rawSystemExit, withTempDirectory )
import Distribution.Package
         ( PackageIdentifier(..), Package(..), thisPackageVersion )
import Distribution.PackageDescription as PackageDescription
         ( GenericPackageDescription(packageDescription)
         , readPackageDescription )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.Version
         ( Version, VersionRange(AnyVersion, ThisVersion) )
import Distribution.Simple.Utils as Utils (notice, info, die)
import Distribution.System
         ( buildOS, buildArch )
import Distribution.Text
         ( display )
import Distribution.Verbosity (Verbosity, showForCabal, verbose)
import Distribution.Simple.BuildPaths ( exeExtension )

data InstallMisc = InstallMisc {
    rootCmd    :: Maybe FilePath,
    libVersion :: Maybe Version
  }

-- |Installs the packages needed to satisfy a list of dependencies.
install, upgrade
  :: Verbosity
  -> PackageDB
  -> [Repo]
  -> Compiler
  -> ProgramConfiguration
  -> Cabal.ConfigFlags
  -> InstallFlags
  -> [UnresolvedDependency]
  -> IO ()
install verbosity packageDB repos comp conf configFlags installFlags deps =
  installWithPlanner planner
        verbosity packageDB repos comp conf configFlags installFlags
  where
    planner :: Planner
    planner | null deps = planLocalPackage verbosity comp configFlags
            | otherwise = planRepoPackages PreferLatestForSelected comp deps

upgrade verbosity packageDB repos comp conf configFlags installFlags deps =
  installWithPlanner planner
        verbosity packageDB repos comp conf configFlags installFlags
  where
    planner :: Planner
    planner | null deps = planUpgradePackages comp
            | otherwise = planRepoPackages PreferAllLatest comp deps

type Planner = Maybe (PackageIndex InstalledPackageInfo)
            -> PackageIndex AvailablePackage
            -> IO (Progress String String (InstallPlan BuildResult))

-- |Installs the packages generated by a planner.
installWithPlanner ::
           Planner
        -> Verbosity
        -> PackageDB
        -> [Repo]
        -> Compiler
        -> ProgramConfiguration
        -> Cabal.ConfigFlags
        -> InstallFlags
        -> IO ()
installWithPlanner planner verbosity packageDB repos comp conf configFlags installFlags = do
  installed <- getInstalledPackages verbosity comp packageDB conf
  available <- getAvailablePackages verbosity repos

  progress <- planner installed available

  notice verbosity "Resolving dependencies..."
  maybePlan <- foldProgress (\message rest -> info verbosity message >> rest)
                            (return . Left) (return . Right) progress
  case maybePlan of
    Left message -> die message
    Right installPlan -> do
      when (dryRun || verbosity >= verbose) $
        printDryRun verbosity installPlan

      unless dryRun $ do
        installPlan' <-
          executeInstallPlan installPlan $ \cpkg ->
            installConfiguredPackage configFlags cpkg $ \configFlags' apkg ->
              installAvailablePackage verbosity apkg $
                installUnpackedPackage verbosity (setupScriptOptions installed)
                                       miscOptions configFlags'
        writeInstallPlanBuildReports installPlan'
        writeInstallPlanBuildLog     installPlan'
        printBuildFailures installPlan'

  where
    setupScriptOptions index = SetupScriptOptions {
      useCabalVersion  = maybe AnyVersion ThisVersion (libVersion miscOptions),
      useCompiler      = Just comp,
      usePackageIndex  = if packageDB == UserPackageDB then index else Nothing,
      useProgramConfig = conf,
      useDistPref      = Cabal.fromFlagOrDefault
                           (useDistPref defaultSetupScriptOptions)
                           (Cabal.configDistPref configFlags),
      useLoggingHandle = Nothing,
      useWorkingDir    = Nothing
    }
    dryRun       = Cabal.fromFlag (installDryRun installFlags)
    miscOptions  = InstallMisc {
      rootCmd    = if Cabal.fromFlag (Cabal.configUserInstall configFlags)
                     then Nothing      -- ignore --root-cmd if --user.
                     else Cabal.flagToMaybe (installRootCmd installFlags),
      libVersion = Cabal.flagToMaybe (installCabalVersion installFlags)
    }

-- | Make an 'InstallPlan' for the unpacked package in the current directory,
-- and all its dependencies.
--
planLocalPackage :: Verbosity -> Compiler -> Cabal.ConfigFlags -> Planner
planLocalPackage verbosity comp configFlags installed available = do
  pkg <- readPackageDescription verbosity =<< defaultPackageDesc verbosity
  let -- The trick is, we add the local package to the available index and
      -- remove it from the installed index. Then we ask to resolve a
      -- dependency on exactly that package. So the resolver ends up having
      -- to pick the local package.
      available' = PackageIndex.insert localPkg available
      installed' = PackageIndex.deletePackageId (packageId localPkg) `fmap` installed
      localPkg = AvailablePackage {
        packageInfoId                = packageId pkg,
        Available.packageDescription = pkg,
        packageSource                = LocalUnpackedPackage
      }
      localPkgDep = UnresolvedDependency {
        dependency = thisPackageVersion (packageId localPkg),
        depFlags   = Cabal.configConfigurationsFlags configFlags
      }

  return $ resolveDependenciesWithProgress buildOS buildArch (compilerId comp)
             installed' available' PreferLatestForSelected [localPkgDep]

-- | Make an 'InstallPlan' for the given dependencies.
--
planRepoPackages :: PackagesVersionPreference -> Compiler
                 -> [UnresolvedDependency] -> Planner
planRepoPackages pref comp deps installed available = do
  deps' <- IndexUtils.disambiguateDependencies available deps
  return $ resolveDependenciesWithProgress buildOS buildArch (compilerId comp)
             installed available pref deps'

planUpgradePackages :: Compiler -> Planner
planUpgradePackages comp (Just installed) available = return $
  resolveDependenciesWithProgress buildOS buildArch (compilerId comp)
    (Just installed) available PreferAllLatest
    [ UnresolvedDependency dep []
    | dep <- upgradableDependencies installed available ]
planUpgradePackages comp _ _ =
  die $ display (compilerId comp)
     ++ " does not track installed packages so cabal cannot figure out what"
     ++ " packages need to be upgraded."

printDryRun :: Verbosity -> InstallPlan BuildResult -> IO ()
printDryRun verbosity plan = case unfoldr next plan of
  []   -> notice verbosity "No packages to be installed."
  pkgs -> notice verbosity $ unlines $
            "In order, the following would be installed:"
          : map display pkgs
  where
    next plan' = case InstallPlan.ready plan' of
      []      -> Nothing
      (pkg:_) -> Just (pkgid, InstallPlan.completed pkgid plan')
        where pkgid = packageId pkg

printBuildFailures :: InstallPlan BuildResult -> IO ()
printBuildFailures plan =
  case [ (pkg, reason)
       | InstallPlan.Failed pkg reason <- InstallPlan.toList plan ] of
    []     -> return ()
    failed -> die . unlines
            $ "Error: some packages failed to install:"
            : [ display (packageId pkg) ++ printFailureReason reason
              | (pkg, reason) <- failed ]
  where
    printFailureReason reason = case reason of
      DependentFailed pkgid -> " depends on " ++ display pkgid
                            ++ " which failed to install."
      UnpackFailed    e -> " failed while unpacking the package."
                        ++ " The exception was:\n  " ++ show e
      ConfigureFailed e -> " failed during the configure step."
                        ++ " The exception was:\n  " ++ show e
      BuildFailed     e -> " failed during the building phase."
                        ++ " The exception was:\n  " ++ show e
      InstallFailed   e -> " failed during the final install step."
                        ++ " The exception was:\n  " ++ show e
      _ -> ""

executeInstallPlan :: Monad m
                   => InstallPlan BuildResult
                   -> (ConfiguredPackage -> m BuildResult)
                   -> m (InstallPlan BuildResult)
executeInstallPlan plan installPkg = case InstallPlan.ready plan of
  [] -> return plan
  (pkg: _) -> do
    buildResult <- installPkg pkg
    let pkgid = packageId pkg
        updatePlan = case buildResult of
          BuildOk -> InstallPlan.completed pkgid
          _       -> InstallPlan.failed    pkgid buildResult depsResult
            where depsResult = DependentFailed pkgid
            -- So this first pkgid failed for whatever reason (buildResult)
            -- all the other packages that depended on this pkgid which we
            -- now cannot build we mark as failing due to DependentFailed
            -- which kind of means it was not their fault.
    executeInstallPlan (updatePlan plan) installPkg

-- | Call an installer for an 'AvailablePackage' but override the configure
-- flags with the ones given by the 'ConfiguredPackage'. In particular the 
-- 'ConfiguredPackage' specifies an exact 'FlagAssignment' and exactly
-- versioned package dependencies. So we ignore any previous partial flag
-- assignment or dependency constraints and use the new ones.
--
installConfiguredPackage ::  Cabal.ConfigFlags -> ConfiguredPackage
                         -> (Cabal.ConfigFlags -> AvailablePackage  -> a)
                         -> a
installConfiguredPackage configFlags (ConfiguredPackage pkg flags deps)
  installPkg = installPkg configFlags {
    Cabal.configConfigurationsFlags = flags,
    Cabal.configConstraints = map thisPackageVersion deps
  } pkg

installAvailablePackage
  :: Verbosity -> AvailablePackage
  -> (GenericPackageDescription -> Maybe FilePath -> IO BuildResult)
  -> IO BuildResult
installAvailablePackage _ (AvailablePackage _ pkg LocalUnpackedPackage)
  installPkg = installPkg pkg Nothing

installAvailablePackage verbosity apkg@(AvailablePackage _ pkg _)
  installPkg = do
  pkgPath <- fetchPackage verbosity apkg
  tmp <- getTemporaryDirectory
  let pkgid = packageId pkg
      tmpDirPath = tmp </> ("TMP" ++ display pkgid)
      path = tmpDirPath </> display pkgid
  onFailure UnpackFailed $ withTempDirectory verbosity tmpDirPath $ do
    info verbosity $ "Extracting " ++ pkgPath ++ " to " ++ tmpDirPath ++ "..."
    extractTarGzFile tmpDirPath pkgPath
    let descFilePath = tmpDirPath </> display pkgid
                                  </> pkgName pkgid <.> "cabal"
    exists <- doesFileExist descFilePath
    when (not exists) $
      die $ "Package .cabal file not found: " ++ show descFilePath
    installPkg pkg (Just path)

installUnpackedPackage :: Verbosity
                   -> SetupScriptOptions
                   -> InstallMisc
                   -> Cabal.ConfigFlags
                   -> GenericPackageDescription
                   -> Maybe FilePath -- ^ Directory to change to before starting the installation.
                   -> IO BuildResult
installUnpackedPackage verbosity scriptOptions miscOptions configFlags pkg mpath
    = onFailure ConfigureFailed $ do
        setup configureCommand (filterConfigureFlags configFlags)
        onFailure BuildFailed $ do
          setup buildCommand (const Cabal.emptyBuildFlags)
          onFailure InstallFailed $ do
            case rootCmd miscOptions of
              (Just cmd) -> reexec cmd
              Nothing    -> setup Cabal.installCommand
                                  (const Cabal.emptyInstallFlags)
            return BuildOk
  where
    buildCommand     = Cabal.buildCommand defaultProgramConfiguration
    setup cmd flags  =
      setupWrapper verbosity
        scriptOptions { useWorkingDir = mpath }
        (Just $ PackageDescription.packageDescription pkg)
        cmd flags []
    reexec cmd = do
      -- look for our on executable file and re-exec ourselves using
      -- a helper program like sudo to elevate priviledges:
      bindir <- getBinDir
      let self = bindir </> "cabal" <.> exeExtension
      weExist <- doesFileExist self
      if weExist
        then inDir mpath $
               rawSystemExit verbosity cmd
                 [self, "install", "--only"
                 ,"--verbose=" ++ showForCabal verbosity]
        else die $ "Unable to find cabal executable at: " ++ self
               
-- helper
onFailure :: (Exception -> BuildResult) -> IO BuildResult -> IO BuildResult
onFailure result = Exception.handle (return . result)
