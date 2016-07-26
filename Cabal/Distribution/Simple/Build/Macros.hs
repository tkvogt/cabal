-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Build.Macros
-- Copyright   :  Simon Marlow 2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Generate cabal_macros.h - CPP macros for package version testing
--
-- When using CPP you get
--
-- > VERSION_<package>
-- > MIN_VERSION_<package>(A,B,C)
--
-- for each /package/ in @build-depends@, which is true if the version of
-- /package/ in use is @>= A.B.C@, using the normal ordering on version
-- numbers.
--
module Distribution.Simple.Build.Macros (
    generate,
    generatePackageVersionMacros,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Package
import Distribution.Version
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program.Db
import Distribution.Simple.Program.Types
import Distribution.Text

-- ------------------------------------------------------------
-- * Generate cabal_macros.h
-- ------------------------------------------------------------

-- | The contents of the @cabal_macros.h@ for the given configured package.
--
generate :: PackageDescription -> LocalBuildInfo -> ComponentLocalBuildInfo -> String
generate pkg_descr lbi clbi =
  "/* DO NOT EDIT: This file is automatically generated by Cabal */\n\n" ++
  generatePackageVersionMacros
    (package pkg_descr : map snd (componentPackageDeps clbi)) ++
  generateToolVersionMacros (configuredPrograms . withPrograms $ lbi) ++
  generateComponentIdMacro lbi clbi

-- | Helper function that generates just the @VERSION_pkg@ and @MIN_VERSION_pkg@
-- macros for a list of package ids (usually used with the specific deps of
-- a configured package).
--
generatePackageVersionMacros :: [PackageIdentifier] -> String
generatePackageVersionMacros pkgids = concat
  [ "/* package " ++ display pkgid ++ " */\n"
  ++ generateMacros "" pkgname version
  | pkgid@(PackageIdentifier name version) <- pkgids
  , let pkgname = map fixchar (display name)
  ]

-- | Helper function that generates just the @TOOL_VERSION_pkg@ and
-- @MIN_TOOL_VERSION_pkg@ macros for a list of configured programs.
--
generateToolVersionMacros :: [ConfiguredProgram] -> String
generateToolVersionMacros progs = concat
  [ "/* tool " ++ progid ++ " */\n"
  ++ generateMacros "TOOL_" progname version
  | prog <- progs
  , isJust . programVersion $ prog
  , let progid       = programId prog ++ "-" ++ display version
        progname     = map fixchar (programId prog)
        Just version = programVersion prog
  ]

-- | Common implementation of 'generatePackageVersionMacros' and
-- 'generateToolVersionMacros'.
--
generateMacros :: String -> String -> Version -> String
generateMacros macro_prefix name version =
  concat
  ["#define ", macro_prefix, "VERSION_",name," ",show (display version),"\n"
  ,"#define MIN_", macro_prefix, "VERSION_",name,"(major1,major2,minor) (\\\n"
  ,"  (major1) <  ",major1," || \\\n"
  ,"  (major1) == ",major1," && (major2) <  ",major2," || \\\n"
  ,"  (major1) == ",major1," && (major2) == ",major2," && (minor) <= ",minor,")"
  ,"\n\n"
  ]
  where
    (major1:major2:minor:_) = map show (versionBranch version ++ repeat 0)

-- | Generate the @CURRENT_COMPONENT_ID@ definition for the component ID
--   of the current package.
generateComponentIdMacro :: LocalBuildInfo -> ComponentLocalBuildInfo -> String
generateComponentIdMacro lbi clbi =
  concat $
      (case clbi of
        LibComponentLocalBuildInfo{} ->
          ["#define CURRENT_PACKAGE_KEY \"" ++ componentCompatPackageKey clbi ++ "\"\n"]
        _ -> [])
      ++
      ["#define CURRENT_COMPONENT_ID \"" ++ display (componentComponentId clbi) ++ "\"\n"
      ,"#define LOCAL_COMPONENT_ID \"" ++ display (localComponentId lbi) ++ "\"\n"
      ,"\n"]

fixchar :: Char -> Char
fixchar '-' = '_'
fixchar c   = c
