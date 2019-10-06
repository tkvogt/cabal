{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Distribution.FieldGrammar.Described (
    Described (..),
    describeDoc,
    describeHaskellString,
    describeUnqualComponentName,
    -- * Regular expressions
    Regex,
    reChar,
    reEps,
    reUnion,
    reAppend,
    reDigits,
    reDot,
    reUpperChar,
    reMunchCS,
    reMunch1CS,
    csChars,
    -- ** Details
    RE (..),
    CharSet (..),
    regexDoc,
    regexMatch,
    ) where

import Data.Char                   (isAlphaNum, isDigit)
import Data.String                 (IsString (..))
import Distribution.Compat.Prelude
import Data.Void (Void, absurd)
import Prelude ()

import Distribution.Parsec (Parsec)
import Distribution.Pretty (Pretty)

import qualified Data.Set         as Set
import qualified Text.PrettyPrint as PP

-- | Class describing the pretty/parsec format of a.
class (Pretty a, Parsec a) => Described a where
    -- | A pretty document of "regex" describing the field format
    describe :: proxy a -> Regex

-- | Pretty-print description.
--
-- >>> describeDoc ([] :: [Bool])
-- True|False
describeDoc :: Described a => proxy a -> PP.Doc
describeDoc p = regexDoc (describe p)

instance Described Bool where
    describe _ = REUnion ["True", "False"]

instance Described a => Described (Identity a) where
    describe _ = describe ([] :: [a])

-------------------------------------------------------------------------------
-- Common patterns
-------------------------------------------------------------------------------

describeHaskellString :: Regex
describeHaskellString = RENamed "haskell-string" $
    reChar '"' <>
    -- TODO
    reChar '"'

describeUnqualComponentName :: Regex
describeUnqualComponentName =
    --  TODO
    REString "unqual-component-name"

-------------------------------------------------------------------------------
-- Regex
-------------------------------------------------------------------------------

type Regex = RE String String Void

-- | Regular expressions tuned for 'Described' use-case.
--
-- Type arguments are
--
-- * @f@ names of functions (e.g. list)
-- * @x@ names of variables (e.g. module-name)
-- * @a@ variables for functions
--
data RE f x a
    = REAppend  [RE f x a]   -- ^ append @ab@
    | REUnion   [RE f x a]   -- ^ union @a|b@
    | REMunch   (RE f x a)   -- ^ star @a*@
    | REMunch1  (RE f x a)   -- ^ plus @a+@
    | REOpt     (RE f x a)   -- ^ optional @r?@
    | REString  String       -- ^ literal string @abcd@
    | RECharSet CharSet      -- ^ charset @[:alnum:]@
    | REPunct   Char         -- ^ punctuation character @,@ (can be surrounded by spaces)
    | RESpaces               -- ^ 1..many spaces
    | RENamed   x (RE f x a)
    | REVar     a
    | REApp     f (RE f x (Maybe a)) (RE f x a)
  deriving (Eq, Ord, Show, Functor)

instance Applicative (RE f x) where
    pure = REVar
    (<*>) = ap

instance Monad (RE f x) where
    return = pure

    REVar x     >>= k = k x
    RENamed n r >>= k = RENamed n (r >>= k)
    REApp n f x >>= k = REApp n (f >>= traverse k) (x >>= k)

    REAppend rs >>= k = REAppend (map (>>= k) rs)
    REUnion rs  >>= k = REAppend (map (>>= k) rs)
    REMunch r   >>= k = REMunch (r >>= k)
    REMunch1 r  >>= k = REMunch1 (r >>= k)
    REOpt r     >>= k = REOpt (r >>= k)
    REString s  >>= _ = REString s
    RECharSet s >>= _ = RECharSet s
    REPunct c   >>= _ = REPunct c
    RESpaces    >>= _ = RESpaces

instance IsString (RE f x a) where
    fromString = REString

instance Semigroup (RE f x a) where
    x <> y = reAppend (unAppend x ++ unAppend y) where
        unAppend (REAppend rs) = rs
        unAppend r             = [r]

instance Monoid (RE f x a) where
    mempty  = REAppend []
    mappend = (<>)

-------------------------------------------------------------------------------
-- Smart constructors
-------------------------------------------------------------------------------

reEps :: RE f x a
reEps = REAppend []

reUnion :: [Regex] -> Regex
reUnion [r] = r
reUnion rs  = REUnion rs

reAppend :: [RE f x a] -> RE f x a
reAppend [r] = r
reAppend rs  = REAppend rs

reChar :: Char -> Regex
reChar = RECharSet . CSChar

reDigits :: Regex
reDigits = REMunch1 (RECharSet CSDigit)

reDot :: Regex
reDot = REPunct '.'

reMunch1CS :: CharSet -> Regex
reMunch1CS = REMunch1 . RECharSet

reMunchCS :: CharSet -> Regex
reMunchCS = REMunch . RECharSet

reUpperChar :: Regex
reUpperChar = RECharSet CSUpper

csChars :: String -> CharSet
csChars = CSUnion . Set.fromList . map CSChar



-------------------------------------------------------------------------------
-- Character sets
-------------------------------------------------------------------------------

-- | Character sets.
--
-- Order of constructors is important.
-- @'CSDigit' < 'CSAlphaNum' < 'CSNotSpaceOrComma' < 'CSNotSpace'@
--
data CharSet
    = CSDigit               -- ^ decimal digits  @[:digit:]@
    | CSUpper               -- ^ upper characters @[:upper:]@
    | CSAlphaNum            -- ^ alpha-numeric   @[:alnum:]@
    | CSNotSpaceOrComma     -- ^ not space, nor comma: @[^ ,]@
    | CSNotSpace            -- ^ not space: @[^ ]@
    | CSChar Char           -- ^ single character
    | CSUnion (Set CharSet)
  deriving (Eq, Ord, Show)

-- | Union of 'CharSet's.
--
-- >>> CSAlphaNum <> CSDigit
-- CSAlphaNum
--
-- >>> CSAlphaNum <> CSChar '-'
-- CSUnion (fromList [CSAlphaNum,CSChar '-'])
--
instance Semigroup CharSet where
    x <> y = simplify1 $ Set.union (fromUnion x) (fromUnion y) where
        fromUnion (CSUnion s) = s
        fromUnion s           = Set.singleton s

        simplify1 s | Set.member CSAlphaNum s = simplify2 (removeChars isAlphaNum $ Set.filter (>= CSAlphaNum) s)
                    | otherwise               = simplify2 s

        simplify2 s | Set.member CSDigit s = simplify3 (removeChars isDigit s)
                    | otherwise            = simplify3 s

        removeChars p = Set.filter $ \z -> case z of
            CSChar c -> not (p c)
            _        -> True

        simplify3 s = case Set.toList s of
            [s'] -> s'
            _    -> CSUnion s

-------------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------------

-- |
--
-- >>> regexDoc $ REString "True"
-- True
--
-- >>> regexDoc $ REString "foo" <> REString "bar"
-- foobar
--
-- >>> regexDoc $ REUnion [REString "False" , REString "True"]
-- False|True
--
-- >>> regexDoc $ REMunch1 $ RECharSet $ CSAlphaNum <> CSChar '-'
-- [[:alnum:]-]+
--
-- >>> regexDoc $ REMunch1 $ REUnion [ RECharSet $ CSAlphaNum <> CSChar '-', REString "weird"]
-- ([[:alnum:]-]|weird)+
--
-- >>> regexDoc $ RENamed "something"
-- {something}
--
regexDoc :: Regex -> PP.Doc
regexDoc = go 0 where
    go :: Int -> Regex -> PP.Doc
    go d (REAppend rs)  = parensIf (d > 1) $ PP.hcat (map (go 1) rs)
    go d (REUnion rs)   = parensIf (d > 2) $ PP.hcat (PP.punctuate (PP.char '|') (map (go 2) rs))
    go _ (REMunch r)    = go 3 r <<>> PP.char '*'
    go _ (REMunch1 r)   = go 3 r <<>> PP.char '+'
    go _ (REOpt r)      = go 3 r <<>> PP.char '?'
    go d (REString s)   = parensIf (d > 2) $ PP.text s
    go _ (RECharSet cs) = charsetDoc cs
    go _ (REPunct c)    = PP.char c
    go _ RESpaces       = PP.space
    go _ (RENamed n _)  = PP.braces (PP.text n)
    go _ (REVar n)      = absurd n
    go _ (REApp n _ s)  = PP.braces (PP.text n <<>> PP.text " $ " <<>> go 0 s)

    parensIf :: Bool -> PP.Doc -> PP.Doc
    parensIf True  = PP.parens
    parensIf False = id

-- |
--
-- >>> traverse_ (print . charsetDoc) [CSDigit, CSAlphaNum, CSNotSpaceOrComma, CSNotSpace, CSChar 'a']
-- [:digit:]
-- [:alnum:]
-- [^ ,]
-- [^ ]
-- a
--
-- >>> print $ charsetDoc $ CSAlphaNum <> CSChar '-'
-- [[:alnum:]-]
--
charsetDoc :: CharSet -> PP.Doc
charsetDoc CSDigit           = PP.text "[:digit:]"
charsetDoc CSUpper           = PP.text "[:upper:]"
charsetDoc CSAlphaNum        = PP.text "[:alnum:]"
charsetDoc CSNotSpaceOrComma = PP.text "[^ ,]"
charsetDoc CSNotSpace        = PP.text "[^ ]"
charsetDoc (CSChar c)        = PP.char c
charsetDoc (CSUnion cs)      = PP.brackets $ PP.hcat (map charsetDoc $ Set.toList cs)

-------------------------------------------------------------------------------
-- Matching
-------------------------------------------------------------------------------

regexMatch :: Regex -> String -> Bool
regexMatch _ _ = False
