{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable,
             ExistentialQuantification, GADTs,
             FlexibleInstances, UndecidableInstances,
             TypeOperators #-}
-- | Extensible options. They are used for provider-specific settings,
-- ingredient-specific settings and core settings (such as the test name pattern).
module Test.Tasty.Options
  (
    -- * IsOption class
    IsOption(..)
  , CLParser
  , short
  , onValue
  , metavar
    -- * Option sets and operations
  , OptionSet
  , setOption
  , changeOption
  , lookupOption
  , singleOption
  , OptionDescription(..)
    -- * Utilities
  , safeRead
  ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Tagged
import Data.Proxy
import Data.Typeable
import Data.Monoid
import Prelude hiding (mod) -- Silence FTP import warnings
import Test.Tasty.Options.CLParser
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup)
import qualified Data.Semigroup (Semigroup((<>)))
#endif

-- | An option is a data type that inhabits the `IsOption` type class.
class Typeable v => IsOption v where
  -- | The value to use if the option was not supplied explicitly
  defaultValue :: v
  -- | Try to parse an option value from a string
  parseValue :: String -> Maybe v
  -- | The option name. It is used to form the command line option name, for
  -- instance. Therefore, it had better not contain spaces or other fancy
  -- characters. It is recommended to use dashes instead of spaces.
  optionName :: Tagged v String
  -- | The option description or help string. This can be an arbitrary
  -- string.
  optionHelp :: Tagged v String

  -- | How this option is given on the command line (see 'CLParser').
  --
  -- This method has a sensible default implementation based on the other
  -- methods.
  optionCLParser :: CLParser v
  optionCLParser = mempty

data OptionValue = forall v . IsOption v => OptionValue v

-- | A set of options. Only one option of each type can be kept.
--
-- If some option has not been explicitly set, the default value is used.
newtype OptionSet = OptionSet (Map TypeRep OptionValue)

-- | Later options override earlier ones
instance Monoid OptionSet where
  mempty = OptionSet mempty
  OptionSet a `mappend` OptionSet b =
    OptionSet $ Map.unionWith (flip const) a b
#if MIN_VERSION_base(4,9,0)
instance Semigroup OptionSet where
  (<>) = mappend
#endif

-- | Set the option value
setOption :: IsOption v => v -> OptionSet -> OptionSet
setOption v (OptionSet s) =
  OptionSet $ Map.insert (typeOf v) (OptionValue v) s

-- | Query the option value
lookupOption :: forall v . IsOption v => OptionSet -> v
lookupOption (OptionSet s) =
  case Map.lookup (typeOf (undefined :: v)) s of
    Just (OptionValue x) | Just v <- cast x -> v
    Just {} -> error "OptionSet: broken invariant (shouldn't happen)"
    Nothing -> defaultValue

-- | Change the option value
changeOption :: forall v . IsOption v => (v -> v) -> OptionSet -> OptionSet
changeOption f s = setOption (f $ lookupOption s) s

-- | Create a singleton 'OptionSet'
singleOption :: IsOption v => v -> OptionSet
singleOption v = setOption v mempty

-- | The purpose of this data type is to capture the dictionary
-- corresponding to a particular option.
data OptionDescription where
  Option :: IsOption v => Proxy v -> OptionDescription

-- | Safe read function. Defined here for convenience to use for
-- 'parseValue'.
safeRead :: Read a => String -> Maybe a
safeRead s
  | [(x, "")] <- reads s = Just x
  | otherwise = Nothing
