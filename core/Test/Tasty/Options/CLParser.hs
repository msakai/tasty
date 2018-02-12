-- | We define 'CLParser' in its own module instead of "Test.Tasty.Options" because:
--
-- 1. We want to keep it abstract, so that we can change it without causing
-- breakage;
--
-- 2. We need to expose it to other internal modules, namely "Test.Tasty.CmdLine"
--
-- 3. But "Test.Tasty.Options" is itself an exposed module, so if we export
-- it from there in a non-abstract way, it is no longer abstract for the
-- users.
module Test.Tasty.Options.CLParser where

import Control.Monad (mplus)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup)
import qualified Data.Semigroup (Semigroup((<>)))
#endif

-- | @'CLOption' v@ describes how the option @v@ is given on the command
-- line.
--
-- There are currently two ways to give an option on the command line: as
-- an on/off switch ('CLFlag'), appropriate for boolean options, or as an
-- option with an argument ('CLOption').
--
-- For instance, we use 'CLFlag' for the 'Quiet' option like this:
--
-- >newtype Quiet = Quiet Bool
-- >instance IsOption Quiet where
-- >  defaultValue = Quiet False
-- >  parseValue = fmap Quiet . safeRead
-- >  optionName = return "quiet"
-- >  optionHelp = return "Do not produce any output; indicate success only by the exit code"
-- >  optionCLParser = CLFlag ['q'] (Quiet True)
--
-- Using 'CLFlag' instead of 'CLOption' here allows the user to say
-- @--quiet@ instead of @--quiet=True@, and @['q']@ allows to say
-- @-q@ instead of @--quiet@.
--
-- The default (\"off\") value of the switch is specified in the definition
-- of 'defaultValue', and the alternative (\"on\") value is given in
-- 'optionCLParser'.
--
-- On the other hand, for 'Timeout' we use 'CLOption':
--
-- >data Timeout = ...
-- >instance IsOption Timeout where
-- >  ...
-- >  optionName = return "timeout"
-- >  optionHelp = return "Timeout for individual tests (suffixes: ms,s,m,h; default: s)"
-- >  optionCLParser = CLOption ['t'] "DURATION"
--
-- If we did not define 'optionCLParser' explicitly, the @--timeout@ option
-- would work in the same way, except there would be no short option @-t@,
-- and in the help message a generic metavariable @ARG@ would be used
-- instead of the more descriptive @DURATION@.
data CLParser v = CLParser
  { clShort :: [Char]
    -- ^ short options
  , clOnValue :: Maybe v
    -- ^ default value when the option is specified
  , clMetavar :: Maybe String
    -- ^ metavariable for the option to be displayed in the help message
    -- (such as @NUMBER@, @FILE@ etc.)
  }

instance Monoid (CLParser v) where
  mempty = CLParser [] Nothing Nothing
  mappend cl1 cl2 = CLParser
    { clShort = clShort cl1 ++ clShort cl2
    , clOnValue = clOnValue cl2 `mplus` clOnValue cl1 -- right-biased
    , clMetavar = clMetavar cl2 `mplus` clMetavar cl1 -- right-biased
    }
#if MIN_VERSION_base(4,9,0)
instance Semigroup (CLParser v) where
  (<>) = mappend
#endif

short :: Char -> CLParser v
short c = mempty { clShort = [c] }

onValue :: v -> CLParser v
onValue v = mempty { clOnValue = Just v }

metavar :: String -> CLParser v
metavar s = mempty { clMetavar = Just s }

