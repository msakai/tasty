-- | Parsing options supplied on the command line
{-# LANGUAGE CPP, ScopedTypeVariables, DeriveDataTypeable, RecordWildCards #-}
module Test.Tasty.CmdLine
  ( optionParser
  , suiteOptions
  , suiteOptionParser
  , defaultMainWithIngredients
  ) where

import Data.Monoid
import Data.Proxy
import Data.Tagged
import Data.Either (lefts, rights)
import Data.Maybe
import Data.List
import Prelude  -- Silence AMP and FTP import warnings
import System.Exit
import System.IO
import System.Environment (getArgs)
import qualified System.Console.GetOpt as GetOpt
import Text.Printf
import Control.Monad (when)

-- We install handlers only on UNIX (obviously) and on GHC >= 7.6.
-- GHC 7.4 lacks mkWeakThreadId (see #181), and this is not important
-- enough to look for an alternative implementation, so we just disable it
-- there.
#define INSTALL_HANDLERS defined UNIX && MIN_VERSION_base(4,6,0)

#if INSTALL_HANDLERS
import Control.Concurrent (mkWeakThreadId, myThreadId)
import Control.Exception (Exception(..), throwTo)
import Control.Monad (forM_)
import Data.Typeable (Typeable)
import System.Posix.Signals
import System.Mem.Weak (deRefWeak)
#endif

import Test.Tasty.Core
import Test.Tasty.Ingredients
import Test.Tasty.Options
import Test.Tasty.Options.CLParser
import Test.Tasty.Options.Core
import Test.Tasty.Options.Env

-- | Generate a command line parser from a list of option descriptions
optionParser :: [OptionDescription] -> [GetOpt.OptDescr (Either String OptionSet)]
optionParser = map $ \(Option (Proxy :: Proxy v)) ->
  let
    Tagged name = optionName :: Tagged v String
    Tagged help = optionHelp :: Tagged v String
    CLParser{..} = optionCLParser :: CLParser v
    metavar_ = fromMaybe "ARG" clMetavar
    parseOpt :: String -> Either String OptionSet
    parseOpt str =
      case parseValue str of
        Just (val :: v) -> Right $ singleOption val
        Nothing -> Left $ printf "Invalid value for option %s: %s"
          name (show str)
    argDescr :: GetOpt.ArgDescr (Either String OptionSet)
    argDescr =
      case clOnValue of
        Nothing -> GetOpt.ReqArg parseOpt metavar_
        Just val ->
          GetOpt.OptArg
            (maybe (Right $ singleOption val) parseOpt)
            metavar_
  in
    GetOpt.Option
      clShort
      [name]
      argDescr
      help

-- | The command line parser for the test suite
suiteOptionParser
  :: [Ingredient]
  -> TestTree ->
  [GetOpt.OptDescr (Either String OptionSet)]
-- We don't use this function ourselves, but export it so that others could
-- if they wanted.
suiteOptionParser ins tree = optionParser $ suiteOptions ins tree

-- | Parse the command line arguments and run the tests using the provided
-- ingredient list.
--
-- When the tests finish, this function calls 'exitWith' with the exit code
-- that indicates whether any tests have failed. See 'defaultMain' for
-- details.
defaultMainWithIngredients :: [Ingredient] -> TestTree -> IO ()
defaultMainWithIngredients ins testTree = do
  installSignalHandlers
  args <- getArgs
  let
    opt_descrs :: [OptionDescription]
    opt_descrs = suiteOptions ins testTree

    getopt_descrs :: [GetOpt.OptDescr (Either String OptionSet)]
    getopt_descrs = optionParser opt_descrs

    (opts_or_errs, _, errs0) = GetOpt.getOpt GetOpt.RequireOrder getopt_descrs args
    errs1 = errs0 ++ lefts opts_or_errs
    usage = "Mmm... tasty test suite\n\nAvailable options:\n" ++ usageInfo opt_descrs
  cmdlineOpts <-
    if null errs1
      then return . mconcat . rights $ opts_or_errs
      else do
        mapM_ (hPutStrLn stderr) errs1
        hPutStrLn stderr "" -- add an extra empty line after the errors
        hPutStr stderr usage
        exitFailure

  envOpts <- suiteEnvOptions ins testTree

  let opts = envOpts <> cmdlineOpts
      Help help = lookupOption opts

  when help $ do
    putStr usage
    exitSuccess

  case tryIngredients ins opts testTree of
    Nothing -> do
      hPutStrLn stderr
        "No ingredients agreed to run. Something is wrong either with your ingredient set or the options."
      exitFailure
    Just act -> do
      ok <- act
      if ok then exitSuccess else exitFailure

-- | An alternative to GetOpt's help formatting.
--
-- Here we can take advantage of some of the specifics of tasty
-- option parsing. We don't print the allowed but rarely used optional
-- arguments to on/off switches, e.g. we print @-h@ although @-h[=BOOL]@ is
-- theoretically allowed.
--
-- The formatting is closer to optparse-applicative's one,
-- with the suggested improvement by Simon Jakobi (see #121)
usageInfo :: [OptionDescription] -> String
usageInfo = format_all . map option_doc
  where
    opt_indent = 2
    total_width = 80

    format_all :: [(String, String)] -> String
    format_all [] = ""
    format_all docs =
      let
        left_col_width = (+1) . maximum . map (length . fst) $ docs
        right_col_width = total_width - opt_indent - left_col_width
      in unlines . map (indent opt_indent) . flip concatMap docs $ \(opt_line, help) ->
        case wrap right_col_width help of
          [] -> [opt_line]
          first : rest ->
            (opt_line
              ++ replicate (left_col_width - length opt_line) ' '
              ++ first)
              : map (indent left_col_width) rest

    -- return the option line and the help line
    option_doc :: OptionDescription -> (String, String)
    option_doc (Option (Proxy :: Proxy v)) =
      let
        Tagged name = optionName :: Tagged v String
        Tagged help = optionHelp :: Tagged v String
        CLParser{..} = optionCLParser :: CLParser v
        flags = intercalate "," $ map (\c -> '-' : c : "") clShort ++ ["--" ++ name]
        args =
          if isJust clOnValue
            then []
            else [fromMaybe "ARG" clMetavar]
        opt_line = intercalate " " (flags : args)
      in
        (opt_line, help)

-- | Indent a string
indent :: Int -> String -> String
indent n = (replicate n ' ' ++)

-- | Wrap a string
wrap :: Int -> String -> [String]
wrap width s0 = foldr step final (words s0) (mempty, 0)
  where
    final :: (Endo [String], Int) -> [String]
    final (acc, acc_len) =
      if acc_len == 0 then [] else [unwords $ appEndo acc []]
    step
      :: String
      -> ((Endo [String], Int) -> [String])
      -> (Endo [String], Int)
      -> [String]
    step word k (acc, acc_len) =
      let
        acc_len' = acc_len + length word +
          {- space -}
          (if acc_len == 0 then 0 else 1)
      in
      if acc_len' <= width
        then k (acc <> Endo (word:), acc_len')
        else
          unwords (appEndo acc []) :
          if length word > width
            then word : k (mempty, 0)
            else k (Endo (word:), length word)

-- from https://ro-che.info/articles/2014-07-30-bracket
-- Install a signal handler so that e.g. the cursor is restored if the test
-- suite is killed by SIGTERM.
installSignalHandlers :: IO ()
installSignalHandlers = do
#if INSTALL_HANDLERS
  main_thread_id <- myThreadId
  weak_tid <- mkWeakThreadId main_thread_id
  forM_ [ sigABRT, sigBUS, sigFPE, sigHUP, sigILL, sigQUIT, sigSEGV,
          sigSYS, sigTERM, sigUSR1, sigUSR2, sigXCPU, sigXFSZ ] $ \sig ->
    installHandler sig (Catch $ send_exception weak_tid sig) Nothing
  where
    send_exception weak_tid sig = do
      m <- deRefWeak weak_tid
      case m of
        Nothing  -> return ()
        Just tid -> throwTo tid (toException $ SignalException sig)

newtype SignalException = SignalException Signal
  deriving (Show, Typeable)
instance Exception SignalException
#else
  return ()
#endif
