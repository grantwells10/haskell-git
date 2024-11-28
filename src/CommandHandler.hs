module CommandHandler (commandHandler) where

import CommandParser (Command (..), CommandError (..), ParsedCommand (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Data.Text (pack)
import Data.ByteString qualified as BS
import Hash (stringToBS)


commandHandler :: ParsedCommand -> Either CommandError (IO ())
commandHandler parsedCmd =
  let cmdStr = subcommand.parsedSubcommand $ parsedCmd
      flags = parsedFlags parsedCmd
      args = parsedArguments parsedCmd
  in case cmdStr of
    "init" -> handleInit flags args
    "commit" -> handleCommit flags args
    "merge" -> handleMerge flags args
    _ -> Left $ CommandError $ "Unknown subcommand: " ++ cmdStr

-- Initialize .hgit directory
handleInit :: [(String, Maybe String)] -> [String] -> Either CommandError (IO ())
-- No flags, no args
handleInit [] [] = Right $ do
    createDirectoryIfMissing True ".hgit" 
    createDirectoryIfMissing True ".hgit/refs"
    createDirectoryIfMissing True ".hgit/objects"
    -- Write as Byte String
    BS.writeFile (".hgit" </> "HEAD") (stringToBS "ref: refs/heads/main\n")
    putStrLn "Initialized .hgit directory."
handleInit _ _ = Left $ CommandError "Unexpected arguments for 'init'. (There should be none.)"

handleCommit :: [(String, Maybe String)] -> [String] -> Either CommandError (IO ())
handleCommit flags args =
  case lookup "--message" flags of
    Just (Just message) -> Right $ putStrLn $ "Committing with message: " ++ message
    _ -> Left $ CommandError "The '--message' flag is required for 'commit'."

handleMerge :: [(String, Maybe String)] -> [String] -> Either CommandError (IO ())
handleMerge flags args =
  case args of
    (branch : _) -> Right $ putStrLn $ "Merging branch: " ++ branch
    [] -> Left $ CommandError "A branch name is required for 'merge'."
