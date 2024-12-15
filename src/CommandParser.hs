-- | CommandParser module
-- This module defines the command parser for the hgit system.
-- It parses the input command and flags, and validates them.
-- It also defines the data type for the parsed command.

module CommandParser (
  parseInput,
  RawCommand (..),
  ParsedCommand (..),
  parseRawCommand,
  handleRawCommand,
  parseFlagsAndArgs,
  rawCommandToTokens
) where

import Control.Exception ( Exception )
import Data.Char (isSpace)
import Data.List (find, isPrefixOf, isSuffixOf)
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
import Command (Command(..), Flag(..), CommandError(..), flags, validate, allFlags)

-- | Raw parsed command, flexible for testing
data RawCommand = RawCommand
  { rawName :: String,
    rawFlags :: [(String, Maybe String)],
    rawArguments :: [String]
  }
  deriving (Show, Eq)

-- | Rigorous parsed command corresponding to a valid HGit command
data ParsedCommand = ParsedCommand
  { cmd :: Command,
    parsedFlags :: [(String, Maybe String)],
    parsedArguments :: [String]
  }
  deriving (Show, Eq)

-- | Parse the input to find the command, flags, and arguments
parseInput :: [String] -> Either CommandError ParsedCommand
parseInput input = do
  rawCmd <- parseRawCommand input
  handleRawCommand rawCmd

-- | Parse the raw command from input
parseRawCommand :: [String] -> Either CommandError RawCommand
parseRawCommand [] = Left $ CommandError "No command provided."
parseRawCommand (cmd : rest) = do
  (parsedFlags, remaining) <- parseFlagsAndArgs rest
  return $ RawCommand cmd parsedFlags remaining

-- | Validate that raw command is valid, convert to a ParsedCommand
handleRawCommand :: RawCommand -> Either CommandError ParsedCommand
handleRawCommand rawCmd = do
  -- First, validate command name
  cmd <- case rawName rawCmd of
    "init" -> Right Init
    "commit" -> Right Commit
    "add" -> Right Add
    "status" -> Right Status
    "log" -> Right Log
    "switch" -> Right Switch
    "branch" -> Right Branch
    _ -> Left $ CommandError $ "Unknown command: " ++ rawName rawCmd
  -- Then, validate flag and 
  validate cmd (rawFlags rawCmd) (rawArguments rawCmd)
  -- If all is well, return the parsed command
  return $ ParsedCommand cmd (rawFlags rawCmd) (rawArguments rawCmd)

-- | Convert a RawCommand to its corresponding command line tokens 
rawCommandToTokens :: RawCommand -> [String]
rawCommandToTokens rawCmd = 
  let flagToTokens :: (String, Maybe String) -> [String]
      flagToTokens (name, maybeValue) =
        case maybeValue of
          Just value -> ["--" ++ name ++ "=", value]
          Nothing -> ["--" ++ name]
      flagTokens = concatMap flagToTokens (rawFlags rawCmd)
      argTokens = rawArguments rawCmd
      cmdName = rawName rawCmd
  in cmdName : flagTokens ++ argTokens

-- | Checks if a token is a flag (starts with -)
isFlag :: String -> Bool
isFlag ('-' : _) = True
isFlag _ = False

-- | Checks if a flag is an argument flag (starts with -- and has an = at the end)
isArgFlag :: String -> Bool
isArgFlag flag = "--" `isPrefixOf` flag && "=" `isSuffixOf` flag

-- | Extracts the flag name from a flag token (e.g. "--update=" -> "update")
extractFlagName :: String -> String
extractFlagName flag = 
  let stripped = dropWhile (== '-') flag in
  if "=" `isSuffixOf` stripped
    then takeWhile (/= '=') stripped
    else stripped

-- | Main flag parsing function stays the same but calls simplified functions
parseFlagsAndArgs :: [String] -> Either CommandError ([(String, Maybe String)], [String])
parseFlagsAndArgs tokens = parseTokens tokens [] [] False

-- | Recursive token parser that accumulates flags and arguments
parseTokens :: [String] -> [(String, Maybe String)] -> [String] -> Bool -> Either CommandError ([(String, Maybe String)], [String])
parseTokens [] parsedFlags args _ = 
    Right (reverse parsedFlags, reverse args)
parseTokens (x : xs) parsedFlags args seenArg
    | not seenArg && isFlag x = 
        handleFlag x xs parsedFlags args
    | otherwise = 
        parseTokens xs parsedFlags (x : args) True

-- | Processes a flag token, handling potential errors and flag values
handleFlag ::
  String ->      -- flag token (e.g. "--update" or "--message=")
  [String] ->    -- remaining tokens
  [(String, Maybe String)] ->  -- accumulated flags
  [String] ->    -- accumulated args
  Either CommandError ([(String, Maybe String)], [String])
handleFlag flagToken rest parsedFlags args = 
    let rawName = extractFlagName flagToken 
        mappedName = mapFlagName rawName allFlags in
    if isArgFlag flagToken
        then case rest of
            -- If it's a "--flag=" format, next token must be the value
            (value:remaining) -> 
                parseTokens remaining ((mappedName, Just value) : parsedFlags) args False
            [] -> Left $ CommandError $ "Flag " ++ rawName ++ " requires a value"
        else
            -- Regular flag without value
            parseTokens rest ((mappedName, Nothing) : parsedFlags) args False

-- | Map a flag token to its long name if it's a short flag, otherwise return the original token
mapFlagName :: String -> [Flag] -> String
mapFlagName token flags =
    let 
        strippedToken = dropWhile (== '-') token
        isShortFlag = "-" `isPrefixOf` token && not ("--" `isPrefixOf` token)
    in 
        -- If it's a short flag, try to find its matching long name
        case find (\f -> shortName f == Just strippedToken) flags of
            Just flag -> longName flag    -- Found matching flag, use its long name
            Nothing -> strippedToken      -- No match, just use original name