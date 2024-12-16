-- | Command module
-- This module defines the supported commands and flags for the hgit system.
-- It also includes validation functions for each command.

module Command (
  Command(..),
  Flag(..),
  CommandError(..),
  description,
  flags,
  validate,
  commandToString,
  allFlags
) where

import Control.Exception (Exception)

-- Data type for each supported command
data Command = Init
  | Commit
  | Add
  | Status
  | Log
  | Switch
  | Branch
  deriving (Show, Eq)

-- Data type for flag
data Flag = Flag
  { longName :: String,
    shortName :: Maybe String,
    requiresArg :: Bool
  }
  deriving (Show, Eq, Ord)

-- Custom type for command errors
newtype CommandError = CommandError String
  deriving (Show, Eq)

-- Exception instance for CommandError
instance Exception CommandError

-- Description of each command
description :: Command -> String
description Init = "Creates the .hgit directory..."
description Commit = "Creates a new commit from the current index..."
description Add = "Adds file(s) to the index."
description Status = "Show the working tree status."
description Log = "Displays commit logs..."
description Switch = "Switches to an existing branch. Usage: 'hgit switch <branchname>'"
description Branch = "List, create, or delete branches..."

-- Flags for each command
flags :: Command -> [Flag]
flags Init = []
flags Commit = [Flag {longName = "message", shortName = Just "m", requiresArg = True}]
flags Add = [Flag {longName = "update", shortName = Just "u", requiresArg = False}]
flags Status = []
flags Log = []
flags Switch = []
flags Branch = [Flag {longName = "delete", shortName = Just "d", requiresArg = True}]

-- Validation function for each command
validate :: Command -> [(String, Maybe String)] -> [String] -> Either CommandError ()
validate command flags args = case command of
  Init -> defaultValidate flags args
  Add -> validateAddCommand flags args
  Commit -> validateCommitCommand flags args
  Branch -> validateBranchCommand flags args
  Switch -> validateSwitchCommand flags args
  Log -> defaultValidate flags args
  Status -> defaultValidate flags args

-- | Default validation function for commands that don't have any flags or arguments
defaultValidate :: [(String, Maybe String)] -> [String] -> Either CommandError ()
defaultValidate [] [] = Right ()
defaultValidate _ _ = Left $ CommandError "This command does not accept any flags or arguments."

-- | Validate the add command
validateAddCommand :: [(String, Maybe String)] -> [String] -> Either CommandError ()
validateAddCommand flags args =
  case (flags, args) of
    ([("update", Nothing)], []) -> Right ()
    ([], ["."]) -> Right ()
    ([], _ : _) -> Right ()
    _ -> Left $ CommandError "Invalid usage of 'hgit add'. Use 'hgit add -u', 'hgit add <file>... ', or 'hgit add .'"

-- | Validate the commit command
validateCommitCommand :: [(String, Maybe String)] -> [String] -> Either CommandError ()
validateCommitCommand flags args =
  case (flags, args) of
    ([("message", Just msg)], []) | not (null msg) -> Right ()
    _ -> Left $ CommandError "Invalid usage of 'hgit commit'. Use 'hgit commit -m=\"msg\"'.'"

-- | Validate the branch command
validateBranchCommand :: [(String, Maybe String)] -> [String] -> Either CommandError ()
validateBranchCommand flags args =
  case flags of
    [("delete", Just _)] -> Right () -- Deleting a branch
    [] ->
      case args of
        [] -> Right () -- Listing branches
        [_] -> Right () -- Creating a branch
        _ -> Left $ CommandError "Invalid usage of 'hgit branch'."
    _ -> Left $ CommandError "Invalid flags for 'hgit branch'."

-- | Validate the switch command
validateSwitchCommand :: [(String, Maybe String)] -> [String] -> Either CommandError ()
validateSwitchCommand _ args =
  case args of
    [_] -> Right ()
    _ -> Left $ CommandError "Invalid usage of 'hgit switch'."

-- | Convert a command to its string representation
commandToString :: Command -> Either CommandError String
commandToString Init = Right "init"
commandToString Add = Right "add"
commandToString Commit = Right "commit"
commandToString Status = Right "status"
commandToString Log = Right "log"
commandToString Switch = Right "switch"
commandToString Branch = Right "branch"

-- | All flags for all commands
allFlags :: [Flag]
allFlags = concatMap flags [Init, Commit, Add, Status, Log, Switch, Branch]

{-

FORMER COMMAND STRUCTURE (Q2)

Command
  { subcommand :: String,
    description :: String,
    flags :: [Flag],
    validate :: [(String, Maybe String)] -> [String] -> Either CommandError ()
  }

commands :: [Command]
commands =
  [ Command
      { subcommand = "init",
        description =
          "Creates the .hgit directory...",
        flags = [],
        validate = defaultValidate
      },
    Command
      { subcommand = "add",
        description = "Adds file(s) to the index.",
        flags =
          [ Flag {longName = "update", shortName = Just "u", flagType = NoArg}
          ],
        validate = validateAddCommand
      },
    Command
      { subcommand = "commit",
        description = "Creates a new commit from the current index...",
        flags =
          [Flag {longName = "message", shortName = Just "m", flagType = RequiresArg}],
        validate = validateCommitCommand
      },
    Command
      { subcommand = "branch",
        description =
          "List, create, or delete branches...",
        flags =
          [ Flag { longName = "delete", shortName = Just "d", flagType = RequiresArg }
          ],
        validate = validateBranchCommand
      },
    Command
      { subcommand = "switch",
        description =
          "Switches to an existing branch. Usage: 'hgit switch <branchname>'",
        flags = [],
        validate = validateSwitchCommand
      },
    Command
      { subcommand = "log",
        description = "Displays commit logs...",
        flags = [],
        validate = defaultValidate
      },
          Command
      { subcommand = "status",
        description = "Show the working tree status.",
        flags = [],
        validate = defaultValidate
      }
  ]
-}