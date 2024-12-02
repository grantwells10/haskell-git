module Commands
  ( commands,
    commandInit,
    commandAdd,
    commandCommit,
    commandBranch
    -- Add more commands here as needed
  )
where

import CommandParser (Command(..), Flag(..), FlagType(..), defaultValidate, CommandError(..))
import Data.Text qualified (pack)

-- | All commands
commands :: [Command]
commands = 
  [ commandInit
  , commandAdd
  , commandCommit
  , commandBranch
  ]

-- | Init Command
commandInit :: Command
commandInit = Command
  { subcommand = "init",
    description =
      "Creates the .hgit directory with necessary subdirectories and files, including an empty HEAD file, an empty objects directory, and refs/heads/ with an empty main file.",
    flags = [],
    validate = defaultValidate
  }

-- | Add Command
commandAdd :: Command
commandAdd = Command
  { subcommand = "add",
    description =
      "Adds file(s) to the index. Supports adding individual files, updating tracked files, or adding all files in the current directory and subdirectories.",
    flags =
      [ Flag { longName = "update", shortName = Just "u", flagType = NoArg }
      ],
    validate = validateAddCommand
  }

-- | Commit Command
commandCommit :: Command
commandCommit = Command
  { subcommand = "commit",
    description =
      "Creates a new commit from the current index with a commit message. Supports committing with a message, amending the last commit, or using the previous commit message.",
    flags =
      [ Flag { longName = "message", shortName = Just "m", flagType = RequiresArg }
      ],
    validate = validateCommitCommand
  }

-- | Branch Command
commandBranch :: Command
commandBranch = Command
  { subcommand = "branch",
    description =
      "List, create, or delete branches.\n\
      \Usage:\n\
      \  hgit branch                List all branches\n\
      \  hgit branch <branchname>   Create a new branch\n\
      \  hgit branch -d <branchname> Delete an existing branch",
    flags =
      [ Flag { longName = "delete", shortName = Just "d", flagType = RequiresArg }
      ],
    validate = validateBranchCommand
  }

-- Add more command definitions here as needed

-- Validation functions

validateAddCommand :: [(String, Maybe String)] -> [String] -> Either CommandError ()
validateAddCommand flags args =
  case (flags, args) of
    ([("update", Nothing)], []) -> Right ()
    ([], ["."]) -> Right ()
    ([], _ : _) -> Right ()
    _ -> Left $ CommandError "Invalid usage of 'hgit add'. Use 'hgit add -u', 'hgit add <file>... ', or 'hgit add .'"

validateCommitCommand :: [(String, Maybe String)] -> [String] -> Either CommandError ()
validateCommitCommand flags args =
  case (flags, args) of
    ([("message", Just msg)], []) | not (null msg) -> Right ()
    _ -> Left $ CommandError "Invalid usage of 'hgit commit'. Use 'hgit commit -m \"msg\"'.'"

validateBranchCommand :: [(String, Maybe String)] -> [String] -> Either CommandError ()
validateBranchCommand flags args =
  case flags of
    [("delete", Just branchName)] -> Right () -- Deleting a branch
    [] ->
      case args of
        [] -> Right () -- Listing branches
        [branchName] -> Right () -- Creating a branch
        _ -> Left $ CommandError "Invalid usage of 'hgit branch'. Use 'hgit branch', 'hgit branch <branchname>', or 'hgit branch -d <branchname>'."
    _ -> Left $ CommandError "Invalid flags for 'hgit branch'. Use '-d <branchname>' to delete a branch."