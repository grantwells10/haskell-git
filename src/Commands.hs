{-|
This module contains the definitions for all available HGit commands,
including their subcommands, descriptions, flags, and arguments.
Each command is defined as a Command data type that specifies its
configuration and usage.
-}

module Commands (initCmd) where

import CommandParser (Command (..), CommandError (..), parseInput)

initCmd :: Command
initCmd = Command {
    subcommand = "init",
    description = "Initialize a new HGit repository",
    flags = [],
    args = []
}

-- TODO: Add more commands here