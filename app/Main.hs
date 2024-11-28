-- \* Entry point for a Haskell application
-- Typically, this file is short and most code is part of a reusable library

module Main where

import CommandHandler (commandHandler)
import CommandParser (Command (..), CommandError (..), parseInput)
import Control.Monad (unless)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (except)
import Commands (initCmd)

commands :: [Command]
commands = [initCmd] -- add more commands here

main :: IO ()
main = do
  input <- getLine
  result <- runExceptT $ do
    -- Parse the input
    cmd <- except $ parseInput commands $ words input
    -- Handle the command
    result <- except $ commandHandler cmd
    -- Execute the command
    liftIO result
  -- Handle errors
  case result of
    Left (CommandError errMsg) -> putStrLn $ "[ERROR] " ++ errMsg
    Right () -> return ()
