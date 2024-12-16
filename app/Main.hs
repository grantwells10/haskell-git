module Main where

import CommandHandler (commandHandler)
import Command (Command (..), CommandError (..))
import CommandParser (parseInput)
import Control.Monad (unless)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (except)
import System.Environment (getArgs)

main :: IO ()
main = do
  input <- getArgs
  result <- runExceptT $ do
    -- Parse the input
    cmd <- except $ parseInput input
    -- Handle the command
    cmdResult <- liftIO $ commandHandler cmd
    output <- except cmdResult
    -- Print the output if it is not empty
    unless (null output) $ liftIO $ putStrLn output
  -- Handle errors
  case result of
    Left (CommandError errMsg) -> putStrLn $ "[ERROR] " ++ errMsg
    Right () -> return ()
