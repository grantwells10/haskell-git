-- | ParserTests module
-- This module contains tests for the Parser module.

module ParserTests
  ( parserTests,
    prop_parseInputRawCommand,
  ) where

import CommandParser 
  ( ParsedCommand (..),
    RawCommand (..),
    parseRawCommand,
    parseFlagsAndArgs,
    parseInput,
    handleRawCommand,
    rawCommandToTokens
  )

import Command (
  Command(..),
  Flag(..),
  CommandError(..),
  validate,
  flags,
  allFlags
  )

import Test.HUnit ( (~:), (~?=), Test(TestLabel, TestList) )
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Property,
    Testable,
    chooseInt,
    elements,
    forAll,
    listOf1,
    vectorOf,
    counterexample,
    suchThat,
    Gen
  )

import Data.Maybe (mapMaybe, maybeToList)

-- | Arbitrary instance for RawCommand to test Parser
instance Arbitrary RawCommand where
  arbitrary = do
    name <- listOf1 $ elements $ ['a'..'z']
    numFlags <- chooseInt (0, 3)
    flags <- generateUniqueFlags numFlags
    numArgs <- chooseInt (0, 3)
    args <- vectorOf numArgs $ listOf1 $ (elements $ ['a'..'z'] ++ ['0'..'9'])
    return $ RawCommand name flags args

-- | Generate n unique flag strings
generateUniqueFlags :: Int -> Gen [(String, Maybe String)]
generateUniqueFlags n = go n []
  where
    go 0 _ = return []
    go count usedLongNames = do
      -- Do not allow short flags that are real to avoid accidental conversion
      long <- listOf1 (elements ['a' .. 'z']) `suchThat` \name -> 
               not (any (\f -> shortName f == Just name) allFlags)
      if long `elem` usedLongNames
        then go count usedLongNames
        else do
          hasValue <- arbitrary  -- randomly pick if flag needs value
          value <- if hasValue 
                  then Just <$> listOf1 (elements ['a'..'z'])
                  else return Nothing
          let newUsedLongNames = long : usedLongNames
          rest <- go (count - 1) newUsedLongNames
          return $ (long, value) : rest

-- | Unit tests for rawCommandToTokens
testRawCommandToTokens :: Test
testRawCommandToTokens = TestList
  [ "Test RawCommand to tokens" ~:
      rawCommandToTokens (RawCommand "add" [("update", Nothing)] ["file1.txt"])
      ~?= ["add", "--update", "file1.txt"]
  , "Test RawCommand to tokens with multiple flags" ~:
      rawCommandToTokens (RawCommand "add" [("update", Nothing), ("verbose", Just "true")] ["file1.txt"])
      ~?= ["add", "--update", "--verbose=", "true", "file1.txt"],
    "Test RawCommand to tokens with multiple args" ~:
      rawCommandToTokens (RawCommand "add" [("update", Nothing)] ["file1.txt", "file2.txt"])
      ~?= ["add", "--update", "file1.txt", "file2.txt"],
    "Test RawCommand to tokens with no flags and no args" ~:
      rawCommandToTokens (RawCommand "add" [] [])
      ~?= ["add"],
    "Test RawCommand to tokens with no flags and multiple args" ~:
      rawCommandToTokens (RawCommand "add" [] ["file1.txt", "file2.txt"])
      ~?= ["add", "file1.txt", "file2.txt"]
  ]

-- | Property-based testing for RawCommand, round trip
prop_parseInputRawCommand :: Property
prop_parseInputRawCommand= forAll arbitrary $ \rawCmd ->
  let 
    clTokens = rawCommandToTokens rawCmd
    debug = "Original RawCommand: " ++ show rawCmd ++ "\n" ++
                "Tokens: " ++ show clTokens
  in case parseRawCommand clTokens of
    Right parsedRawCmd ->
      counterexample
                (debug ++ "\nParsed back to: " ++ show parsedRawCmd) $
      rawName parsedRawCmd == rawName rawCmd &&
      rawFlags parsedRawCmd == rawFlags rawCmd &&
      rawArguments parsedRawCmd == rawArguments rawCmd
    Left err ->
      counterexample 
                (debug ++ "\nFailed to parse with error: " ++ show err) False

-- | Test RawCommand -> ParsedCommand conversion
testValidateRawCommand :: Test
testValidateRawCommand = TestList
  [ "Validate init command" ~: 
        handleRawCommand (RawCommand "init" [] [])
        ~?= Right (ParsedCommand Init [] []),
     "Validate add with update" ~:
        handleRawCommand (RawCommand "add" [("update", Nothing)] [])
        ~?= Right (ParsedCommand Add [("update", Nothing)] []),
    "Validate add with files" ~:
        handleRawCommand (RawCommand "add" [] ["file1.txt"])
        ~?= Right (ParsedCommand Add [] ["file1.txt"]),
    "Reject invalid add command" ~:
        handleRawCommand (RawCommand "add" [("update", Nothing)] ["file1.txt"])
        ~?= Left (CommandError "Invalid usage of 'hgit add'. Use 'hgit add -u', 'hgit add <file>... ', or 'hgit add .'"),
    "Validate commit with message" ~:
        handleRawCommand (RawCommand "commit" [("message", Just "test")] [])
        ~?= Right (ParsedCommand Commit [("message", Just "test")] []),
    "Validate branch creation" ~:
        handleRawCommand (RawCommand "branch" [] ["new-branch"])
        ~?= Right (ParsedCommand Branch [] ["new-branch"]),
    "Validate branch deletion" ~:
        handleRawCommand (RawCommand "branch" [("delete", Just "old-branch")] [])
        ~?= Right (ParsedCommand Branch [("delete", Just "old-branch")] []),
    "Validate switch" ~:
        handleRawCommand (RawCommand "switch" [] ["branch-name"])
        ~?= Right (ParsedCommand Switch [] ["branch-name"]),
    "Reject invalid command" ~:
        handleRawCommand (RawCommand "invalidcmd" [] [])
        ~?= Left (CommandError "Unknown command: invalidcmd")
    ]

-- | Test Flags and Args parsing
testParseFlagsAndArgs :: Test
testParseFlagsAndArgs =
  TestList
    [ "Parse no-arg flag (--update)"
        ~: parseFlagsAndArgs ["--update"]
        ~?= Right ([("update", Nothing)], []),
      "Parse short flag (-u)"
        ~: parseFlagsAndArgs ["-u"]
        ~?= Right ([("update", Nothing)], []),
      "Parse arguments"
        ~: parseFlagsAndArgs ["file1.txt", "file2.txt"]
        ~?= Right ([], ["file1.txt", "file2.txt"]),
      "Parse flags and arguments"
        ~: parseFlagsAndArgs ["--update", "file1.txt"]
        ~?= Right ([("update", Nothing)], ["file1.txt"]),
      "Parse flags with required and optional flag args and normal args"
        ~: parseFlagsAndArgs ["--required=", "test", "--optional", "file1.txt"]
        ~?= Right ([("required", Just "test"), ("optional", Nothing)], ["file1.txt"])
    ]

-- | Full integration test for parseInput
testParseInput :: Test
testParseInput =
  TestList
    [ "Parse 'add' with no flags or args"
        ~: parseInput ["add"]
        ~?= Left (CommandError "Invalid usage of 'hgit add'. Use 'hgit add -u', 'hgit add <file>... ', or 'hgit add .'"),
      "Parse 'add' with --update flag"
        ~: parseInput ["add", "--update"]
        ~?= Right
          ( ParsedCommand
              { cmd = Command.Add,
                parsedFlags = [("update", Nothing)],
                parsedArguments = []
              }
          ),
      "Parse 'add' with files"
        ~: parseInput ["add", "file1.txt", "file2.txt"]
        ~?= Right
          ( ParsedCommand
              { cmd = Command.Add,
                parsedFlags = [],
                parsedArguments = ["file1.txt", "file2.txt"]
              }
          ),
      "Parse 'add' with flags and files"
        ~: parseInput ["add", "-u", "file1.txt"]
        ~?= Left (CommandError "Invalid usage of 'hgit add'. Use 'hgit add -u', 'hgit add <file>... ', or 'hgit add .'"),
      "Parse 'init' with no flags or args"
        ~: parseInput ["init"]
        ~?= Right
          ( ParsedCommand
              { cmd = Command.Init,
                parsedFlags = [],
                parsedArguments = []
              }
          ),
      "Parse 'init' with unexpected args"
        ~: parseInput ["init", "extra"]
        ~?= Left (CommandError "This command does not accept any flags or arguments.")
    ]

-- | Testing parseInput with required flag
testParseInputRequiredFlag :: Test
testParseInputRequiredFlag = 
  TestList
    [ "Parse 'commit' with message flag"
        ~: parseInput ["commit", "--message=", "test"]
        ~?= Right
          ( ParsedCommand
              { cmd = Command.Commit,
                parsedFlags = [("message", Just "test")],
                parsedArguments = []
              }
          ),
      "Parse 'commit' with message flag with no space after ="
        ~: parseInput ["commit", "--message=test2"]
        ~?= Right
          ( ParsedCommand
              { cmd = Command.Commit,
                parsedFlags = [("message", Just "test2")],
                parsedArguments = []
              }
          ),
      "Parse 'commit' with m flag with space after ="
        ~: parseInput ["commit", "-m=", "test3"]
        ~?= Right
          ( ParsedCommand
              { cmd = Command.Commit,
                parsedFlags = [("message", Just "test3")],
                parsedArguments = []
              }
          ),
      "Parse 'commit' with m flag with no space after ="
        ~: parseInput ["commit", "-m=test4"]
        ~?= Right
          ( ParsedCommand
              { cmd = Command.Commit,
                parsedFlags = [("message", Just "test4")],
                parsedArguments = []
              }
          ),
      "Parse 'commit' with invalid message flag format"
        ~: parseInput ["commit", "--message", "test5"]
        ~?= Left (CommandError "Invalid usage of 'hgit commit'. Use 'hgit commit -m=\"msg\"'.'"),
      "Parse 'commit' with invalid short message flag format"
        ~: parseInput ["commit", "-m", "test6"]
        ~?= Left (CommandError "Invalid usage of 'hgit commit'. Use 'hgit commit -m=\"msg\"'.'")
    ]

-- | Collection of all parser tests
parserTests :: Test
parserTests =
  TestLabel "Parser Tests" $
    TestList
      [ TestLabel "Parse Command" testValidateRawCommand,
        TestLabel "Parse Flags and Args" testParseFlagsAndArgs,
        TestLabel "Parse Input" testParseInput,
        TestLabel "RawCommand to tokens" testRawCommandToTokens,
        TestLabel "Parse Input with required flag" testParseInputRequiredFlag
      ]
