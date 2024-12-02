{-# LANGUAGE OverloadedStrings #-}

module ParserTests
  ( parserTests,
    prop_parseInput_correct
  )
where

import CommandParser
  ( Command (Command, flags, subcommand),
    Flag (..),
    FlagType (..),
    ParsedCommand (..),
    CommandError (..),
    defaultValidate,
    parseCommand,
    parseFlagsAndArgs,
    parseInput,
  )
import Test.HUnit ( (~:), (~?=), Test(TestLabel, TestList) )
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Property,
    Testable,
    chooseInt,
    elements,
    forAll,
    frequency,
    listOf,
    listOf1,
    quickCheck,
    quickCheckWith,
    stdArgs,
    (==>), Gen,
  )
import Control.Monad (when)
import Data.Bifunctor qualified
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding.Error (UnicodeException)
import System.FilePath (takeDirectory, (</>))
import Data.Set qualified as Set
import TestUtils ( testValidate )
import Commands (commandInit, commandAdd, commandCommit, commandBranch)

-- Parsing-related Tests

testParseCommand :: Test
testParseCommand =
  TestList
    [ "Valid Command 'add'"
        ~: parseCommand [commandAdd] ["add"]
        ~?= Right (commandAdd, []),
      "Valid Command 'init'"
        ~: parseCommand [commandInit] ["init"]
        ~?= Right (commandInit, []),
      "Invalid Command"
        ~: parseCommand [commandAdd, commandInit, commandCommit, commandBranch] ["status"]
        ~?= Left (CommandError "Unknown command: status")
    ]

testParseFlagsAndArgs :: Test
testParseFlagsAndArgs =
  TestList
    [ "Parse no-arg flag (--update)"
        ~: parseFlagsAndArgs (flags commandAdd) ["--update"]
        ~?= Right ([("update", Nothing)], []),
      "Parse short flag (-u)"
        ~: parseFlagsAndArgs (flags commandAdd) ["-u"]
        ~?= Right ([("update", Nothing)], []),
      "Parse arguments"
        ~: parseFlagsAndArgs (flags $ commandAdd) ["file1.txt", "file2.txt"]
        ~?= Right ([], ["file1.txt", "file2.txt"]),
      "Parse flags and arguments"
        ~: parseFlagsAndArgs (flags $ commandAdd) ["--update", "file1.txt"]
        ~?= Right ([("update", Nothing)], ["file1.txt"])
    ]

testParseInput :: Test
testParseInput =
  TestList
    [ "Parse 'add' with no flags or args"
        ~: parseInput [commandAdd] ["add"]
        ~?= Left (CommandError "Invalid usage of 'hgit add'. Use 'hgit add -u', 'hgit add <file>... ', or 'hgit add .'"),
      "Parse 'add' with --update flag"
        ~: parseInput [commandAdd] ["add", "--update"]
        ~?= Right
          ( ParsedCommand
              { parsedSubcommand = commandAdd,
                parsedFlags = [("update", Nothing)],
                parsedArguments = []
              }
          ),
      "Parse 'add' with files"
        ~: parseInput [commandAdd] ["add", "file1.txt", "file2.txt"]
        ~?= Right
          ( ParsedCommand
              { parsedSubcommand = commandAdd,
                parsedFlags = [],
                parsedArguments = ["file1.txt", "file2.txt"]
              }
          ),
      "Parse 'add' with flags and files"
        ~: parseInput [commandAdd] ["add", "-u", "file1.txt"]
        ~?= Left (CommandError "Invalid usage of 'hgit add'. Use 'hgit add -u', 'hgit add <file>... ', or 'hgit add .'"),
      "Parse 'init' with no flags or args"
        ~: parseInput [commandInit] ["init"]
        ~?= Right
          ( ParsedCommand
              { parsedSubcommand = commandInit,
                parsedFlags = [],
                parsedArguments = []
              }
          ),
      "Parse 'init' with unexpected args"
        ~: parseInput [commandInit] ["init", "extra"]
        ~?= Left (CommandError "This command does not accept any flags or arguments.")
    ]

-- | Validation function to ensure no flag conflicts
validateCommand :: Command -> Bool
validateCommand cmd =
  let flagNames = map longName (flags cmd)
      shortNames = mapMaybe shortName (flags cmd)
      longNamesSet = Set.fromList flagNames
      shortNamesSet = Set.fromList shortNames
      uniqueLongNames = length flagNames == Set.size longNamesSet
      uniqueShortNames = length shortNames == Set.size shortNamesSet
      noOverlap = Set.null $ Set.intersection longNamesSet shortNamesSet
   in uniqueLongNames && uniqueShortNames && noOverlap

-- | Property to test that parseInput correctly parses commands
prop_parseInput_correct :: Property
prop_parseInput_correct = forAll arbitrary $ \cmd ->
  validateCommand cmd ==>
    let flagsWithValues = map assignFlagValue (flags cmd)
        inputArgs = constructInputArgs cmd flagsWithValues []
        expectedParsedFlags = map (Data.Bifunctor.first longName) flagsWithValues
        expectedParsedCommand =
          ParsedCommand
            { parsedSubcommand = cmd,
              parsedFlags = expectedParsedFlags,
              parsedArguments = []
            }
     in case parseInput [cmd] inputArgs of
          Right parsedCmd -> parsedCmd == expectedParsedCommand
          Left err -> error $ "Parser failed with error: " ++ show err
  where
    assignFlagValue :: Flag -> (Flag, Maybe String)
    assignFlagValue flag = case flagType flag of
      RequiresArg -> (flag, Just "value")
      NoArg -> (flag, Nothing)

constructInputArgs :: Command -> [(Flag, Maybe String)] -> [String] -> [String]
constructInputArgs cmd flagValues argsList =
  [subcommand cmd] ++ flagStrings ++ argsList
  where
    flagStrings = concatMap flagToArg flagValues
    flagToArg (flag, mValue) =
      let flagName = case shortName flag of
            Just sName -> "-" ++ sName
            Nothing -> "--" ++ longName flag
       in case (flagType flag, mValue) of
            (RequiresArg, Just val) -> [flagName, val]
            (NoArg, _) -> [flagName]
            _ -> []

instance Arbitrary FlagType where
  arbitrary = elements [NoArg, RequiresArg]

instance Arbitrary Flag where
  arbitrary = do
    long <- listOf1 (elements ['a' .. 'z'])
    short <- frequency [(1, Just <$> listOf1 (elements ['a' .. 'z'])), (1, return Nothing)]
    Flag long short <$> arbitrary

instance Arbitrary Command where
  arbitrary = do
    subcmd <- listOf1 $ elements ['a' .. 'z']
    desc <- listOf $ elements $ ['a' .. 'z'] ++ [' ']
    numFlags <- chooseInt (0, 5)
    flags <- generateUniqueFlags numFlags
    return $ Command subcmd desc flags testValidate

generateUniqueFlags :: Int -> Gen [Flag]
generateUniqueFlags n = go n [] []
  where
    go 0 _ _ = return []
    go count usedLongNames usedShortNames = do
      long <- listOf1 (elements ['a' .. 'z'])
      if long `elem` usedLongNames
        then go count usedLongNames usedShortNames
        else do
          mShort <- frequency [(1, Just <$> listOf1 (elements ['a' .. 'z'])), (1, return Nothing)]
          let shortValid = case mShort of
                Just short -> short `notElem` usedShortNames && short /= long
                Nothing -> True
          if not shortValid
            then go count usedLongNames usedShortNames
            else do
              flagType <- arbitrary
              let newUsedLongNames = long : usedLongNames
              let newUsedShortNames = maybe usedShortNames (: usedShortNames) mShort
              rest <- go (count - 1) newUsedLongNames newUsedShortNames
              return $ Flag long mShort flagType : rest

-- | Collection of all parser tests
parserTests :: Test
parserTests =
  TestLabel "Parser Tests" $
    TestList
      [ TestLabel "Parse Command" testParseCommand,
        TestLabel "Parse Flags and Args" testParseFlagsAndArgs,
        TestLabel "Parse Input" testParseInput
      ]