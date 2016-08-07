module Test.Main where

import Prelude

import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Exception

import Data.Either
import Data.Foldable
import Data.List
import Data.Maybe

import Language.Robo.Spec
import Language.Robo.Parser

fail :: forall eff. String -> Eff (err :: EXCEPTION | eff) Unit
fail msg = throwException $ error msg

shouldParseAs :: forall eff. Program -> String -> Eff (err :: EXCEPTION | eff) Unit
shouldParseAs expected input =
    case parseRobo input of
        Left err -> fail $ "Parse error for input '" <> input <> "': " <> show err
        Right output ->
            unless (output == expected) $ do
                fail $ "Unexpected result:\n" <>
                       "Input:    '" <> input <> "'\n" <>
                       "Output:   " <> show output <> "\n" <>
                       "Expected: " <> show expected <> "\n"

allParseAs :: forall eff. Program -> Array String -> Eff (err :: EXCEPTION | eff) Unit
allParseAs expected = traverse_ (shouldParseAs expected)

shouldFail :: forall eff. String -> Eff (err :: EXCEPTION | eff) Unit
shouldFail input = do
    case (parseRobo input) of
         Left _ -> pure unit
         Right output -> fail $ "Should throw a parse error: '" <> input <> "'"

-- | Add label
l :: String -> Instruction -> LInstruction
l label = LInstruction (Just label)

-- | No label
nl :: Instruction -> LInstruction
nl = LInstruction Nothing

main :: Eff (err :: EXCEPTION) Unit
main = do
  allParseAs ((nl MoveLeft) : Nil)
    [ "left"
    , "   left   "
    , "  \n   left   \n   "
    ]

  allParseAs ((nl MoveLeft) : (nl MoveRight) : Nil)
    [ "left\nright"
    , "  left   \n  right"
    ]

  allParseAs ((Comment "foo") : (nl MoveLeft) : (Comment "bar") : (nl MoveRight) : Nil)
    [ "#foo\nleft\n#bar\nright"
    , "  #foo \n  left   \n  #  bar  \n right"
    ]

  allParseAs ((l "foo" MoveLeft) : (nl $ Goto "foo") : Nil)
    [ "foo: left\ngoto foo"
    , "   foo: left   \n   \n  goto    foo  "
    ]

  shouldParseAs
    (  (l "left" MoveLeft)
     : (l "right" MoveRight)
     : (Comment "first comment")
     : (l "skipNext" SkipNext)
     : (Comment "second comment")
     : (l "goto" (Goto "left"))
     : Nil)
    """
    left: left
    right: right
    # first comment
    skipNext: skipNext
    # second comment
    goto: goto left
    """

  shouldFail "foo"
  shouldFail "left right"
  shouldFail "left \n foo"
