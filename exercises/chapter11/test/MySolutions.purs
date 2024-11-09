module Test.MySolutions where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT, runExcept, throwError)
import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.State (State, StateT, execState, modify_, put, runStateT)
import Control.Monad.State.Trans (get)
import Control.Monad.Writer (Writer, WriterT, runWriter, runWriterT, tell)
import Control.Monad.Writer.Trans (execWriterT)
import Data.Array (fold, singleton, some)
import Data.Either (Either)
import Data.Foldable (traverse_)
import Data.Identity (Identity)
import Data.Int (even)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), joinWith, stripPrefix)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple)

testParens ∷ String → Boolean
testParens str =
  let
    openTally ∷ Char → Int → Int
    -- Open parens only considered if not already in deficit.
    -- No recovery from too-many closed parens.
    openTally '(' tally | tally >= 0 = tally + 1
    openTally ')' tally = tally - 1
    -- Non-parens has no effect
    openTally _ tally = tally

    sumParens ∷ Array Char → State Int Unit
    sumParens = traverse_ \c → modify_ $ openTally c

    finalTally ∷ Int
    finalTally = execState (sumParens $ toCharArray str) 0
  in
    finalTally == 0

-- import Split (Parser, runParser, split)

-- manyParens ∷ Parser (Array (Additive Int))
-- manyParens = many openOrCloseParen

-- openOrCloseParen ∷ Parser (Additive Int)
-- openOrCloseParen = openParen <|> closeParen <|> anyChar

-- replace all open parens by 1
-- openParen ∷ Parser (Additive Int)
-- openParen = do
--   s ← split
--   guard $ s == "("
--   pure (Additive 1)

-- replace all close parens by -1
-- closeParen ∷ Parser (Additive Int)
-- closeParen = do
--   s ← split
--   guard $ s == ")"
--   pure (Additive (-1))

-- anyChar ∷ Parser (Additive Int)
-- anyChar = do
--   _ ← split
--   pure (Additive 0)

-- testParens ∷ String → Boolean
-- testParens str =
--   let
--     default = Tuple [] ""
--     Tuple arr _ = fromRight default $ runParser manyParens str
--     runningSum = scanl (<>) (Additive 0) arr
--     isRunningSumAlwaysPositive = all (_ >= Additive 0) runningSum
--     isCloseEqOpenParens = fold arr == Additive 0
--   in
--     isRunningSumAlwaysPositive && isCloseEqOpenParens

type Level = Int

type Doc = Reader Level String

line ∷ String → Doc
line str = do
  level ← ask
  pure $ power "  " level <> str

indent ∷ Doc → Doc
indent = local (_ + 1)

cat ∷ Array Doc → Doc
-- cat = (sequence ∷ Array Doc → Reader Level (Array String)) >=> joinWith "\n" >>> pure
cat = sequence >=> joinWith "\n" >>> pure

render ∷ Doc → String
render doc = runReader doc 0

sumArrayWriter ∷ Array Int → Writer (Additive Int) Unit
sumArrayWriter = traverse_ (\n → tell $ Additive n)

collatz ∷ Int → Tuple Int (Array Int)
collatz n = runWriter $ go n 0
  where
  go ∷ Int → Int → Writer (Array Int) Int
  go 1 acc = do
    tell $ singleton 1
    pure acc
  go m acc = do
    tell $ singleton m
    go (if even m then m / 2 else 3 * m + 1) (acc + 1)

collatz' ∷ Int → Int
collatz' n = go n 0
  where
  go 1 acc = acc
  go m acc = go (if even m then m / 2 else 3 * m + 1) (acc + 1)

safeDivide ∷ Int → Int → ExceptT String Identity Int
safeDivide _ 0 = throwError "Divide by zero!"
safeDivide n1 n2 = pure $ n1 / n2

type Errors = Array String
type Log = Array String
type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

runParser' ∷ ∀ a. Parser a → String → Either Errors (Tuple (Tuple a String) Log)
runParser' p = runExcept <<< runWriterT <<< runStateT p

string ∷ String → Parser String
string prefix = do
  s ← get
  tell [ "The state is " <> s ]
  case stripPrefix (Pattern prefix) s of
    Nothing → throwError [ "Could not parse" ]
    Just stripped → do
      put stripped
      pure prefix

type Level' = Int
type Doc' = WriterT (Array String) (ReaderT Level' Identity) Unit

line' ∷ String → Doc'
line' s = do
  level ← ask
  tell [ (power "  " level) <> s ]
  pure unit

indent' ∷ Doc' → Doc'
indent' = local (_ + 1)

render' ∷ Doc' → String
render' = execWriterT >>> flip runReaderT 0 >>> unwrap >>> joinWith "\n"

asFollowedByBs ∷ Parser String
asFollowedByBs = do
  as ← some $ string "a"
  bs ← some $ string "b"
  pure $ fold $ as <> bs

asOrBs ∷ Parser String
asOrBs = fold <$> some (string "a" <|> string "b")

