module Test.MySolutions where

import Prelude

import Affjax (URL)
import Affjax.Node as AN
import Affjax.ResponseFormat as AXRF
import Control.Parallel (parOneOf, parTraverse)
import Data.Array (concat, (:))
import Data.Either (Either(..), hush)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), length, split)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff, Error, attempt, delay)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath)
import Node.Path as Path
import Test.HTTP (getUrl)

concatenateFiles ∷ FilePath → FilePath → FilePath → Aff Unit
concatenateFiles in1 in2 out = do
  in_txt1 ← readTextFile UTF8 in1
  in_txt2 ← readTextFile UTF8 in2
  writeTextFile UTF8 out $ in_txt1 <> in_txt2

concatenateMany ∷ Array FilePath → FilePath → Aff Unit
concatenateMany inFiles outFile = do
  in_txts ← traverse (readTextFile UTF8) inFiles
  writeTextFile UTF8 outFile $ fold in_txts

-- countCharacters ∷ FilePath → Aff (Either Error Int)
-- countCharacters inFile = do
--   in_txt ← try $ readTextFile UTF8 inFile
--   case in_txt of
--     Left error → pure $ Left error
--     Right txt → pure $ Right $ length txt

countCharacters ∷ FilePath → Aff (Either Error Int)
countCharacters file = attempt do
  contents ← readTextFile UTF8 file
  pure $ length contents

writeGet ∷ URL → FilePath → Aff Unit
writeGet reqUrl outFile = do
  str ← getUrl reqUrl
  writeTextFile UTF8 outFile str

concatenateManyParallel ∷ Array FilePath → FilePath → Aff Unit
concatenateManyParallel inFiles outFile = do
  in_txts ← parTraverse (readTextFile UTF8) inFiles
  writeTextFile UTF8 outFile $ fold in_txts

-- getWithTimeout ∷ Number → URL → Aff (Maybe String)
-- getWithTimeout time_out url = do
--   parOneOf [ delay (Milliseconds time_out) $> Nothing, pure <$> getUrl url ]

getWithTimeout ∷ Number → String → Aff (Maybe String)
getWithTimeout ms url =
  parOneOf
    [ AN.get AXRF.string url <#> hush <#> map _.body
    , delay (Milliseconds ms) $> Nothing
    ]

recurseFiles ∷ FilePath → Aff (Array FilePath)
recurseFiles file = do
  contents ← readTextFile UTF8 file
  case contents of
    "" → pure [ file ]
    _c → do
      let
        dir = Path.dirname file
        files = split (Pattern "\n") contents
        filesFromRoot = map (\f → Path.concat [ dir, f ]) files
      arrarr ← parTraverse recurseFiles filesFromRoot
      pure $ file : concat arrarr
