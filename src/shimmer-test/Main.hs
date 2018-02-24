{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified SMR.Test.Gen           as S
import qualified SMR.Source.Pretty      as S

import qualified SMR.Core.Exp           as S
import qualified SMR.Prim.Op            as S
import qualified SMR.Prim.Op.Base       as S
import qualified SMR.Prim.Name          as S

import           Hedgehog
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range

import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text
import Data.Text                        (Text)
import Control.Monad


prop_reverse :: Property
prop_reverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs


tests :: IO Bool
tests =
  checkParallel $$(discover)


sampleExps :: Int -> IO ()
sampleExps n
 = replicateM_ n
 $ do
        x       <- Gen.sample
                $  Gen.resize 10000
                $  do  ctx     <- S.genContextTextPrim
                       S.genExp ctx

        Text.putStrLn $ S.pretty x
        Text.putStrLn ""
