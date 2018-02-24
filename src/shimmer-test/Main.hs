{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified SMR.Test.Gen           as S
import qualified SMR.Source.Pretty      as S

import qualified SMR.Core.Codec           as S
import qualified SMR.Core.Exp           as S
import qualified SMR.Prim.Op            as S
import qualified SMR.Prim.Name          as S

import           Hedgehog
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range

import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text
import Data.Text                        (Text)
import Control.Monad


prop_ref_pack_unpack :: Property
prop_ref_pack_unpack
 = withTests 10000
 $ property
 $ do   ctx     <- forAll $ S.genContextTextPrim
        let cfg =  S.Config ctx S.genText S.genPrim
        x       <- forAll $ S.genRef cfg
        S.unpackRef (S.packRef x) === x


prop_exp_pack_unpack :: Property
prop_exp_pack_unpack
 = withTests 10000
 $ property
 $ do   ctx     <- forAll $ S.genContextTextPrim
        let cfg =  S.Config ctx S.genText S.genPrim
        x       <- forAll $ S.genExp cfg
        S.unpackExp (S.packExp x) === x


prop_decl_pack_unpack :: Property
prop_decl_pack_unpack
 = withTests 10000
 $ property
 $ do   ctx     <- forAll $ S.genContextTextPrim
        let cfg =  S.Config ctx S.genText S.genPrim
        x       <- forAll $ S.genDecl cfg
        S.unpackDecl (S.packDecl x) === x


prop_decls_pack_unpack :: Property
prop_decls_pack_unpack
 = withTests 1000
 $ property
 $ do   ctx     <- forAll $ S.genContextTextPrim
        let cfg =  S.Config ctx S.genText S.genPrim
        x       <- forAll $ S.genFileDecls cfg
        S.unpackFileDecls (S.packFileDecls x) === x


sampleExps :: Int -> IO ()
sampleExps n
 = replicateM_ n
 $ do
        x       <- Gen.sample
                $  Gen.resize 10000
                $  do   ctx     <- S.genContextTextPrim
                        let cfg = S.Config ctx S.genText S.genPrim
                        S.genExp cfg

        Text.putStrLn $ S.pretty x
        Text.putStrLn ""


tests :: IO Bool
tests =
  checkParallel $$(discover)
