
module SMR.Test.Gen where
import qualified SMR.Core.Exp           as S
import qualified SMR.Prim.Op            as S
import qualified SMR.Prim.Op.Base       as S
import qualified SMR.Prim.Name          as S

import           Hedgehog
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range

import qualified Data.Text              as Text
import Data.Text                        (Text)
import Control.Monad


-- Context ----------------------------------------------------------------------------------------
data Context s p
        = Context
        { contextMacros :: [S.Name]
        , contextSets   :: [S.Name]
        , contextVars   :: [S.Name]
        , contextGenSym :: Gen s
        , contextGenPrm :: Gen p }


genContextTextPrim :: Gen (Context Text S.Prim)
genContextTextPrim
 = do   nsMac   <- Gen.list (Range.linear 1 10) (genText)
        nsSet   <- Gen.list (Range.linear 1 10) (genText)

        return
         $ Context
                { contextMacros = nsMac
                , contextSets   = nsSet
                , contextVars   = []
                , contextGenSym = genText
                , contextGenPrm = genPrim }


-- Exp --------------------------------------------------------------------------------------------
genExp :: Context s p -> Gen (S.Exp s p)
genExp ctx
 = Gen.recursive Gen.choice
        [ S.XRef <$> genRef ctx]
        [ S.XKey <$> genKey <*> Gen.small (genExp ctx)

        , Gen.subterm2 (genExp ctx) (genExp ctx)
           $ \x1 x2 -> S.XApp x1 [x2]

        , S.XVar <$> genText
                 <*> (fmap fromIntegral $ Gen.word16 (Range.linear 1 10))

        , S.XAbs <$> Gen.list (Range.linear 1 10) genParam
                 <*> genExp ctx
        ]


-- Param ------------------------------------------------------------------------------------------
genParam :: Gen S.Param
genParam
 = S.PParam <$> genText <*> genForm
 where  genForm  = Gen.choice $ map return [S.PVal, S.PExp]


-- Key --------------------------------------------------------------------------------------------
genKey :: Gen S.Key
genKey
 = Gen.choice $ map return [ S.KBox, S.KRun ]


-- Ref --------------------------------------------------------------------------------------------
genRef :: Context s p -> Gen (S.Ref s p)
genRef ctx
 = Gen.choice
 $      [ fmap S.RSym (contextGenSym ctx)
        , fmap S.RPrm (contextGenPrm ctx)
        , fmap S.RTxt genText
        , fmap S.RMac (Gen.choice $ map return $ contextMacros ctx)
        , fmap S.RSet (Gen.choice $ map return $ contextSets   ctx)
        , fmap S.RNom (fmap fromIntegral $ Gen.resize (Size 100) $ Gen.word16 (Range.linear 1 10))
        ]


-- Prim -------------------------------------------------------------------------------------------
genPrim :: Gen S.Prim
genPrim
 = Gen.choice
        [ return S.PrimTagUnit
        , return S.PrimTagList
        , fmap   S.PrimLitBool    $ Gen.choice [return True, return False]
        , fmap   S.PrimLitNat     $ fmap fromIntegral $ Gen.word32 (Range.linear 1 1000)
        , fmap   S.PrimLitInt     $ fmap fromIntegral $ Gen.int    (Range.linear (-1000) 1000)

        , fmap   S.PrimLitWord8   $ Gen.word8  $ Range.constant 0 255
        , fmap   S.PrimLitWord16  $ Gen.word16 $ Range.constant 0 65535
        , fmap   S.PrimLitWord32  $ Gen.word32 $ Range.constant 0 (2^(28 :: Int))
        , fmap   S.PrimLitWord64  $ Gen.word64 $ Range.constant 0 (2^(62 :: Int))

        , fmap   S.PrimLitInt8    $ Gen.int8   $ Range.constant (-128)   127
        , fmap   S.PrimLitInt16   $ Gen.int16  $ Range.constant (-32768) 32767
        , fmap   S.PrimLitInt32   $ Gen.int32  $ Range.constant (-(2^(28 :: Int))) (2^(28 :: Int))
        , fmap   S.PrimLitInt64   $ Gen.int64  $ Range.constant (-(2^(62 :: Int))) (2^(62 :: Int))

        , fmap   S.PrimLitFloat32 $ Gen.float  $ Range.exponentialFloat 0 100000
        , fmap   S.PrimLitFloat64 $ Gen.double $ Range.exponentialFloat 0 100000

        , fmap   S.PrimOp         $ Gen.choice $ map return
                                  $ [tx | S.PrimOp tx <- map S.primEvalName $ S.primEvals ]
        ]


-- Text -------------------------------------------------------------------------------------------
genText :: Gen Text
genText
 = Gen.choice
        [ Gen.text
                (Range.linear 1 20)
                (Gen.choice $ map return nameBodyChars)

        , Gen.resize (Size 100)
           $ Gen.text
                (Range.linear 1 20)
                (Gen.choice $ map return nameBodyChars) ]


nameBodyChars :: [Char]
nameBodyChars
        =  ['a' .. 'z']
        ++ ['A' .. 'Z']
        ++ ['0' .. '9']
        ++ ['-', '\'', '_']
