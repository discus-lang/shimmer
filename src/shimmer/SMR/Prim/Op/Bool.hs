{-# LANGUAGE OverloadedStrings #-}
module SMR.Prim.Op.Bool where
import SMR.Core.Exp
import SMR.Prim.Op.Base
import Data.Text        (Text)


-- | Primitive evaluators for boolean operators.
primOpsBool :: [PrimEval s Prim w]
primOpsBool
 = [ primOpBool1 "not" "boolean negation" (\b -> not b)
   , primOpBool2 "and" "boolean and"      (&&)
   , primOpBool2 "or"  "boolean or"       (||)
   , primOpIf ]


-- | Construct an evaluator for 1-arity bool operator.
primOpBool1
        :: Name -> Text
        -> (Bool -> Bool)
        -> PrimEval s Prim w

primOpBool1 name desc fn
 = PrimEval (PrimOp name) desc [PVal] fn'
 where  fn' _world as0
         | Just (b1, []) <- takeArgBool as0
         = return $ Just $ makeXBool (fn b1)
        fn' _world _
         = return $ Nothing


-- | Construct an evaluator for 2-arity bool operator.
primOpBool2
        :: Name -> Text
        -> (Bool -> Bool -> Bool)
        -> PrimEval s Prim w

primOpBool2 name desc fn
 = PrimEval (PrimOp name) desc [PVal, PVal] fn'
 where
        fn' _world as0
         | Just (b1, as1) <- takeArgBool as0
         , Just (b2, [])  <- takeArgBool as1
         = return $ Just $ makeXBool (fn b1 b2)
        fn' _world _
         = return $ Nothing


-- | Primitive evaluator for the #if operator.
--   Only the scrutinee is demanded, while the branches are not.
primOpIf :: PrimEval s Prim w
primOpIf
 = PrimEval
        (PrimOp "if")
        "boolean if-then-else operator"
        [PVal, PExp, PExp] fn'
 where
        fn' _world as0
         | Just (b1, as1) <- takeArgBool as0
         , Just (x1, as2) <- takeArgExp  as1
         , Just (x2, [])  <- takeArgExp  as2
         = return $ Just $ if b1 then x1 else x2

        fn' _world _
         = return $ Nothing

