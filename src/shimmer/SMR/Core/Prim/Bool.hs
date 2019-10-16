
module SMR.Core.Prim.Bool where
import SMR.Core.Prim.Base
import SMR.Core.Exp
import Data.Text        (Text)


-- | Primitive evaluators for boolean operators.
primOpsBool :: [PrimEval w]
primOpsBool
 = [ primOpBool1 "not" "boolean negation" (\b -> not b)
   , primOpBool2 "and" "boolean and"      (&&)
   , primOpBool2 "or"  "boolean or"       (||)
   , primOpIf ]


-- | Construct an evaluator for 1-arity bool operator.
primOpBool1
        :: Name -> Text
        -> (Bool -> Bool)
        -> PrimEval w

primOpBool1 name desc fn
 = PrimEval (POp name) desc fn'
 where  fn' _world [XBool b1]
         = return $ Just $ XBool (fn b1)
        fn' _world _
         = return $ Nothing


-- | Construct an evaluator for 2-arity bool operator.
primOpBool2
        :: Name -> Text
        -> (Bool -> Bool -> Bool)
        -> PrimEval w

primOpBool2 name desc fn
 = PrimEval (POp name) desc fn'
 where
        fn' _world [XBool b1, XBool b2]
         = return $ Just $ XBool (fn b1 b2)
        fn' _world _
         = return $ Nothing


-- | Primitive evaluator for the #if operator.
--   Only the scrutinee is demanded, while the branches are not.
primOpIf :: PrimEval w
primOpIf
 = PrimEval
        (POp "if")
        "boolean if-then-else operator"
        fn'
 where
        fn' _world [XBool b1, x1, x2]
         = return $ Just $ if b1 then XRun x1 else XRun x2

        fn' _world _
         = return $ Nothing

