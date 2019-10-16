
module SMR.Core.Prim.Nat where
import SMR.Core.Prim.Base
import SMR.Core.Exp

type Nat = Integer

-- | Primitive evaluators for nat operators.
primOpsNat :: [PrimEval w]
primOpsNat
 = [ primOpNat2Nat  "nat-add" "natural addition"            (+)
   , primOpNat2Nat  "nat-sub" "natural subtration"
        (\a b -> let x = a - b
                 in if x < 0 then 0 else x)

   , primOpNat2Nat  "nat-mul" "natural multiplication"      (*)
   , primOpNat2Nat  "nat-div" "natural division"            div
   , primOpNat2Nat  "nat-rem" "natural remainder"           rem
   , primOpNat2Bool "nat-eq"  "natural equality"            (==)
   , primOpNat2Bool "nat-neq" "natural negated equality"    (/=)
   , primOpNat2Bool "nat-lt"  "natural less than"           (<)
   , primOpNat2Bool "nat-le"  "natural less than equal"     (<=)
   , primOpNat2Bool "nat-gt"  "natural greater than"        (>)
   , primOpNat2Bool "nat-ge"  "natural greather than equal" (>=) ]


-- | Construct an evaluator for a 2-arity nat operator returning nat.
primOpNat2Nat
        :: Text -> Text -> (Nat -> Nat -> Nat)
        -> PrimEval w
primOpNat2Nat name desc fn
 =  PrimEval (POp name) desc fn'
 where  fn' _world [XNat n1, XNat n2]
         = return $ Just $ XNat (fn n1 n2)
        fn' _world _
         = return $ Nothing


-- | Construct an evaluator for a 2-arity nat operator returning bool.
primOpNat2Bool
        :: Text -> Text -> (Nat -> Nat -> Bool)
        -> PrimEval w
primOpNat2Bool name desc fn
 =  PrimEval (POp name) desc fn'
 where  fn' _world [XNat n1, XNat n2]
         = return $ Just $ XBool (fn n1 n2)
        fn' _world _
         = return $ Nothing
