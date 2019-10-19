
module SMR.Core.Prim.Nat (primOpsNat) where
import SMR.Core.Prim.Base
import SMR.Core.Exp


-- | Primitive operators for natural numbers.
primOpsNat :: [PrimEval w]
primOpsNat
 = [ PP { name  = "nat'add"
        , desc  = "natural addition"
        , eval  = opNat2Nat (+) }

   , PP { name  = "nat'sub"
        , desc  = "natural subtraction"
        , eval  = opNat2Nat $ \a b -> let x = a - b in if x < 0 then 0 else x }

   , PP { name  = "nat'mul"
        , desc  = "natural multiplication"
        , eval  = opNat2Nat (*) }

   , PP { name  = "nat'div"
        , desc  = "natural division"
        , eval  = \case [VNat a, VNat b]
                          -> if b > 0 then Just [VNat $ div a b] else Nothing
                        _ -> Nothing }

   , PP { name  = "nat'rem"
        , desc  = "natural remainder"
        , eval  = \case [VNat a, VNat b]
                          -> if b > 0 then Just [VNat $ rem a b] else Nothing
                        _ -> Nothing }

   , PP { name  = "nat'eq"
        , desc  = "natural equality"
        , eval  = opNat2Bool (==) }

   , PP { name  = "nat'neq"
        , desc  = "natural negated equality"
        , eval  = opNat2Bool (/=) }

   , PP { name  = "nat'lt"
        , desc  = "natura less than"
        , eval  = opNat2Bool (<) }

   , PP { name  = "nat'le"
        , desc  = "natural less than equal"
        , eval  = opNat2Bool (<=) }

   , PP { name  = "nat'gt"
        , desc  = "natural greater than"
        , eval  = opNat2Bool (>) }

   , PP { name  = "nat'ge"
        , desc  = "natural greater than equal"
        , eval  = opNat2Bool (>=) }
   ]


type Nat = Integer

opNat2Nat :: (Nat -> Nat -> Nat)   -> [Val] -> Maybe [Val]
opNat2Nat f [VNat a, VNat b]    = Just [VNat $ f a b]
opNat2Nat _ _                   = Nothing

opNat2Bool :: (Nat -> Nat -> Bool) -> [Val] -> Maybe [Val]
opNat2Bool f [VNat a, VNat b]   = Just [VBool $ f a b]
opNat2Bool _ _                  = Nothing
