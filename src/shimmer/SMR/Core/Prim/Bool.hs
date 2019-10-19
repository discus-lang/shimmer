
module SMR.Core.Prim.Bool (primOpsBool) where
import SMR.Core.Prim.Base
import SMR.Core.Exp
import Data.Text        (Text)


-- | Primitive evaluators for boolean operators.
primOpsBool :: [PrimEval w]
primOpsBool
 = [ PP { name  = "not"
        , desc  = "boolean negation"
        , eval  = opBool1 not }

   , PP { name  = "and"
        , desc  = "boolean and"
        , eval  = opBool2 (&&) }

   , PP { name  = "or"
        , desc  = "boolean or"
        , eval  = opBool2 (||) }

   , PP { name  = "if"
        , desc  = "selection based on a boolean"
        , eval  = \case [VBool b1, v2, v3]
                          -> Just $ if b1 then [v2] else [v3]
                        _ -> Nothing }
   ]


opBool1 :: (Bool -> Bool) -> [Val] -> Maybe [Val]
opBool1 f [VBool a]             = Just [VBool $ f a]
opBool1 f _                     = Nothing

opBool2 :: (Bool -> Bool -> Bool) -> [Val] -> Maybe [Val]
opBool2 f [VBool a, VBool b]    = Just [VBool $ f a b]
opBool2 f _                     = Nothing

