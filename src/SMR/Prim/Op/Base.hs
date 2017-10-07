
module SMR.Prim.Op.Base where
import SMR.Core.Exp
import Data.Text        (Text)


-------------------------------------------------------------------------------
-- | Primitive values and operators.
data Prim
        = PrimOp        Text
        | PrimLitBool   Bool
        | PrimLitNat    Integer
        deriving (Eq, Ord, Show)


-- Exp ----------------------------------------------------
-- | Take the first expression argument from a list of primitives.
takeArgExp
        :: [Exp s Prim]
        -> Maybe (Exp s Prim, [Exp s Prim])
takeArgExp xx
 = case xx of
        x1 : xs -> Just (x1, xs)
        _       -> Nothing


-- Bool ---------------------------------------------------
-- | Take a literal Bool from an expression.
takeXBool :: Exp s Prim -> Maybe Bool
takeXBool xx
 = case xx of
        XRef (RPrm (PrimLitBool b))     -> Just b
        _                               -> Nothing


-- | Make a literal Bool expression.
makeXBool :: Bool -> Exp s Prim
makeXBool b
 = XRef (RPrm (PrimLitBool b))


-- | Split a literal Bool from an argument list.
takeArgBool :: [Exp s Prim] -> Maybe (Bool, [Exp s Prim])
takeArgBool xx
 = case xx of
        XRef (RPrm (PrimLitBool b)) : xs
          -> Just (b, xs)
        _ -> Nothing


-- Nat ----------------------------------------------------
-- | Take a literal Nat from an expression.
takeXNat :: Exp s Prim -> Maybe Integer
takeXNat xx
 = case xx of
        XRef (RPrm (PrimLitNat n))      -> Just n
        _                               -> Nothing

-- | Make a literal Nat expression.
makeXNat :: Integer -> Exp s Prim
makeXNat n
 = XRef (RPrm (PrimLitNat n))


-- | Split a literal Nat from an argument list.
takeArgNat :: [Exp s Prim] -> Maybe (Integer, [Exp s Prim])
takeArgNat xx
 = case xx of
        XRef (RPrm (PrimLitNat n)) : xs
          -> Just (n, xs)
        _ -> Nothing


-------------------------------------------------------------------------------
-- | Primitive evaluator.
data PrimEval s p
        = PrimEval
        { primEvalName  :: p            -- ^ Op name.
        , primEvalDesc  :: Text         -- ^ Op description.
        , primEvalForm  :: [Form]       -- ^ Argument passing methods.
        , primEvalFun   :: [Exp s p] -> Maybe (Exp s p)
                                        -- ^ Evaluation function.
        }


-- | Construct a primitive evaluator for a call-by-value arity-1 operator.
primEvalOp1
        :: p -> Text -> [Form] -> ([Exp s p] -> Maybe (Exp s p))
        -> PrimEval s p
primEvalOp1 name desc args fn
 = PrimEval name desc [PVal] fn


-- | Construct a primitive evaluator for a call-by-value arity-2 operator.
primEvalOp2
        :: p -> Text -> [Form] -> ([Exp s p] -> Maybe (Exp s p))
        -> PrimEval s p
primEvalOp2 name desc args fn
 = PrimEval name desc [PVal, PVal] fn

