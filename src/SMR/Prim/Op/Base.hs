
module SMR.Prim.Op.Base where
import SMR.Core.Exp
import SMR.Core.World
import Data.Text        (Text)


-------------------------------------------------------------------------------
-- | Primitive values and operators.
data Prim
        = PrimOp        Text
        | PrimLitBool   Bool
        | PrimLitNat    Integer
        | PrimTagList
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
data PrimEval s p w
        = PrimEval
        { primEvalName  :: p            -- ^ Op name.
        , primEvalDesc  :: Text         -- ^ Op description.
        , primEvalForm  :: [Form]       -- ^ Argument passing methods.

          -- | Evaluation function.
        , primEvalFun
                :: World w
                -> [Exp s p]
                -> IO (Maybe (Exp s p))
        }

