
module SMR.Core.Eval where
import SMR.Core.Exp
import SMR.Core.Prim

import Control.Monad
import Control.Exception
import Data.Typeable
import Data.Map                 (Map)
import qualified Data.Map       as Map

--------------------------------------------------------------------------------
-- | Evaluation config
data Config w
        = Config
        { -- | Primitive operator declarations.
          configPrims           :: Map PrimOp (PrimEval w)

          -- | Macro declarations.
        , configDeclsMac        :: Map Name Exp }


--------------------------------------------------------------------------------
-- | Evaluation errors.
data Error
        = ErrorVarUnbound Name
        | ErrorPrmUnknown Name
        | ErrorPrmStuck   Name
        deriving (Typeable, Show, Exception)


--------------------------------------------------------------------------------
-- | Big Step Evaluation.
eval    :: Config w -> World w
        -> [Env] -> Exp -> IO [Val]

eval c w envs xx
 = do   ss      <- seek c w envs xx
        mapM (reduce c w) ss


--------------------------------------------------------------------------------
data Solid
        = SVal Val
        | SExp [Env] Exp
        deriving (Eq, Show)

-- | Reduce a solid to a value.
reduce  :: Config w -> World w -> Solid -> IO Val
reduce c w (SExp envs x)
 = do   vs      <- eval c w envs x
        case vs of
         [v]    -> return v
         _      -> error "arity error in solid reduction"

reduce c w (SVal (VThk envs x))
 = do   vs      <- eval c w envs x
        case vs of
         [v]    -> return v
         _      -> error "arity error in solid reduction"

reduce c w (SVal v) = return v


-- | Convert a solid to a value representation,
--   packing any expressions that are not already values into thunks.
convert :: Config w -> World w -> Solid -> IO Val
convert c w (SVal v)
 = return v

convert c w (SExp envs x)
 = return $ VThk envs x


--------------------------------------------------------------------------------
-- | Evaluate an expression far enough to see what the arity of the result is,
--   but don't force evaluation of any the components of that result.
seek    :: Config w -> World w
        -> [Env] -> Exp -> IO [Solid]

seek c w envs x@(XVal v)
 = return [SVal v]

seek c w envs (XMac n)
 = case Map.lookup n (configDeclsMac c) of
        Nothing -> error "unbound macro"
        Just x  -> seek c w envs x

seek c w envs (XVar n _)
 = go envs
 where  go [] = throw $ ErrorVarUnbound n
        go (env : envs')
         = case Map.lookup n env of
                Just v  -> return [SVal v]
                Nothing -> go envs'

seek c w envs x@(XAbs bs ns xBody)
 = return [SVal $ VClo envs bs ns xBody]

seek c w envs (XVec xs)
 = return $ map (SExp envs) xs

seek c w envs (XApp xFun xArgs)
 = do   vFun    <- eval c w envs xFun
        ssArg   <- seek c w envs xArgs

        case vFun of
         [VClo envs' bs ns xBody]
          | length bs == length ns
          , length bs == length ssArg
          -> do let capture True  s = reduce  c w s
                    capture False s = convert c w s
                vsArg    <- zipWithM capture bs ssArg
                let env'   = Map.fromList $ zip ns vsArg
                let envs'' = env' : envs'
                seek c w envs'' xBody

         [VClo{}] -> error "wrong arity in function application"
         [_]      -> error "cannot apply non function"
         _        -> error "function expression has wrong arity"

seek c w envs xx@(XPrm (POPrim name) xArg)
 = case Map.lookup (POPrim name) (configPrims c) of
        Nothing
         -> throw $ ErrorPrmUnknown name

        Just peval
         -> do  vsArg   <- eval c w envs xArg
                mrs     <- primEvalFun peval w vsArg
                case mrs of
                 Nothing -> error $ "stuck " ++ show (name, vsArg)
                 Just vs -> return $ map SVal vs

seek c w envs xx@(XDel xBody)
 = return [SVal (VThk envs xBody)]

seek c w envs xx@(XNow xBody)
 = do   vs <- eval c w envs xBody
        return $ map SVal vs

seek _ _ _ xx
 = error $ "no match for " ++ show xx


