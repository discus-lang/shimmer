
module SMR.Core.Eval where
import SMR.Core.Exp
import SMR.Core.Prim

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
        -> [Env] -> Exp
        -> IO [Val]

eval c w envs (XVal v)
 = return [v]

eval c w envs (XMac n)
 = case Map.lookup n (configDeclsMac c) of
        Nothing -> error "unbound macro"
        Just x  -> eval c w envs x

eval c w envs (XVar n _)
 = go envs
 where  go [] = throw $ ErrorVarUnbound n
        go (env : envs')
         = case Map.lookup n env of
                Just v  -> return [v]
                Nothing -> go envs'

eval c w envs (XAbs nsParam xBody)
 = return [VClo envs nsParam xBody]

eval c w envs (XVec xs)
 = do   vss <- mapM (eval c w envs) xs
        return $ concat vss

eval c w envs (XApp xFun xArgs)
 = do   vFun  <- eval c w envs xFun
        vsArg <- eval c w envs xArgs
        case vFun of
         [VClo envs nsParam xBody]
          | length vsArg == length nsParam
          -> let env'   = Map.fromList $ zip nsParam vsArg
                 envs'  = env' : envs
             in  eval c w envs' xBody

         [VClo{}] -> error "wrong arity in function application"
         [_]      -> error "cannot apply non function"
         _        -> error "function expression has wrong arity"

eval c w envs xx@(XPrm (POPrim name) xArg)
 = case Map.lookup (POPrim name) (configPrims c) of
        Nothing
         -> throw $ ErrorPrmUnknown name

        Just peval
         -> do  vsArg   <- eval c w envs xArg
                mrs     <- primEvalFun peval w vsArg
                case mrs of
                 Nothing -> error $ "stuck " ++ show (name, vsArg)
                 Just vs -> return vs

eval _ _ _ xx
 = error $ "no match for " ++ show xx