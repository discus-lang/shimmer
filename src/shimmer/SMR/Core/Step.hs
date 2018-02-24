{-# LANGUAGE BangPatterns #-}
module SMR.Core.Step
        ( Config        (..)
        , World         (..)
        , Result        (..)
        , newWorld
        , steps
        , step)
where
import SMR.Core.Exp
import SMR.Core.World
import SMR.Prim.Op.Base
import Data.Text                (Text)
import Data.Map                 (Map)
import qualified Data.Map       as Map


--------------------------------------------------------------------------------
-- | Evaluation config
data Config s p w
        = Config
        { -- | Reduce under lambda abstractions.
          configUnderLambdas    :: !Bool

          -- | Reduce arguments when head is not an abstraction.
        , configHeadArgs        :: !Bool

          -- | Primitive operator declarations.
        , configPrims           :: !(Map p (PrimEval s p w))

          -- | Macro declarations.
        , configDeclsMac        :: !(Map Name (Exp s p)) }


-- | Result of evaluation.
data Result
        = ResultDone
        | ResultError   Text
        deriving Show


-------------------------------------------------------------------------------
-- | Multi-step reduction to normal form.
steps   :: (Ord p, Show p)
        => Config s p w
        -> World w -> Exp s p
        -> IO (Either Text (Exp s p))

steps !config !world !xx
 = do   erx <- step config world xx
        case erx of
         Left ResultDone         -> return $ Right xx
         Left (ResultError err)  -> return $ Left err
         Right xx'               -> steps config world xx'


-------------------------------------------------------------------------------
-- | Single step reduction.
--
--   This is a definitional interpreter, intended to be easy to understand
--   and get right, but not fast. Each time we take a step we decend into
--   the AST looking for the next redex, which causes evaluation to have
--   a higher asymptotic complexity than it would with an evaluator that
--   that manages the evaluation context properly.
--
step    :: (Ord p, Show p)
        => Config s p w
        -> World w -> Exp s p
        -> IO (Either Result (Exp s p))

step !config !world !xx
 = case xx of
        -- Reference
        XRef ref
         -> case ref of
                -- Expand macro declarations.
                RMac n
                  -> case Map.lookup n (configDeclsMac config) of
                        Nothing -> return $ Left ResultDone
                        Just x  -> return $ Right x

                -- Leave other references as-is.
                _ -> return $ Left ResultDone

        -- Plain variable, we're done.
        XVar{}
         -> return $ Left ResultDone

        -- Abstraction.
        XAbs ns1 x2
         -- Reduce the body of the abstraction if requested.
         |  configUnderLambdas config
         -> do  er2'     <- step config world x2
                case er2' of
                 Left  r2  -> return $ Left r2
                 Right x2' -> return $ Right $ XAbs ns1 x2'

         -- Otherwise treat abstractions as values.
         |  otherwise
         -> return $ Left ResultDone

        -- Application.
        XApp xF []
         -> return $ Right xF

        XApp{}
         -- Unzip the application and try to step the functional expression first.
         |  Just (xF, xsArgs)    <- takeXApps xx
         -> do  erx <- step (config { configUnderLambdas = False })
                            world xF
                case erx of
                 -- Functional expression makes progress.
                 Right xF'
                  -> return $ Right $ makeXApps xF' xsArgs

                 -- Evaluation of functional expression failed.
                 Left err@(ResultError _)
                  -> return $ Left err

                 -- Functional expression is done.
                 Left ResultDone
                  -> case xF of
                      XRef (RPrm primF)  -> stepAppPrm config world primF xsArgs
                      XAbs nsParam xBody -> stepAppAbs config world nsParam xBody xsArgs

                      -- Functional expression is inactive, but optionally
                      -- continue reducing arguments to eliminate all of
                      -- the redexes in the expression.
                      _ |  configHeadArgs config
                        -> do   erxArgs <- stepFirstVal config world xsArgs
                                case erxArgs of
                                 Right xsArgs' -> return $ Right $ makeXApps xF xsArgs'
                                 Left res      -> return $ Left res

                        |  otherwise
                        -> return $ Left ResultDone

         | otherwise
         -> return $ Left ResultDone

        -- Substitution trains.
        XSub{}
         -> case pushHead xx of
                Nothing  -> return $ Left ResultDone
                Just xx' -> return $ Right xx'

        -- Boxed expressions are already normal forms.
        XKey KBox _
         -> return $ Left ResultDone

        -- Run a boxed expression.
        XKey KRun x1
         -> do  erx <- step (config { configUnderLambdas = False
                                    , configHeadArgs     = False })
                            world x1

                case erx of
                 -- Body makes progress.
                 Right x1'
                  -> return $ Right (XKey KRun x1')

                 -- Body expression evaluation failed.
                 Left err@(ResultError _)
                  -> return $ Left err

                 -- If the body expression is a box then unwrap it,
                 -- otherwise just return the value as-is.
                 Left ResultDone
                  -> case x1 of
                         XKey KBox x11   -> return $ Right x11
                         _               -> return $ Right x1


-------------------------------------------------------------------------------
-- | Step an application of a primitive operators to its arguments.
stepAppPrm
        :: (Ord p, Show p)
        => Config s p w
        -> World w -> p -> [Exp s p]
        -> IO (Either Result (Exp s p))

stepAppPrm !config !world !prim !xsArgs
 = case Map.lookup prim (configPrims config) of
        Nothing         -> return $ Left ResultDone
        Just primEval   -> stepPrim config world primEval xsArgs


-------------------------------------------------------------------------------
-- | Step an application of an abstraction applied to its arguments.
stepAppAbs
        :: (Ord p, Show p)
        => Config s p w
        -> World w -> [Param] -> Exp s p -> [Exp s p]
        -> IO (Either Result (Exp s p))

stepAppAbs !config !world !psParam !xBody !xsArgs
 = do
        let arity         = length psParam
        let args          = length xsArgs
        let xsArgs_sat    = take arity xsArgs
        let xsArgs_remain = drop arity xsArgs
        let fsParam_sat   = map formOfParam psParam

        erxs   <- stepFirst config world xsArgs_sat fsParam_sat
        case erxs of
         -- One of the args makes progress.
         Right xsArgs_sat'
          -> do let xFun    = XAbs psParam xBody
                return $ Right
                 $ makeXApps (makeXApps xFun xsArgs_sat') xsArgs_remain

         -- Stepping one of the arguments failed.
         Left err@(ResultError _)
          ->    return $ Left err

         -- The arguments are all done.
         Left ResultDone
          -- Saturated application
          | args == arity
          -> do let nsParam = map nameOfParam psParam
                let snv     = snvOfNamesArgs nsParam xsArgs
                return $ Right
                 $ snvApply False snv xBody

          -- Under application.
          | args < arity
          -> do let psParam_sat    = take args psParam
                let nsParam_sat    = map nameOfParam psParam_sat
                let psParam_remain = drop args psParam
                let snv     = snvOfNamesArgs nsParam_sat xsArgs_sat
                return $ Right
                 $ makeXApps
                        (snvApply False snv $ XAbs psParam_remain xBody)
                        xsArgs_remain

          -- Over application.
          | otherwise
          -> do let nsParam = map nameOfParam psParam
                let snv     = snvOfNamesArgs nsParam xsArgs_sat
                return $ Right
                 $ makeXApps
                        (snvApply False snv xBody)
                        xsArgs_remain


-------------------------------------------------------------------------------
-- | Step an application of a primitive operator to some arguments.
stepPrim
        :: (Ord p, Show p)
        => Config s p w
        -> World w -> PrimEval s p w -> [Exp s p]
        -> IO (Either Result (Exp s p))

stepPrim !config !world !pe !xsArgs
 | PrimEval _prim _desc csArg eval <- pe
 = let
        -- Evaluation of arguments is complete.
        evalArgs [] [] xsArgsDone
         = do   mr <- eval world (reverse xsArgsDone)
                case mr of
                 Just xResult    -> return $ Right xResult
                 Nothing         -> return $ Left ResultDone

        -- We have more args than the primitive will accept.
        evalArgs [] xsArgsRemain xsArgsDone
         = do   mr <- eval world (reverse xsArgsDone)
                case mr of
                 Just xResult    -> return $ Right $ makeXApps xResult xsArgsRemain
                 Nothing         -> return $ Left ResultDone

        -- Evaluate the next argument if needed.
        evalArgs (cArg' : csArg') (xArg' : xsArg') xsArgsDone
         -- Primitive does not demand a value fo rthis arg.
         | PExp <- cArg'
         = evalArgs csArg' xsArg' (xArg' : xsArgsDone)

         -- Primtiive demands a value for this arg.
         | otherwise
         = do   erxArg' <-  step (config { configUnderLambdas = False
                                         , configHeadArgs = False })
                                 world xArg'
                case erxArg' of
                 Left err@(ResultError _)
                  -> return $ Left err

                 Left ResultDone
                  -> evalArgs csArg' xsArg' (xArg' : xsArgsDone)

                 Right xArg''
                  -> return $ Right
                        $ makeXApps (XRef (RPrm (primEvalName pe)))
                         $ (reverse xsArgsDone) ++ (xArg'' : xsArg')

        -- We have less args than the prim will accept,
        -- so leave the application as it is.
        evalArgs _ [] _xsArgsDone
         = return $ Left ResultDone

   in   evalArgs csArg xsArgs []


-------------------------------------------------------------------------------
-- | Step the first available expression in a list,
--   reducing them all towards values.
stepFirstVal
        :: (Ord p, Show p)
        => Config s p w
        -> World w -> [Exp s p]
        -> IO (Either Result [Exp s p])

stepFirstVal !config !world !xx
 = stepFirst config world xx (replicate (length xx) PVal)


-- | Step the first available expression in a list.
stepFirst
        :: (Ord p, Show p)
        => Config s p w
        -> World w -> [Exp s p] -> [Form]
        -> IO (Either Result [Exp s p])

stepFirst !config !world !xx !ff
 = case (xx, ff) of
        ([], _)
         -> return $ Left ResultDone

        (_,  [])
         -> return $ Left ResultDone

        (x1 : xs2, f1 : fs2)
         | PExp <- f1
         -> do  erx <- stepFirst config world xs2 fs2
                case erx of
                 Left r     -> return $ Left r
                 Right xs2' -> return $ Right $ x1 : xs2'

         | otherwise
         -> do  erx1 <- step config world x1
                case erx1 of
                 Left err@(ResultError{})
                  -> return $ Left err

                 Left ResultDone
                  -> do erxs2 <- stepFirst config world xs2 fs2
                        case erxs2 of
                         Left  r    -> return $ Left r
                         Right xs2' -> return $ Right $ x1 : xs2'

                 Right x1'
                  -> return $ Right $ x1' : xs2

