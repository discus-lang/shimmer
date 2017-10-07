
module SMR.Core.Step
        ( Config        (..)
        , Result        (..)
        , steps
        , step)
where
import SMR.Core.Exp
import SMR.Prim.Name
import SMR.Prim.Op.Base
import Data.Text                (Text)
import Data.Map                 (Map)
import Data.Maybe
import qualified Data.Map       as Map


--------------------------------------------------------------------------------
-- | Evaluation config
data Config s p
        = Config
        { -- | Reduce under lambda abstractions.
          configUnderLambdas    :: Bool

          -- | Reduce arguments when head is not an abstraction.
        , configHeadArgs        :: Bool

          -- | Primitive operator declarations.
        , configPrims           :: Map p (PrimEval s p)

          -- | Macro declarations.
        , configDeclsMac        :: Map Name (Exp s p) }


-- | Result of evaluation.
data Result
        = ResultDone
        | ResultError   Text
        deriving Show


-------------------------------------------------------------------------------
-- | Multi-step reduction to normal form.
steps   :: (Ord p, Show p)
        => Config s p -> Exp s p
        -> Either Text (Exp s p)
steps config xx
 = case step config xx of
        Left ResultDone         -> Right xx
        Left (ResultError err)  -> Left err
        Right xx'               -> steps config xx'


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
        => Config s p -> Exp s p
        -> Either Result (Exp s p)
step config xx
 = case xx of
        -- Reference
        XRef ref
         -> case ref of
                -- Expand macro declarations.
                RMac n
                  -> case Map.lookup n (configDeclsMac config) of
                        Nothing -> Left ResultDone
                        Just x  -> Right x

                -- Leave other references as-is.
                _ -> Left ResultDone

        -- Plain variable, we're done.
        XVar{}
         -> Left ResultDone

        -- Abstraction.
        XAbs ns1 x2
         -- Reduce the body of the abstraction if requested.
         |  configUnderLambdas config
         ,  Right x2'    <- step config x2
         -> Right $ XAbs ns1 x2'

         -- Otherwise treat abstractions as values.
         |  otherwise
         -> Left ResultDone

        -- Application.
        XApp{}
         -- Unzip the application and try to step the functional expression first.
         | Just (xF, xsArgs)    <- takeXApps xx
         -> case step (config { configUnderLambdas = False }) xF of
                -- Functional expression makes progress.
                Right xF' -> Right $ makeXApps xF' xsArgs

                -- Evaluation of functional expression failed.
                Left err@(ResultError _) -> Left err

                -- Functional expression is done.
                Left ResultDone
                 -> case xF of
                     XRef (RPrm primF)  -> stepAppPrm config primF xsArgs
                     XAbs nsParam xBody -> stepAppAbs config nsParam xBody xsArgs
                     XKey KSeq xBody    -> stepAppSeq xBody xsArgs
                     XKey KTag xBody    -> stepAppTag config xBody xsArgs

                     -- Functional expression is inactive, but optionally
                     -- continue reducing arguments to eliminate all of
                     -- the redexes in the expression.
                     _ |  configHeadArgs config
                       -> case stepFirstVal config xsArgs of
                           Right xsArgs' -> Right $ makeXApps xF xsArgs'
                           Left res      -> Left res

                       |  otherwise
                       -> Left ResultDone

        -- Substitution trains.
        XSub{}
         -> case pushHead xx of
                Nothing  -> Left ResultDone
                Just xx' -> Right xx'

        -- Boxed expressions are already normal forms.
        XKey KBox _
         -> Left ResultDone

        -- Run a boxed expression.
        XKey KRun x1
         -> case step (config { configUnderLambdas = False
                              , configHeadArgs     = False }) x1 of
                -- Body makes progress.
                Right x1'
                 -> Right (XKey KRun x1')

                -- Body expression evaluation failed.
                Left err@(ResultError _)
                 -> Left err

                -- If the body expression is a box then unwrap it,
                -- otherwise just return the value as-is.
                Left ResultDone
                 -> case x1 of
                        XKey KBox x11   -> Right x11
                        _               -> Right x1

        -- Step the body of a seq expression.
        XKey KSeq x
         -> case step config x of
                Right x'  -> Right $ XKey KSeq x'
                Left err  -> Left err

        -- Tagged expressions are always done.
        XKey KTag x
         -> Left ResultDone


-------------------------------------------------------------------------------
-- | Step an application of a primitive operators to its arguments.
stepAppPrm
        :: (Ord p, Show p)
        => Config s p
        -> p -> [Exp s p]
        -> Either Result (Exp s p)

stepAppPrm config prim xsArgs
 = case Map.lookup prim (configPrims config) of
        Nothing         -> Left ResultDone
        Just primEval   -> stepPrim config primEval xsArgs


-------------------------------------------------------------------------------
-- | Step an application of an abstraction applied to its arguments.
stepAppAbs
        :: (Ord p, Show p)
        => Config s p
        -> [Param] -> Exp s p -> [Exp s p]
        -> Either Result (Exp s p)

stepAppAbs config psParam xBody xsArgs
 = let
        arity           = length psParam
        args            = length xsArgs
        xsArgs_sat      = take arity xsArgs
        xsArgs_remain   = drop arity xsArgs
        fsParam_sat     = map formOfParam psParam

   in case stepFirst config xsArgs_sat fsParam_sat of
        -- One of the args makes progress.
        Right xsArgs_sat'
         -> let xFun    = XAbs psParam xBody
            in  Right $ makeXApps (makeXApps xFun xsArgs_sat') xsArgs_remain

        -- Stepping one of the arguments failed.
        Left err@(ResultError _)
         -> Left err

        -- The arguments are all done.
        Left ResultDone
         -- Saturated application
         | args == arity
         -> let nsParam = map nameOfParam psParam
                snv     = snvOfNamesArgs nsParam xsArgs
            in  Right $ snvApply False snv xBody

         -- Under application.
         | args < arity
         -> let psParam_sat    = take args psParam
                nsParam_sat    = map nameOfParam psParam_sat
                psParam_remain = drop args psParam
                snv     = snvOfNamesArgs nsParam_sat xsArgs_sat
            in  Right $ XApp (snvApply False snv $ XAbs psParam_remain xBody)
                             xsArgs_remain

         -- Over application.
         | otherwise
         -> let nsParam = map nameOfParam psParam
                snv     = snvOfNamesArgs nsParam xsArgs_sat
            in  Right $ XApp (snvApply False snv xBody)
                             xsArgs_remain


-------------------------------------------------------------------------------
-- | Step an application of the ##seq super prim.
stepAppSeq
        :: (Ord p, Show p)
        => Exp s p -> [Exp s p]
        -> Either Result (Exp s p)

stepAppSeq xBody xsArgs
 -- Application of a seq to an abstraction.
 -- As we can see the abstraction, build the substitution directly without
 -- going through an intermediate application.
 | xArg1 : xsArgs' <- xsArgs
 , XAbs ps11  x12  <- fromMaybe xArg1 (pushHead xArg1)
 , p1    : ps11'   <- ps11
 = let  n1      = nameOfParam p1
        snv     = snvOfNamesArgs [n1] [xBody]
        car     = CSim snv
        cars    = [car]
   in   Right $ makeXApps (trainApply cars $ XAbs ps11' x12) xsArgs'

 -- Application of a seq to something that isn't yet an abstraction.
 | otherwise
 =      Right $ makeXApps (XKey KSeq xBody) xsArgs


-------------------------------------------------------------------------------
-- | Step an application of the ##tag superprim.
stepAppTag
        :: (Ord p, Show p)
        => Config s p
        -> Exp s p -> [Exp s p]
        -> Either Result (Exp s p)

stepAppTag config xBody xsArgs
 = case stepFirstVal config xsArgs of
        Left  res       -> Left res
        Right xsArgs'   -> Right $ makeXApps (XKey KTag xBody) xsArgs'


-------------------------------------------------------------------------------
-- | Step an application of a primitive operator to some arguments.
stepPrim
        :: (Ord p, Show p)
        => Config s p
        -> PrimEval s p -> [Exp s p]
        -> Either Result (Exp s p)
stepPrim config pe xsArgs
 | PrimEval prim desc csArg eval <- pe
 = let
        -- Evaluation of arguments is complete.
        evalArgs [] [] xsArgsDone
         = case eval (reverse xsArgsDone) of
                Just xResult    -> Right xResult
                Nothing         -> Left ResultDone

        -- We have more args than the primitive will accept.
        evalArgs [] xsArgsRemain xsArgsDone
         = case eval (reverse xsArgsDone) of
                Just xResult    -> Right $ XApp xResult (xsArgsRemain)
                Nothing         -> Left ResultDone

        -- Evaluate the next argument if needed.
        evalArgs (cArg' : csArg') (xArg' : xsArg') xsArgsDone
         -- Primitive does not demand a value fo rthis arg.
         | PExp <- cArg'
         = evalArgs csArg' xsArg' (xArg' : xsArgsDone)

         -- Primtiive demands a value for this arg.
         | otherwise
         = case step (config { configUnderLambdas = False
                             , configHeadArgs = False })
                     xArg' of
                Left err@(ResultError _)
                 -> Left err

                Left ResultDone
                 -> evalArgs csArg' xsArg' (xArg' : xsArgsDone)

                Right xArg''
                 -> Right $ makeXApps (XRef (RPrm (primEvalName pe)))
                          $ (reverse xsArgsDone) ++ (xArg'' : xsArg')

        -- We have less args than the prim will accept,
        -- so leave the application as it is.
        evalArgs _ [] xsArgsDone
         = Left ResultDone

   in   evalArgs csArg xsArgs []


-------------------------------------------------------------------------------
-- | Step the first available expression in a list,
--   reducing them all towards values.
stepFirstVal
        :: (Ord p, Show p)
        => Config s p
        -> [Exp s p]
        -> Either Result [Exp s p]

stepFirstVal config xx
 = stepFirst config xx (replicate (length xx) PVal)


-- | Step the first available expression in a list.
stepFirst
        :: (Ord p, Show p)
        => Config s p
        -> [Exp s p] -> [Form]
        -> Either Result [Exp s p]

stepFirst config xx ff
 = case (xx, ff) of
        ([], _)         -> Left ResultDone
        (_,  [])        -> Left ResultDone

        (x1 : xs2, f1 : fs2)
         | PExp <- f1
         -> case stepFirst config xs2 fs2 of
                Left r     -> Left r
                Right xs2' -> Right $ x1 : xs2'

         | otherwise
         -> case step config x1 of
                Left err@(ResultError{})
                 -> Left err

                Left ResultDone
                 -> case stepFirst config xs2 fs2 of
                        Left r     -> Left r
                        Right xs2' -> Right $ x1 : xs2'

                Right x1'
                 -> Right $ x1' : xs2

