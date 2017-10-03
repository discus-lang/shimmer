{-# LANGUAGE OverloadedStrings #-}
module SMR.Source.Pretty where
import SMR.Core.Exp.Base
import Data.Monoid
import Data.Text                                (Text)
import Data.Text.Lazy.Builder                   (Builder)
import qualified Data.Text.Lazy.Builder         as B


-- Class ----------------------------------------------------------------------
class Build a where
 build  :: a -> Builder

instance Build Text where
 build tx = B.fromText tx


parens :: Builder -> Builder
parens bb
 = "(" <> bb <> ")"


-- Decl -----------------------------------------------------------------------
buildDecl
        :: (Build s, Build p)
        => Decl s p -> Builder
buildDecl dd
 = case dd of
        DeclMac n xx
         -> "@" <> B.fromText n <> " = " <> buildExp CtxTop xx <> ";\n"

        DeclSet n xx
         -> "+" <> B.fromText n <> " = " <> buildExp CtxTop xx <> ";\n"


-- Exp ------------------------------------------------------------------------
data Ctx
        = CtxTop        -- ^ Top level context.
        | CtxFun        -- ^ Functional expression in an an application.
        | CtxArg        -- ^ Argument expression in an application.
        deriving Show


buildExp
        :: (Build s, Build p)
        => Ctx -> Exp s p -> Builder
buildExp ctx xx
 = case xx of
        XRef r    -> buildRef r

        XVar n 0  -> B.fromText n
        XVar n d  -> B.fromText n <> "^" <> B.fromString (show d)

        XKey k1 xs2
         -> let ppExp   = buildKey k1 <> " " <> go xs2
                go []             = ""
                go (x : [])       = buildExp CtxArg x
                go (x1 : x2 : xs) = buildExp CtxArg x1 <> " " <> go (x2 : xs)
            in  case ctx of
                 CtxArg -> parens ppExp
                 _      -> ppExp

        XApp x1 xs2
         -> let ppExp   =  buildExp CtxFun x1 <> " " <> go xs2
                go []             = ""
                go (x : [])       = buildExp CtxArg x
                go (x1 : x2 : xs) = buildExp CtxArg x1 <> " " <> go (x2 : xs)
            in case ctx of
                CtxArg  -> parens ppExp
                _       -> ppExp

        XAbs vs x
         -> let go []        = "."
                go (p1 : []) = buildParam p1 <> "."
                go (p1 : ps) = buildParam p1 <> " " <> go ps
                exp          = "\\" <> go vs <> buildExp CtxTop x
            in  case ctx of
                 CtxArg -> parens exp
                 CtxFun -> parens exp
                 _      -> exp

        XSub train x
         |  length train == 0
         -> buildExp ctx x
         |  otherwise
         -> let exp     = buildTrain train <> "." <> buildExp CtxTop x
            in  case ctx of
                 CtxArg  -> parens exp
                 CtxFun  -> parens exp
                 _       -> exp


buildParam :: Param -> Builder
buildParam pp
 = case pp of
        PParam n PVal    -> B.fromText n
        PParam n PExp    -> "~" <> B.fromText n


buildKey :: Key -> Builder
buildKey kk
 = case kk of
        KBox    -> "##box"
        KRun    -> "##run"
        KSeq    -> "##seq"
        KTag    -> "##tag"


-- Train ----------------------------------------------------------------------
buildTrain  :: (Build s, Build p) => Train s p -> Builder
buildTrain cs
 = go cs
 where  go []           = ""
        go (c : cs)     = go cs <> buildCar c


buildCar :: (Build s, Build p) => Car s p -> Builder
buildCar cc
 = case cc of
        CSim snv        -> buildSnv snv
        CRec snv        -> "[" <> buildSnv snv <> "]"
        CUps ups        -> buildUps ups


-- Snv ------------------------------------------------------------------------
buildSnv  :: (Build s, Build p) => Snv s p -> Builder
buildSnv (SSnv vs)
 = "[" <> go (reverse vs) <> "]"
 where  go []   = ""
        go (b : [])     = buildSnvBind b
        go (b : bs)     = buildSnvBind b <> ", " <> go bs


buildSnvBind :: (Build s, Build p) => SnvBind s p -> Builder
buildSnvBind ((name, bump), xx)
 | bump == 0
 = B.fromText name
 <> "=" <> buildExp CtxTop xx

 | otherwise
 =  B.fromText name <> "^" <> B.fromString (show bump)
 <> "=" <> buildExp CtxTop xx


-- Ups ------------------------------------------------------------------------
buildUps :: Ups -> Builder
buildUps (UUps vs)
 = "{" <> go (reverse vs) <> "}"
 where  go []   = ""
        go (b : [])     = buildUpsBump b
        go (b : bs)     = buildUpsBump b <> ", " <> go bs


buildUpsBump :: UpsBump -> Builder
buildUpsBump ((name, bump), inc)
 | bump == 0
 = B.fromText name
 <> "=" <> B.fromString (show inc)

 | otherwise
 =  B.fromText name <> "^" <> B.fromString (show bump)
 <> "=" <> B.fromString (show inc)


-- Ref ------------------------------------------------------------------------
buildRef :: (Build s, Build p) => Ref s p -> Builder
buildRef rr
 = case rr of
        RMac n  -> "@" <> B.fromText n
        RSet n  -> "+" <> B.fromText n
        RSym s  -> "%" <> build s
        RPrm p  -> "#" <> build p

