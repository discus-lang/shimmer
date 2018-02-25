{-# LANGUAGE OverloadedStrings #-}
module SMR.Source.Pretty where
import SMR.Core.Exp.Base
import SMR.Prim.Name
import Data.Monoid
import Data.Text                                (Text)
import Data.Text.Lazy.Builder                   (Builder)
import qualified Data.Text.Lazy.Builder         as B
import qualified Data.Text.Lazy                 as L
import qualified Data.Text                      as T
import qualified Data.Char                      as Char
import qualified Numeric                        as Numeric


-- Class ----------------------------------------------------------------------
-- | Class of things that can be converted to text builders.
class Build a where
 build  :: a -> Builder

instance Build Text where
 build tx = B.fromText tx

instance Build Prim where
 build pp = buildPrim pp

instance (Build s, Build p) => Build (Decl s p) where
 build xx = buildDecl xx

instance (Build s, Build p) => Build (Exp s p) where
 build xx = buildExp CtxTop xx

instance (Build s, Build p) => Build (Ref s p) where
 build xx = buildRef xx


-- | Context we're currently in when pretty printing.
data Ctx
        = CtxTop        -- ^ Top level context.
        | CtxFun        -- ^ Functional expression in an an application.
        | CtxArg        -- ^ Argument expression in an application.
        deriving Show


-- | Wrap a thing in parenthesis.
parens :: Builder -> Builder
parens bb
 = "(" <> bb <> ")"


-- | Pretty print a thing as strict `Text`.
pretty :: Build a => a -> Text
pretty x
 = L.toStrict $ B.toLazyText $ build x


-- Decl -----------------------------------------------------------------------
-- | Yield a builder for a declaration.
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
-- | Yield a builder for an expression.
buildExp
        :: (Build s, Build p)
        => Ctx -> Exp s p -> Builder
buildExp ctx xx
 = case xx of
        XRef r    -> buildRef r

        XVar n 0  -> B.fromText n
        XVar n d  -> B.fromText n <> "^" <> B.fromString (show d)

        XKey k1 x2
         -> let ppExp   = buildKey k1 <> " " <> buildExp CtxArg x2
            in  case ctx of
                 CtxArg -> parens ppExp
                 _      -> ppExp

        XApp x1 []
         -> buildExp CtxFun x1

        XApp x1 xs2
         -> let ppExp   =  buildExp CtxFun x1 <> " " <> go xs2
                go []               = ""
                go (x : [])         = buildExp CtxArg x
                go (x11 : x21 : xs) = buildExp CtxArg x11 <> " " <> go (x21 : xs)
            in case ctx of
                CtxArg  -> parens ppExp
                _       -> ppExp

        XAbs vs x
         -> let go []        = "."
                go (p1 : []) = buildParam p1 <> "."
                go (p1 : ps) = buildParam p1 <> " " <> go ps
                ss           = "\\" <> go vs <> buildExp CtxTop x
            in  case ctx of
                 CtxArg -> parens ss
                 CtxFun -> parens ss
                 _      -> ss

        XSub train x
         |  length train == 0
         -> buildExp ctx x
         |  otherwise
         -> let ss     = buildTrain train <> "." <> buildExp CtxTop x
            in  case ctx of
                 CtxArg  -> parens ss
                 CtxFun  -> parens ss
                 _       -> ss


-- | Yield a builder for a parameter.
buildParam :: Param -> Builder
buildParam pp
 = case pp of
        PParam n PVal    -> B.fromText n
        PParam n PExp    -> "~" <> B.fromText n


-- | Yield a builder for a keyword.
buildKey :: Key -> Builder
buildKey kk
 = case kk of
        KBox    -> "##box"
        KRun    -> "##run"


-- Train ----------------------------------------------------------------------
-- | Yield a builder for a train.
buildTrain  :: (Build s, Build p) => Train s p -> Builder
buildTrain cs0
 = go cs0
 where  go []           = ""
        go (c : cs)     = go cs <> buildCar c


-- | Yield a builder for a train car.
buildCar :: (Build s, Build p) => Car s p -> Builder
buildCar cc
 = case cc of
        CSim snv        -> buildSnv snv
        CRec snv        -> "[" <> buildSnv snv <> "]"
        CUps ups        -> buildUps ups


-- Snv ------------------------------------------------------------------------
-- | Yield a builder for a substitution.
buildSnv  :: (Build s, Build p) => Snv s p -> Builder
buildSnv (SSnv vs)
 = "[" <> go (reverse vs) <> "]"
 where  go []   = ""
        go (b : [])     = buildSnvBind b
        go (b : bs)     = buildSnvBind b <> ", " <> go bs


-- | Yield a builder for a substitution binding.
buildSnvBind :: (Build s, Build p) => SnvBind s p -> Builder
buildSnvBind (BindVar name bump xx)
 | bump == 0
 = B.fromText name
 <> "=" <> buildExp CtxTop xx

 | otherwise
 =  B.fromText name <> "^" <> B.fromString (show bump)
 <> "=" <> buildExp CtxTop xx

buildSnvBind (BindNom ix xx)
 =  "?" <> B.fromString (show ix)
 <> "=" <> buildExp CtxTop xx


-- Ups ------------------------------------------------------------------------
-- | Yield a builder for an ups.
buildUps :: Ups -> Builder
buildUps (UUps vs)
 = "{" <> go (reverse vs) <> "}"
 where  go []   = ""
        go (b : [])     = buildUpsBump b
        go (b : bs)     = buildUpsBump b <> ", " <> go bs


-- | Yield a builder for an ups bump.
buildUpsBump :: UpsBump -> Builder
buildUpsBump ((name, bump), inc)
 | bump == 0
 = B.fromText name
 <> "=" <> B.fromString (show inc)

 | otherwise
 =  B.fromText name <> "^" <> B.fromString (show bump)
 <> "=" <> B.fromString (show inc)


-- Ref ------------------------------------------------------------------------
-- | Yield a builder for a reference.
buildRef :: (Build s, Build p) => Ref s p -> Builder
buildRef rr
 = case rr of
        RSym s  -> "%" <> build s
        RPrm p  -> "#" <> build p
        RTxt t  -> buildText t
        RMac n  -> "@" <> B.fromText n
        RSet n  -> "+" <> B.fromText n
        RNom i  -> "?" <> B.fromString (show i)


-- | Build a text string, escaping special chars in JSON style.
buildText :: Text -> Builder
buildText tx
 = (B.fromString $ ['"'] ++ escape (T.unpack tx) ++ ['"'])
 where  escape []               = []

        escape ('\\' : cs)      = '\\' : '\\' : escape cs
        escape ('\"' : cs)      = '\\' : '\"' : escape cs
        escape ('\b' : cs)      = '\\' : '\b' : escape cs
        escape ('\f' : cs)      = '\\' : '\f' : escape cs
        escape ('\n' : cs)      = '\\' : '\n' : escape cs
        escape ('\r' : cs)      = '\\' : '\r' : escape cs
        escape ('\t' : cs)      = '\\' : '\t' : escape cs

        escape (c : cs)
         | Char.ord c >= 32 && Char.ord c <= 126
         = c : escape cs

         | otherwise
         = let  s       = Numeric.showHex (Char.ord c) ""
                ss      = replicate (4 - length s) '0' ++ s
           in   "\\u" ++ ss ++ escape cs


-- Prim -----------------------------------------------------------------------
-- | Yield a builder for a primitive.
buildPrim :: Prim -> Builder
buildPrim pp
 = B.fromText $ pprPrim pp

