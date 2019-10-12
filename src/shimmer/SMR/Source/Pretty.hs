
module SMR.Source.Pretty where
import SMR.Source.Prim
import SMR.Core.Exp.Base
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

{-}
instance Build Prim where
 build pp = buildPrim pp
-}
instance Build Decl where
 build xx = buildDecl xx

instance Build Exp where
 build xx = buildExp CtxTop xx

instance Build Ref where
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
buildDecl :: Decl -> Builder
buildDecl dd
 = case dd of
        DeclMac n xx
         -> "@" <> B.fromText n <> " = " <> buildExp CtxTop xx <> ";\n"

        DeclSet n xx
         -> "+" <> B.fromText n <> " = " <> buildExp CtxTop xx <> ";\n"


-- Exp ------------------------------------------------------------------------
-- | Yield a builder for an expression.
buildExp :: Ctx -> Exp -> Builder
buildExp ctx xx
 = case xx of
        XRef r    -> buildRef r

        XVar n 0  -> B.fromText n
        XVar n d  -> B.fromText n <> "^" <> B.fromString (show d)

        XKey k1 xx
         -> let go []           = "]"
                go (x : xs)     = buildExp CtxTop x <> ", " <> go xs
            in  buildKey k1 <> " " <> "[" <> go xx

{-
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
-}
        XAbs vs x
         -> let go []        = "."
                go (n : [])  = B.fromText n <> "."
                go (n : ns)  = B.fromText n <> " " <> go ns
                ss           = "\\" <> go vs <> buildExp CtxTop x
            in  case ctx of
                 CtxArg -> parens ss
                 CtxFun -> parens ss
                 _      -> ss


-- | Yield a builder for a keyword.
buildKey :: Key -> Builder
buildKey kk
 = case kk of
        KBox    -> "##box"
        KRun    -> "##run"


-- Ref ------------------------------------------------------------------------
-- | Yield a builder for a reference.
buildRef :: Ref -> Builder
buildRef rr
 = case rr of
        RSym s  -> "%" <> build s
        RTxt t  -> buildText t
        RMac n  -> "@" <> B.fromText n
        RSet n  -> "+" <> B.fromText n
        RNom i  -> "?" <> B.fromString (show i)
        RVal v  -> buildVal v

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
buildVal :: Val -> Builder
buildVal vv
 = B.fromText $ pprVal vv

