
module SMR.Source.Pretty where
import SMR.Source.Prim
import SMR.Core.Exp.Patterns
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
        XVal v    -> B.fromText $ pprVal v

        XMac n    -> B.fromText $ "@" <> n

        XVar n 0  -> B.fromText n
        XVar n d  -> B.fromText n <> "^" <> B.fromString (show d)

        XAbs vs x
         -> let go []        = "} "
                go (n : [])  = B.fromText n <> "} "
                go (n : ns)  = B.fromText n <> " " <> go ns
                ss           = "{" <> go vs <> buildExp CtxTop x
            in  case ctx of
                 CtxArg -> parens ss
                 CtxFun -> parens ss
                 _      -> ss

        XVec xs
         -> let go []           = "]"
                go (x : [])     = buildExp CtxTop x <> "]"
                go (x : xs)     = buildExp CtxTop x <> ", " <> go xs
            in  "[" <> go xs

        XApp xFun xArg
         -> let ppExp   =  buildExp CtxFun xFun <> " "
                        <> buildExp CtxArg xArg
            in  case ctx of
                 CtxArg -> parens ppExp
                 _      -> ppExp

        XPrm p xArg
         -> let ppExp   =  "#" <> B.fromText (pprPrimOp p) <> " "
                        <> buildExp CtxArg xArg
            in  case ctx of
                 CtxArg -> parens ppExp
                 _      -> ppExp

        XKey k1 xx
         -> let go []           = "]"
                go (x : [])     = buildExp CtxTop x <> "]"
                go (x : xs)     = buildExp CtxTop x <> ", " <> go xs
            in  buildKey k1 <> " " <> "[" <> go xx


-- | Yield a builder for a keyword.
buildKey :: Key -> Builder
buildKey kk
 = case kk of
        KVec    -> "##vec"
        KApp    -> "##app"
        KPrm p  -> "##prm" <> " #" <> B.fromText (pprPrimOp p)


-- Ref ------------------------------------------------------------------------
-- | Yield a builder for a reference.
pprRef :: Ref -> Text
pprRef  rr
 = case rr of
        RSym s  -> "%" <> s
        RTxt t  -> t
        RSet n  -> "+" <> n
        RNom i  -> "?" <> T.pack (show i)


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
 = "#" <> (B.fromText $ pprVal vv)


-- Val ------------------------------------------------------------------------
-- | Pretty print a primitive value.
pprVal :: Val -> Text
pprVal vv
 = case vv of
        VRef r          -> pprRef r
        VPrim p         -> pprPrimVal p
        VList vs        -> "[list|" <> T.intercalate "," (map pprVal vs) <> "]"


-- PrimOp ---------------------------------------------------------------------
-- | Pretty print a primitive operator.
pprPrimOp :: PrimOp -> Text
pprPrimOp po
 = case po of
        POList          -> "list"
        POPrim op       -> op


-- PrimVal --------------------------------------------------------------------
-- | Pretty print a primitive value.
pprPrimVal :: PrimVal -> Text
pprPrimVal pv
 = case pv of
        PVUnit          -> T.pack "unit"

        PVBool True     -> T.pack "true"
        PVBool False    -> T.pack "false"

        PVNat n         -> T.pack $ "nat'" ++ show n
        PVInt i         -> T.pack $ "int'" ++ show i

        PVWord8  w      -> T.pack $ "w8'"  ++ Numeric.showHex w ""
        PVWord16 w      -> T.pack $ "w16'" ++ Numeric.showHex w ""
        PVWord32 w      -> T.pack $ "w32'" ++ Numeric.showHex w ""
        PVWord64 w      -> T.pack $ "w64'" ++ Numeric.showHex w ""

        PVInt8   i      -> T.pack $ "i8'"  ++ show i
        PVInt16  i      -> T.pack $ "i16'" ++ show i
        PVInt32  i      -> T.pack $ "i32'" ++ show i
        PVInt64  i      -> T.pack $ "i64'" ++ show i

        PVFloat32 f     -> T.pack $ "f32'" ++ show f
        PVFloat64 f     -> T.pack $ "f64'" ++ show f
