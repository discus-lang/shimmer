-- | The Shimmer Abstract Syntax Tree (AST)
module SMR.Core.Exp.Base where
import Data.Text        (Text)
import Data.Int
import Data.Word
import Data.Map         (Map)


-- | Top-level declaration,
--   parameterised by the types of symbols and primitives.
data Decl
        = DeclMac Name Exp              -- ^ Macro declaration.
        | DeclSet Name Exp              -- ^ Element of a declaration set.
        deriving (Eq, Show)


-- | Expression parameterized by the type for primitives.
data Exp
        = XVal  Val                     -- ^ Value.
        | XMac  Name                    -- ^ Macro name.
        | XVar  Name    Depth           -- ^ Variable name with bump.
        | XAbs  [Name]  Exp             -- ^ Abstraction with parameter names and body.
        | XLet  [Name]  [Exp] Exp       -- ^ Non-recursive bindings.
        | XRec  [Name]  [Exp] Exp       -- ^ Recursive bindings.
        | XKey  Key     [Exp]           -- ^ Keyword invocation.
        deriving (Eq, Show)


-- | Expression keys (super primitives)
data Key
        = KVec                          -- ^ Vector formation.
        | KApp                          -- ^ Function application.
        | KPrm PrimOp                   -- ^ Primitive operator application.
        deriving (Eq, Show)


-- | A reference to some external thing.
data Ref
        = RSym  Text                    -- ^ Uninterpreted symbol.
        | RTxt  Text                    -- ^ Text string.
        | RSet  Name                    -- ^ Set name.
        | RNom  Nom                     -- ^ Nominal constant.
        deriving (Eq, Show)


-- | Generic names for things.
type Name  = Text

-- | Index of a nominal constant.
type Nom   = Integer

-- | Binding depth.
type Depth = Integer

-- | Environments.
type Env = Map Name Val

-- | Values
data Val
        = VRef          Ref
        | VPrim         PrimVal
        | VList         [Val]
        | VClo          [Env] [Name] Exp
        deriving (Eq, Show)


-- | Primitive operators.
data PrimOp
        = POList                         -- ^ List constructor.
        | POPrim        Text            -- ^ Primitive operator.
        deriving (Eq, Ord, Show)


-- | Primitive values.
data PrimVal
        = PVUnit
        | PVBool        Bool

        | PVNat         Integer
        | PVInt         Integer

        | PVWord8       Word8
        | PVWord16      Word16
        | PVWord32      Word32
        | PVWord64      Word64

        | PVInt8        Int8
        | PVInt16       Int16
        | PVInt32       Int32
        | PVInt64       Int64

        | PVFloat32     Float
        | PVFloat64     Double
        deriving (Eq, Show)

