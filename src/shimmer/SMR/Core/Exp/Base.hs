-- | The Shimmer Abstract Syntax Tree (AST)
module SMR.Core.Exp.Base where
import Data.Text        (Text)
import Data.Int
import Data.Word


-- | Top-level declaration,
--   parameterised by the types of symbols and primitives.
data Decl
        = DeclMac Name Exp              -- ^ Macro declaration.
        | DeclSet Name Exp              -- ^ Element of a declaration set.
        deriving (Eq, Show)


-- | Expression parameterized by the type for primitives.
data Exp
        = XRef  Ref                     -- ^ External reference.
        | XVar  Name    Depth           -- ^ Variable name with bump.
        | XAbs  [Name]  Exp             -- ^ Abstraction with parameter names and body.
        | XLet  [Name]  [Exp] Exp       -- ^ Non-recursive bindings.
        | XRec  [Name]  [Exp] Exp       -- ^ Recursive bindings.
        | XKey  Key     [Exp]           -- ^ Keyword invocation.
        deriving (Eq, Show)


-- | Expression keys (super primitives)
data Key
        = KApv                          -- ^ Application to vector of arguments.
        | KAps                          -- ^ Application to term producing a vector.
        | KBox                          -- ^ Box an expression, delaying evaluation.
        | KRun                          -- ^ Run an expression, forcing evaluation.
        | KPrm Prim                     -- ^ Primitive application.
        deriving (Eq, Show)


-- | A reference to some external thing.
data Ref
        = RSym  Text                    -- ^ Uninterpreted symbol.
        | RTxt  Text                    -- ^ Text string.
        | RMac  Name                    -- ^ Macro name.
        | RSet  Name                    -- ^ Set name.
        | RNom  Nom                     -- ^ Nominal constant.
        | RVal  Val                     -- ^ Value
        deriving (Eq, Show)


-- | Generic names for things.
type Name  = Text

-- | Index of a nominal constant.
type Nom   = Integer

-- | Binding depth.
type Depth = Integer


-- | Primitive operators.
data Prim
        = PrimList
        | PrimOp        Text
        deriving (Eq, Ord, Show)


-- | Primitive values.
data Val
        = VUnit
        | VBool         Bool

        | VNat          Integer
        | VInt          Integer

        | VWord8        Word8
        | VWord16       Word16
        | VWord32       Word32
        | VWord64       Word64

        | VInt8         Int8
        | VInt16        Int16
        | VInt32        Int32
        | VInt64        Int64

        | VFloat32      Float
        | VFloat64      Double

        | VList         [Val]
        deriving (Eq, Ord, Show)

