
-- | The Shimmer Abstract Syntax Tree (AST)
module SMR.Core.Exp.Base where
import Data.Text                (Text)


-- | Top-level declaration,
--   parameterised by the types of symbols and primitives.
data Decl s p
        = DeclMac Name (Exp s p)
        | DeclSet Name (Exp s p)
        deriving Show


-- | Expression,
--   parameterised by the types of symbols and primitives
data Exp s p
        -- | Reference to an external thing.
        = XRef  (Ref s p)

        -- | Keyed expressions.
        | XKey  Key       (Exp s p)

        -- | Application of a function expression to an argument.
        | XApp  (Exp s p) [Exp s p]

        -- | Variable name with a binding depth.
        | XVar  Name Int

        -- | Abstraction with a list of parameters and a body expression.
        | XAbs  [Param] (Exp s p)

        -- | Substitution train applied to an expression.
        --   The train car at the head of the list is applied first.
        | XSub  (Train s p) (Exp s p)
        deriving Show


-- | Substitution train.
type Train s p
        = [Car s p]


-- | Function parameter.
data Param
        = PParam Name Form
        deriving Show


-- | Form of argument required in application.
data Form
        -- | Value for call-by-value.
        = PVal

        -- | Expression for call-by-name
        | PExp
        deriving Show


-- | Expression keys (super primitives)
data Key
        -- | Tagged expression is never evaluated.
        = KTag

        -- | Delay evaluation of an expression used as the argument
        --   of a call-by-value function application.
        | KBox

        -- | Run a boxed expression.
        | KRun

        -- | Sequence evaluation.
        | KSeq
        deriving Show


-- | A car on the substitution train,
--   parameterised by the types used for symbols and primitives.
data Car s p
        -- | Simultaneous subsitution.
        = CSim  (Snv s p)

        -- | Recursive substitution.
        | CRec  (Snv s p)

        -- | Lifting.
        | CUps  Ups
        deriving Show


-- | Explicit substitution map,
--   parameterised by the types used for symbols and primitives.
data Snv s p
        = SSnv [SnvBind s p]
        deriving Show

type SnvBind s p
        = ((Name, Int), Exp s p)


-- | Lifting indicator,
--   mapping name and binding depth to number of levels to lift.
data Ups
        = UUps [UpsBump]
        deriving Show

type UpsBump
        = ((Name, Int), Int)


-- | A reference to some external thing.
data Ref s p
        -- | An uninterpreted symbol.
        = RSym  s

        -- | A primitive value.
        | RPrm  p

        -- | A macro name.
        | RMac  Name

        -- | A set name.
        | RSet  Name

        -- | A nominal variable.
        | RNom  Integer
        deriving Show


-- | Generic names for things.
type Name = Text

