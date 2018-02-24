{-# LANGUAGE BangPatterns #-}
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
        = XRef  !(Ref s p)

        -- | Keyed expressions.
        | XKey  !Key !(Exp s p)

        -- | Application of a function expression to an argument.
        | XApp  !(Exp s p) ![Exp s p]

        -- | Variable name with a depth counter.
        | XVar  !Name !Depth

        -- | Abstraction with a list of parameters and a body expression.
        | XAbs  ![Param] !(Exp s p)

        -- | Substitution train applied to an expression.
        --   The train car at the head of the list is applied first.
        | XSub  !(Train s p) !(Exp s p)
        deriving Show


-- | Substitution train.
type Train s p
        = [Car s p]


-- | Function parameter.
data Param
        = PParam !Name !Form
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
        -- | Delay evaluation of an expression used as the argument
        --   of a call-by-value function application.
        = KBox

        -- | Run a boxed expression.
        | KRun

        deriving Show


-- | A car on the substitution train,
--   parameterised by the types used for symbols and primitives.
data Car s p
        -- | Simultaneous subsitution.
        = CSim  !(Snv s p)

        -- | Recursive substitution.
        | CRec  !(Snv s p)

        -- | Lifting.
        | CUps  !Ups
        deriving Show


-- | Explicit substitution map,
--   parameterised by the types used for symbols and primitives.
data Snv s p
        = SSnv ![SnvBind s p]
        deriving Show

data SnvBind s p
        = BindVar !Name !Depth !(Exp s p)
        | BindNom !Nom         !(Exp s p)
        deriving Show


-- | Lifting indicator,
--   mapping name and binding depth to number of levels to lift.
data Ups
        = UUps ![UpsBump]
        deriving Show


-- | Indicates how to bump the index on a variable.
type UpsBump
        = ((Name, Depth), Bump)


-- | Binding depth indicator.
type Depth = Integer


-- | Bump index indicator.
type Bump  = Integer


-- | A reference to some external thing.
data Ref s p
        -- | An uninterpreted symbol.
        = RSym  !s

        -- | A primitive value.
        | RPrm  !p

        -- | A text string.
        | RTxt  !Text

        -- | A macro name.
        | RMac  !Name

        -- | A set name.
        | RSet  !Name

        -- | A nominal variable.
        | RNom  !Nom
        deriving Show


-- | Generic names for things.
type Name = Text


-- | Index of a nominal constant.
type Nom = Integer

