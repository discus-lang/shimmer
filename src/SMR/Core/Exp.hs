
module SMR.Core.Exp
        ( -- * Abstract Syntax
          Decl  (..)
        , Exp   (..)
        , Param (..)
        , Form  (..)
        , Key   (..)
        , Train
        , Car   (..)
        , Snv   (..), SnvBind(..)
        , Ups   (..), UpsBump
        , Ref   (..)
        , Name
        , Text

         -- * Compounds
        , makeXApps, takeXApps
        , makeXAbs
        , nameOfParam, formOfParam

         -- * Substitution Trains
        , trainCons
        , trainAppend
        , trainApply
        , snvApply
        , snvOfNamesArgs

        -- * Substitution Pushing
        , pushHead
        , pushDeep)
where
import SMR.Core.Exp.Base
import SMR.Core.Exp.Compounds
import SMR.Core.Exp.Train
import SMR.Core.Exp.Push
import Data.Text                (Text)

