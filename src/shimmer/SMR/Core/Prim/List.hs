
module SMR.Core.Prim.List where
import SMR.Core.Prim.Base
import SMR.Core.Exp


-- | Primitive evaluators for list operators.
primOpsList :: [PrimEval w]
primOpsList
 = [ primOpListCons,    primOpListUncons
   , primOpListAppend ]


-- | Cons an element to a the front of a list.
primOpListCons :: PrimEval w
primOpListCons
 = PrimEval
        (POp "list-cons")
        "add an element to the front of a list"
        fn'
 where
        fn' _world [XVal v, XLIST vs]
         = return $ Just $ XLIST (v : vs)
        fn' _world _
         = return $ Nothing


-- | Split an element from the front of a list.
primOpListUncons :: PrimEval w
primOpListUncons
 = PrimEval
        (POp "list-uncons")
        "split an element from the front of a list"
        fn'
 where
        fn' _world [XLIST (v : vs), x2]
         = return $ Just $ XApv x2 [XVal v, XLIST vs]
        fn' _world _
         = return $ Nothing


-- | Append two lists.
primOpListAppend :: PrimEval w
primOpListAppend
 = PrimEval
        (POp "list-append")
        "append two lists"
        fn'
 where
        fn' _world [XLIST vs1, XLIST vs2]
         = return $ Just (XLIST (vs1 ++ vs2))
        fn' _world _
         = return $ Nothing

