
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
        (POPrim "list'cons")
        "add an element to the front of a list"
        fn'
 where
        fn' _world [v, VList vs]
         = return $ Just [VList (v : vs)]
        fn' _world _
         = return $ Nothing


-- | Split an element from the front of a list.
primOpListUncons :: PrimEval w
primOpListUncons
 = PrimEval
        (POPrim "list'uncons")
        "split an element from the front of a list"
        fn'
 where
        fn' _world [VList (v : vs)]
         = return $ Just [v, VList vs]
        fn' _world _
         = return $ Nothing


-- | Append two lists.
primOpListAppend :: PrimEval w
primOpListAppend
 = PrimEval
        (POPrim "list'append")
        "append two lists"
        fn'
 where
        fn' _world [VList vs1, VList vs2]
         = return $ Just [VList (vs1 ++ vs2)]
        fn' _world _
         = return $ Nothing

