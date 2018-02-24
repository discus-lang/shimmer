{-# LANGUAGE OverloadedStrings #-}
module SMR.Prim.Op.List where
import SMR.Core.Exp
import SMR.Prim.Op.Base


-- | Primitive evaluators for list operators.
primOpsList :: [PrimEval s Prim w]
primOpsList
 = [ primOpListCons,    primOpListUncons
   , primOpListSnoc,    primOpListUnsnoc
   , primOpListAppend ]


-- | Cons an element to a the front of a list.
primOpListCons :: PrimEval s Prim w
primOpListCons
 = PrimEval
        (PrimOp "list-cons")
        "add an element to the front of a list"
        [PExp, PVal] fn'
 where
        fn' _world as0
         | Just (x1, as1) <- takeArgExp as0
         , Just (XApp tag@(XRef (RPrm PrimTagList)) xs, [])
                          <- takeArgExp as1
         = return $ Just $ XApp tag (x1 : xs)

        fn' _world _
         = return $ Nothing


-- | Split an element from the front of a list.
primOpListUncons :: PrimEval s Prim w
primOpListUncons
 = PrimEval
        (PrimOp "list-uncons")
        "split an element from the front of a list"
        [PVal, PExp] fn'
 where
        fn' _world as0
         | Just (XApp tag@(XRef (RPrm PrimTagList)) xx, as1)
                          <- takeArgExp as0
         , Just (x2, [])  <- takeArgExp as1
         = case xx of
                x1 : xs -> return $ Just $ XApp x2 [x1, XApp tag xs]
                []      -> return $ Nothing
        fn' _world _
         = return $ Nothing


-- | Snoc an element to a the end of a list.
primOpListSnoc :: PrimEval s Prim w
primOpListSnoc
 = PrimEval
        (PrimOp "list-snoc")
        "add an element to the end of a list"
        [PVal, PExp] fn'
 where
        fn' _world as0
         | Just (XApp tag@(XRef (RPrm PrimTagList)) xs, as1)
                          <- takeArgExp as0
         , Just (x1, [])  <- takeArgExp as1
         = return $ Just $ XApp tag (xs ++ [x1])
        fn' _world _
         = return $ Nothing


-- | Unsnoc an element from the end of a list.
primOpListUnsnoc :: PrimEval s Prim w
primOpListUnsnoc
 = PrimEval
        (PrimOp "list-unsnoc")
        "split an element from the end of a list"
        [PVal, PExp] fn'
 where
        fn' _world as0
         | Just (XApp tag@(XRef (RPrm PrimTagList)) xx, as1)
                          <- takeArgExp as0
         , Just (x2, [])  <- takeArgExp as1
         = case reverse xx of
                x1 : xs -> return $ Just $ XApp x2 [XApp tag (reverse xs), x1]
                []      -> return $ Nothing

        fn' _world _
         = return $ Nothing


-- | Append two lists.
primOpListAppend :: PrimEval s Prim w
primOpListAppend
 = PrimEval
        (PrimOp "list-append")
        "append two lists"
        [PVal, PVal] fn'
 where
        fn' _world as0
         | Just (XApp (XRef (RPrm PrimTagList)) xs1, as1)
                          <- takeArgExp as0
         , Just (XApp tag@(XRef (RPrm PrimTagList)) xs2, [])
                          <- takeArgExp as1
         = return $ Just (XApp tag (xs1 ++ xs2))

        fn' _world _
         = return $ Nothing

