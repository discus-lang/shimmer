{-# LANGUAGE OverloadedStrings #-}
module SMR.Prim.Op.List where
import SMR.Core.Exp
import SMR.Prim.Op.Base


-- | Primitive evaluators for list operators.
primOpsList :: [PrimEval s Prim]
primOpsList
 = [ primOpListCons,    primOpListUncons
   , primOpListSnoc,    primOpListUnsnoc
   , primOpListAppend ]


-- | Cons an element to a the front of a list.
primOpListCons :: PrimEval s Prim
primOpListCons
 = PrimEval
        (PrimOp "list-cons")
        "add an element to the front of a list"
        [PExp, PVal] fn'
 where
        fn' as0
         | Just (x1, as1) <- takeArgExp as0
         , Just (XApp tag@(XRef (RPrm PrimTagList)) xs, [])
                          <- takeArgExp as1
         = Just $ XApp tag (x1 : xs)
        fn' _ = Nothing


-- | Split an element from the front of a list.
primOpListUncons :: PrimEval s Prim
primOpListUncons
 = PrimEval
        (PrimOp "list-uncons")
        "split an element from the front of a list"
        [PVal, PExp, PExp] fn'
 where
        fn' as0
         | Just (XApp tag@(XRef (RPrm PrimTagList)) xx, as1)
                          <- takeArgExp as0
         , Just (x2, as2) <- takeArgExp as1
         , Just (x3, [])  <- takeArgExp as2
         = case xx of
                x1 : xs -> Just $ XApp x2 [x1, XApp tag xs]
                []      -> Just $ x3
        fn' _ = Nothing


-- | Snoc an element to a the end of a list.
primOpListSnoc :: PrimEval s Prim
primOpListSnoc
 = PrimEval
        (PrimOp "list-snoc")
        "add an element to the end of a list"
        [PVal, PExp] fn'
 where
        fn' as0
         | Just (XApp tag@(XRef (RPrm PrimTagList)) xs, as1)
                          <- takeArgExp as0
         , Just (x1, [])  <- takeArgExp as1
         = Just $ XApp tag (xs ++ [x1])
        fn' _ = Nothing


-- | Unsnoc an element from the end of a list.
primOpListUnsnoc :: PrimEval s Prim
primOpListUnsnoc
 = PrimEval
        (PrimOp "list-unsnoc")
        "split an element from the end of a list"
        [PVal, PExp, PExp] fn'
 where
        fn' as0
         | Just (XApp tag@(XRef (RPrm PrimTagList)) xx, as1)
                          <- takeArgExp as0
         , Just (x2, as2) <- takeArgExp as1
         , Just (x3, [])  <- takeArgExp as2
         = case reverse xx of
                x1 : xs -> Just $ XApp x2 [x1, XApp tag (reverse xs)]
                []      -> Just $ x3
        fn' _ = Nothing


-- | Append two lists.
primOpListAppend :: PrimEval s Prim
primOpListAppend
 = PrimEval
        (PrimOp "list-append")
        "append two lists"
        [PVal, PVal] fn'
 where
        fn' as0
         | Just (XApp (XRef (RPrm PrimTagList)) xs1, as1)
                          <- takeArgExp as0
         , Just (XApp tag@(XRef (RPrm PrimTagList)) xs2, [])
                          <- takeArgExp as1
         = Just (XApp tag (xs1 ++ xs2))
        fn' _ = Nothing

