
-- Train ----------------------------------------------------------------------
-- | Yield a builder for a train.
buildTrain  :: (Build s, Build p) => Train s p -> Builder
buildTrain cs0
 = go cs0
 where  go []           = ""
        go (c : cs)     = go cs <> buildCar c


-- | Yield a builder for a train car.
buildCar :: (Build s, Build p) => Car s p -> Builder
buildCar cc
 = case cc of
        CSim snv        -> buildSnv snv
        CRec snv        -> "[" <> buildSnv snv <> "]"
        CUps ups        -> buildUps ups


-- Snv ------------------------------------------------------------------------
-- | Yield a builder for a substitution.
buildSnv  :: (Build s, Build p) => Snv s p -> Builder
buildSnv (SSnv vs)
 = "[" <> go (reverse vs) <> "]"
 where  go []   = ""
        go (b : [])     = buildSnvBind b
        go (b : bs)     = buildSnvBind b <> ", " <> go bs


-- | Yield a builder for a substitution binding.
buildSnvBind :: (Build s, Build p) => SnvBind s p -> Builder
buildSnvBind (BindVar name bump xx)
 | bump == 0
 = B.fromText name
 <> "=" <> buildExp CtxTop xx

 | otherwise
 =  B.fromText name <> "^" <> B.fromString (show bump)
 <> "=" <> buildExp CtxTop xx

buildSnvBind (BindNom ix xx)
 =  "?" <> B.fromString (show ix)
 <> "=" <> buildExp CtxTop xx


-- Ups ------------------------------------------------------------------------
-- | Yield a builder for an ups.
buildUps :: Ups -> Builder
buildUps (UUps vs)
 = "{" <> go (reverse vs) <> "}"
 where  go []   = ""
        go (b : [])     = buildUpsBump b
        go (b : bs)     = buildUpsBump b <> ", " <> go bs


-- | Yield a builder for an ups bump.
buildUpsBump :: UpsBump -> Builder
buildUpsBump ((name, bump), inc)
 | bump == 0
 = B.fromText name
 <> "=" <> B.fromString (show inc)

 | otherwise
 =  B.fromText name <> "^" <> B.fromString (show bump)
 <> "=" <> B.fromString (show inc)


{-
        XSub train x
         |  length train == 0
         -> buildExp ctx x
         |  otherwise
         -> let ss     = buildTrain train <> "." <> buildExp CtxTop x
            in  case ctx of
                 CtxArg  -> parens ss
                 CtxFun  -> parens ss
                 _       -> ss
-}


