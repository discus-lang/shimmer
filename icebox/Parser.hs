
-- Ups ------------------------------------------------------------------------
-- | Parser for an ups.
--
-- @
-- Ups  ::= '{' Bump*, '}'
-- @
--
pUps :: Parser s p Ups
pUps
 = do   _       <- pPunc '{'
        bs      <- P.sepBy pBump (pPunc ',')
        _       <- pPunc '}'
        return  $ UUps (reverse bs)


-- | Parser for a bump.
--
-- @
-- Bump ::= Name ':' Nat
--       |  Name '^' Nat ':' Nat
-- @
pBump :: Parser s p UpsBump
pBump
 = do   name    <- pNameOfSpace SVar
        P.alt
         (do    _       <- pPunc ':'
                inc     <- pNat
                return  ((name, 0), inc))

         (do    _       <- pPunc '^'
                depth   <- pNat
                _       <- pPunc ':'
                inc     <- pNat
                return  ((name, depth), inc))

-- Train ----------------------------------------------------------------------
-- | Parser for a substitution train.
--   The cars are produced in reverse order.
pTrain  :: Config s p -> Parser s p [Car s p]
pTrain c
 = do   cCar    <- pTrainCar c
        P.alt
         (do csCar <- pTrain c
             return $ cCar : csCar)
         (do return $ cCar : [])


-- | Parse a single car in the train.
pTrainCar :: Config s p -> Parser s p (Car s p)
pTrainCar c
 = P.alt
        -- Substitution, both simultaneous and recursive
    (do car     <- pCarSimRec c
        return car)

    (do -- An ups car.
        ups     <- pUps
        return (CUps ups))


-- Snv ------------------------------------------------------------------------
-- | Parser for a substitution environment.
--
-- @
-- Snv   ::= '[' Bind*, ']'
-- @
--
pCarSimRec :: Config s p -> Parser s p (Car s p)
pCarSimRec c
 = do   _       <- pPunc '['

        P.alt   -- Recursive substitution.
         (do    _       <- pPunc '['
                bs      <- P.sepBy (pBind c) (pPunc ',')
                _       <- pPunc ']'
                _       <- pPunc ']'
                return  $ CRec (SSnv (reverse bs)))

                -- Simultaneous substitution.
         (do    bs      <- P.sepBy (pBind c) (pPunc ',')
                _       <- pPunc ']'
                return  $ CSim (SSnv (reverse bs)))


-- | Parser for a binding.
--
-- @
-- Bind ::= Name '=' Exp
--       |  Name '^' Nat '=' Exp
-- @
--
pBind   :: Config s p -> Parser s p (SnvBind s p)
pBind c
 = P.alt
        (P.enterOn (pNameOfSpace SVar) ExContextBind $ \name
         -> P.alt
                (do _       <- pPunc '='
                    x       <- pExp c
                    return  $ BindVar name 0 x)

                (do _       <- pPunc '^'
                    bump    <- pNat
                    _       <- pPunc '='
                    x       <- pExp c
                    return  $ BindVar name bump x))

        (do pPunc '?'
            ix <- pNat
            _  <- pPunc '='
            x  <- pExp c
            return $ BindNom ix x)

-- Param ----------------------------------------------------------------------
-- | Parser for a function parameter.
pParam  :: Parser s p Param
pParam
 = P.alts
 [ do   _       <- pPunc '!'
        n       <- pNameOfSpace SVar
        return  $  PParam n PVal

 , do   _       <- pPunc '~'
        n       <- pNameOfSpace SVar
        return  $  PParam n PExp

 , do   n       <- pNameOfSpace SVar
        return  $  PParam n PVal

 ]



