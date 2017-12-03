
module SMR.Codec.Size
        ( sizeOfSeq
        , sizeOfFile, sizeOfDecl
        , sizeOfRef
        , sizeOfExp,  sizeOfParam
        , sizeOfCar,  sizeOfSnvBind, sizeOfUpsBump
        , sizeOfName, sizeOfBump,    sizeOfNom)
where
import qualified Data.Text.Foreign      as T
import SMR.Core.Exp


-- | Compute the serialized size of a sequence of things.
sizeOfSeq :: (a -> Int) -> [a] -> Int
sizeOfSeq fs xs
 = result
 where  n       = length xs
        result
         | n < 2^8      = 1 + sum (map fs xs)
         | n < 2^16     = 2 + sum (map fs xs)
         | n < 2^32     = 4 + sum (map fs xs)
         | otherwise    = error "shimmer.sizeOfSeq: sequence too long to serialize."


-- | Compute the size of a serialized shimmer file containing the given decls.
sizeOfFile :: [Decl Text Text] -> Int
sizeOfFile decls
 = 4 + sizeOfSeq sizeOfDecl decls


-- | Compute the serialized size of a given declaration.
sizeOfDecl :: Decl Text Text -> Int
sizeOfDecl dd
 = case dd of
        DeclMac n x     -> 1 + sizeOfName n + sizeOfExp x
        DeclSet n x     -> 1 + sizeOfName n + sizeOfExp x


-- | Compute the serialized size of the given reference.
sizeOfRef :: Ref Text Text -> Int
sizeOfRef rr
 = case rr of
        RSym n          -> 1 + sizeOfName n
        RPrm n          -> 1 + sizeOfName n
        RMac n          -> 1 + sizeOfName n
        RSet n          -> 1 + sizeOfName n
        RNom n          -> 1 + sizeOfNom  n


-- | Compute the serialized size of the given expression.
sizeOfExp :: Exp Text Text -> Int
sizeOfExp xx
 = case xx of
        XRef ref        -> 1 + sizeOfRef ref
        XKey _key x     -> 2 + sizeOfExp x
        XApp x1 xs      -> 1 + sizeOfExp x1 + sizeOfSeq sizeOfExp xs
        XVar n b        -> 1 + sizeOfName n + sizeOfBump b
        XAbs ps x       -> 1 + sizeOfSeq sizeOfParam ps + sizeOfExp x
        XSub cs x       -> 1 + sizeOfSeq sizeOfCar cs   + sizeOfExp x


-- | Compute the serialized size of a parameter.
sizeOfParam :: Param -> Int
sizeOfParam (PParam n _form)
 = 1 + sizeOfName n


-- | Compute the serialized size of a substitution car.
sizeOfCar :: Car Text Text -> Int
sizeOfCar cc
 = case cc of
        CSim (SSnv snv) -> 1 + sizeOfSeq sizeOfSnvBind snv
        CRec (SSnv snv) -> 1 + sizeOfSeq sizeOfSnvBind snv
        CUps (UUps ups) -> 1 + sizeOfSeq sizeOfUpsBump ups


-- | Compute the serialized size of a substitution bind.
sizeOfSnvBind :: SnvBind Text Text -> Int
sizeOfSnvBind sb
 = case sb of
        BindVar n i x   -> 1 + sizeOfName n + sizeOfBump i + sizeOfExp x
        BindNom i x     -> 1 + sizeOfBump i + sizeOfExp x


-- | Compute the serialized size of an lifting bump.
sizeOfUpsBump :: UpsBump -> Int
sizeOfUpsBump ub
 = case ub of
        ((n, d), i)     -> 1 + sizeOfName n + sizeOfBump d + sizeOfBump i


-- | Compute the serialized size of a text string.
sizeOfName :: Text -> Int
sizeOfName tt
 = result
 where  n       = T.lengthWord16 tt * 2
        result
         | n < 2^8      = 1 + n
         | n < 2^16     = 2 + n
         | n < 2^32     = 4 + n
         | otherwise    = error "shimmer.sizeOfName: name too long to serialize."


-- | Compute the serialized size of a bump bounter.
sizeOfBump :: Integer -> Int
sizeOfBump _ = 2


-- | Compute the serialized size of a nominal atom.
sizeOfNom  :: Integer -> Int
sizeOfNom i  = 4

