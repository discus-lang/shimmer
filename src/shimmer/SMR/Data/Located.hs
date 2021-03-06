
module SMR.Data.Located where


-- | Location in a source file.
data Location
        = L  Int Int
        deriving Show


-- | A thing located at the given range in a source file.
data Located a
        = LL Location Location a
        deriving Show


-- | Take the start point of a located thing.
startOfLocated :: Located a -> Location
startOfLocated (LL start _ _) = start


-- | Take the end point of a located thing.
endOfLocated :: Located a -> Location
endOfLocated (LL _ end _) = end


-- | Take the value of a located thing.
valueOfLocated :: Located a -> a
valueOfLocated (LL _ _ x) = x

-- | Increment the character position of a located thing.
incCharOfLocation :: Int -> Location -> Location
incCharOfLocation n (L l c) = L l (c + n)


-- | Increment the line position of a located thing.
incLineOfLocation :: Int -> Location -> Location
incLineOfLocation n (L l _) = L (l + n) 1

