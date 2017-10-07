
module SMR.Data.Bag where
import Prelude hiding (map)
import qualified Data.List as List


-- | An unordered collection of things.
--   O(1) to add a single element, a list of elements, or union two bags.
data Bag a
        = BagNil
        | BagElem  a
        | BagList  [a]
        | BagUnion (Bag a) (Bag a)
        deriving Show


-- | O(1). Construct an empty bag.
nil     :: Bag a
nil = BagNil


-- | O(1). Construct a bag containing a single element.
singleton :: a -> Bag a
singleton x
 = BagElem x


-- | O(1). Construct a bag containing a list of elements.
list :: [a] -> Bag a
list xs
 = BagList xs


-- | O(1). Union two bags.
union :: Bag a -> Bag a -> Bag a
union xs1 xs2
 = BagUnion xs1 xs2


-- | O(n). Convert a bag to a list.
--   The elements come out in some deterministic but arbitrary order, no promises.
toList :: Bag a -> [a]
toList bag
 = go [] bag
 where
        go xs1  BagNil          = xs1
        go xs1 (BagElem x)      = x : xs1
        go xs1 (BagList xs2)    = go_list xs1 xs2
        go xs1 (BagUnion b1 b2) = go (go xs1 b1) b2

        go_list _   []          = []
        go_list xs1 (x : xs2)   = go_list (x : xs1) xs2


-- | Apply a function to all the elements in a bag.
map :: (a -> b) -> Bag a -> Bag b
map f bag
 = case bag of
        BagNil          -> BagNil
        BagElem  x      -> BagElem  (f x)
        BagList  xs     -> BagList  (List.map f xs)
        BagUnion b1 b2  -> BagUnion (map f b1) (map f b2)


