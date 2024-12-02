module CustomSet
  ( delete,
    difference,
    empty,
    fromList,
    insert,
    intersection,
    isDisjointFrom,
    isSubsetOf,
    member,
    null,
    size,
    toList,
    union,
  )
where

import Prelude hiding (null)
import qualified Prelude as P (null)

data CustomSet a = Nil | CustomSet a (CustomSet a) (CustomSet a) deriving (Show)

instance Eq a => Eq (CustomSet a) where
  a == b = toList a == toList b


_cross :: (Ord a) => CustomSet a -> CustomSet a -> ([a], [a], [a])
_cross a b =
  let la = toList a
      lb = toList b
   in _crossList la lb


_crossList :: Ord a => [a] -> [a] -> ([a], [a], [a])
_crossList [] lb = ([], [], lb)
_crossList la [] = (la, [], [])
_crossList (a : la) (b : lb)
  | a < b =
      let (l, c, r) = _crossList la (b : lb)
        in (a : l, c, r)
  | a > b =
      let (l, c, r) = _crossList (a : la) lb
        in (l, c, b : r)
  | otherwise =
      let (l, c, r) = _crossList la lb
        in (l, a : c, r)

delete :: (Ord a) => a -> CustomSet a -> CustomSet a
delete _ Nil = Nil
delete a (CustomSet v l r)
  | a < v = CustomSet v (delete a l) r
  | a > v = CustomSet v l (delete a r)
  | Nil <- l = r
  | Nil <- r = l
  | (lmax, l') <- extractMax l = CustomSet lmax l' r
  where
    extractMax Nil = error "Impossible"
    extractMax (CustomSet v l Nil) = (v, l)
    extractMax (CustomSet v l r) =
      let (rmax, l') = extractMax r
       in (rmax, CustomSet v l l')

difference :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
difference setA setB = let
  (l, _, _) = _cross setA setB
  in fromList l

empty :: CustomSet a
empty = Nil

fromList :: Ord a => [a] -> CustomSet a
fromList = foldr insert Nil

insert :: Ord a => a -> CustomSet a -> CustomSet a
insert x Nil = CustomSet x Nil Nil
insert x s@(CustomSet v l r)
  | x < v = CustomSet v (insert x l) r
  | x > v = CustomSet v l (insert x r)
  | otherwise = s

intersection :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
intersection setA setB = let
  (_, c, _) = _cross setA setB
  in fromList c

isDisjointFrom :: Ord a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA setB = let
  (_, c, _) = _cross setA setB
  in P.null c

isSubsetOf :: Ord a => CustomSet a -> CustomSet a -> Bool
isSubsetOf setA setB = let
  (l, _, _) = _cross setA setB
  in P.null l

member :: Ord a => a -> CustomSet a -> Bool
member x Nil = False
member x (CustomSet v l r)
  | x < v || x > v = member x l || member x r
  | otherwise = True

null :: CustomSet a -> Bool
null Nil = True
null _ = False

size :: CustomSet a -> Int
size Nil = 0
size (CustomSet _ l r) = size l + size r + 1

toList :: CustomSet a -> [a]
toList Nil = []
toList (CustomSet v l r) = toList l ++ [v] ++ toList r

union :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
union setA setB = let
  (l, c, r) = _cross setA setB
  in fromList (l ++ c ++ r)
