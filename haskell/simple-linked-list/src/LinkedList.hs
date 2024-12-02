module LinkedList
  ( LinkedList,
    datum,
    fromList,
    isNil,
    new,
    next,
    nil,
    reverseLinkedList,
    toList,
  )
where

data LinkedList a = Nil | Next a (LinkedList a)
  deriving (Eq, Show)

datum :: LinkedList a -> a
datum (Next a _) = a

fromList :: [a] -> LinkedList a
fromList = foldr Next Nil

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new = Next

next :: LinkedList a -> LinkedList a
next (Next _ l) = l

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList Nil = Nil
reverseLinkedList l = _reverseLinkedList Nil l
  where
    _reverseLinkedList left Nil = left
    _reverseLinkedList left (Next a l) = _reverseLinkedList (Next a left) l

toList :: LinkedList a -> [a]
toList Nil = []
toList (Next a l) = a : toList l
