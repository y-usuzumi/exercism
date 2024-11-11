module BST
  ( BST,
    bstLeft,
    bstRight,
    bstValue,
    empty,
    fromList,
    insert,
    singleton,
    toList,
  )
where

data BST a = Null | Node a (BST a) (BST a) deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Null = Nothing
bstLeft (Node _ l _) = Just l

bstRight :: BST a -> Maybe (BST a)
bstRight Null = Nothing
bstRight (Node _ _ r) = Just r

bstValue :: BST a -> Maybe a
bstValue Null = Nothing
bstValue (Node v _ _) = Just v

empty :: BST a
empty = Null

fromList :: (Ord a) => [a] -> BST a
fromList = foldl (flip insert) empty

-- Nowhere in the problem itself is it mentioned that duplicate values are allowed,
-- or it must be inserted to the left branch.
insert :: (Ord a) => a -> BST a -> BST a
insert x Null = singleton x
insert x (Node v l r) = if x <= v then Node v (insert x l) r else Node v l (insert x r)

singleton :: a -> BST a
singleton x = Node x Null Null

toList :: BST a -> [a]
toList Null = []
toList (Node v l r) = toList l ++ [v] ++ toList r
