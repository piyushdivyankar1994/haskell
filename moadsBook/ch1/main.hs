import Prelude hiding (pure)

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

numberOfLeaves :: Tree a -> Integer
numberOfLeaves (Leaf _) = 1
numberOfLeaves (Node l r) = numberOfLeaves l + numberOfLeaves r

relabel :: Tree a -> Int -> (Tree (Int, a), Int)
relabel (Leaf x) i = (Leaf (i, x), i + 1)
relabel (Node l r) i =
  let (l', i1) = relabel l i
      (r', i2) = relabel r i1
   in (Node l' r', i2)

type WithCounter a = Int -> (a, Int)

next :: WithCounter a -> (a -> WithCounter b) -> WithCounter b
f `next` g = \i -> let (r, i') = f i in g r i'

pure :: a -> WithCounter a
pure x = \i -> (x, i)

relabel2 (Leaf x) = \i -> (Leaf (i, x), i + 1)
relabel2 (Node l r) =
  relabel2 l `next` \l' ->
    relabel2 r `next` \r' ->
      pure (Node l' r')
