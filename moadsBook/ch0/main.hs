import Prelude hiding (Bool, Eq, False, True, (!!), (&&), (==))

data Bool = True | False deriving (Show)

or :: Bool -> Bool -> Bool
or True _ = True
or False x = x

(&&) :: Bool -> Bool -> Bool
(&&) True True = True
(&&) _ _ = False

(!!) :: Bool -> Bool
(!!) True = False
(!!) _ = True

class Eq a where
  (==) :: a -> a -> Bool

instance Eq Bool where
  True == True = True
  False == False = False
  _ == _ = False

instance Eq Int where
  x == y = x ^ y

instance (Eq a) => Eq [a] where
  (==) = eqList

eqList :: (Eq a) => [a] -> [a] -> Bool
eqList [] [] = True
eqList (x : xs) (y : ys) = x == y && eqList xs ys

instance (Eq a, Eq b) => Eq (a, b) where
  (==) (a, b) (c, d) = (a == c) && (b == d)

notEq :: (Eq a) => a -> a -> Bool
notEq x y = (!!) $ (==) x y