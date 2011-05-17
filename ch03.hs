import Data.List

-- Recursive types

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList :: [a] -> List a
fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

-- ex. 1
fromList' :: List a -> [a]
fromList' (Cons x xs) = x:(fromList' xs)
fromList' Nil = []

-- ex. 2
data Tree' a = Node' a (Maybe (Tree' a)) (Maybe (Tree' a))
              deriving (Show)

-- Conditional evaluation with guards

-- ex. 1, 2
length' :: (Num n) => [a] -> n
length' [] = 0
length' (x:xs) = 1 + length' xs

guardedLength :: (Num n) => [a] -> n
guardedLength xs
    | null xs = 0
    | otherwise = 1 + guardedLength (tail xs)

-- ex. 3
mean :: (Fractional a) => [a] -> a
mean xs = sum xs / fromIntegral (length xs)

-- ex. 4
toPalindrom :: [a] -> [a]
toPalindrom xs = xs ++ reverse xs

-- ex. 5
isPalindrom :: (Eq a) => [a] -> Bool
isPalindrom xs = xs == reverse xs

-- ex. 6
sortByLength :: [[a]] -> [[a]]
sortByLength xs = sortBy cmpLength xs
    where cmpLength x y = compare (length x) (length y)

-- ex. 7
join :: a -> [[a]] -> [a]
join sep xss = inner_join sep (reverse xss) []
    where inner_join _ [] acc = acc
          inner_join sep (xs:xss) [] = inner_join sep xss xs
          inner_join sep (xs:xss) acc = inner_join sep xss (xs ++ sep:acc)

-- ex. 8
treeHeight :: (Tree a) -> Integer
treeHeight Empty = 0
treeHeight (Node val left right) = 1 + max (treeHeight left) (treeHeight right)

-- ex. 9
data Direction = Straight
               | TurnLeft
               | TurnRight
               deriving (Show, Eq)

-- ex. 10
type Point2D = (Double, Double)
type Vector2D = (Double, Double)

vector :: Point2D -> Point2D -> Vector2D
vector (x1, y1) (x2, y2) = (x2-x1, y2-y1)

direction :: Point2D -> Point2D -> Point2D -> Direction
direction a b c = case vectorZsign of
                        GT -> TurnLeft
                        LT -> TurnRight
                        EQ -> Straight
                where
                    (x1, y1) = vector a b
                    (x2, y2) = vector b c
                    vectorZsign = compare (x1*y2) (y1*x2)

-- ex. 11
pathDirections :: [Point2D] -> [Direction]
pathDirections (p1:p2:p3:rest) = (direction p1 p2 p3):(pathDirections (p2:p3:rest))
pathDirections _ = []

-- ex. 12
sinWithX :: Point2D -> Point2D -> Double
sinWithX p1 p2
              | sin > 0 = sin
              | otherwise = sin
              where
                  (x1, y1) = p1
                  (x2, y2) = p2
                  a = (y2 - y1)
                  (vx, vy) = vector p1 p2
                  h = sqrt (squaresum vx vy)
                  sin = a/h

squaresum x y = x*x + y*y

hullBaseIndex :: Point2D -> Point2D -> Ordering
hullBaseIndex (x1, y1) (x2, y2)
    | ycmp == EQ = xcmp
    | otherwise = ycmp
    where ycmp = compare y1 y2
          xcmp = compare x1 x2

hullDistance :: Point2D -> Point2D -> Point2D -> Ordering
hullDistance p0 p1 p2
    | sin1 == sin2 = let (vx, vy) = vector p0 p2
                         (ux, uy) = vector p0 p2 in
                         compare (squaresum vx vy) (squaresum ux uy)
    | otherwise = compare sin1 sin2
    where
        sin1 = sinWithX p0 p1
        sin2 = sinWithX p0 p2

fillTheHull :: [Point2D] -> [Point2D]
fillTheHull (p0:p1:p2:points)
    | dir == TurnLeft = (p0:(fillTheHull (p1:p2:points)))
    | otherwise = fillTheHull (p0:p2:points)
    where dir = direction p0 p1 p2
fillTheHull any = any

graham :: [Point2D] -> [Point2D]
graham points = let
                    p0 = minimumBy hullBaseIndex points
                    rest_points = init (sortBy (hullDistance p0) points)
                in
                    fillTheHull (p0:rest_points)

--hull_test = [(5.0, 5.0), (3, 2), (4, 4), (4, 4.1), (4, 3.9), (1, 4), (0.5, 3), (0.5, 0.5), (0.5, 2), (0.5, 1), (4, 2)]
