import Data.List
import Data.Maybe
import Data.Either
import Data.Char (digitToInt, isDigit, isSpace)

-- Working with lists

-- ex. 1

-- variation 1: straightforward, without original functions
safeHead :: [a] -> Maybe a
safeHead (x:xs) = (Just x)
safeHead _ = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (x:xs) = (Just xs)
safeTail _ = Nothing

safeLast :: [a] -> Maybe a
safeLast (x:xs)
    | null xs = (Just x)
    | otherwise = safeLast xs
safeLast _ = Nothing

safeInit :: [a] -> Maybe [a]
safeInit (x:xs)
    | null xs = (Just [])
    | otherwise = (Just (x:(fromMaybe [] (safeInit xs))))
safeInit _ = Nothing

-- variation 2: just wraps
safeHead' :: [a] -> Maybe a
safeHead' [] = Nothing
safeHead' xs = Just (head xs)

-- variation 2b: with guards
safeTail' :: [a] -> Maybe [a]
safeTail' xs
    | null xs = Nothing
    | otherwise = Just (tail xs)

safeLast' :: [a] -> Maybe a
safeLast' [] = Nothing
safeLast' xs = Just (last xs)

safeInit' :: [a] -> Maybe [a]
safeInit' [] = Nothing
safeInit' xs = Just (init xs)

-- ex. 2
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith pred [] = []
splitWith pred xs
    | null to_include = splitWith pred rest'
    | otherwise = to_include:(splitWith pred rest')
    where
        (to_include, rest) = span pred xs
        (to_drop, rest') = break pred rest

testSplitter a = a `elem` ['A'..'Z']

-- ex. 3
-- see firstwords.hs

-- ex. 4
-- see asciitranspose.hs


-- How to think about loops

-- ex. 1
asInt_fold :: String -> Int
asInt_fold ('-':cs)
    | null cs = error "Only minus sign provided"
    | otherwise = negate (asInt_fold cs)
asInt_fold cs
    | null cs = error "Empty list"
    | otherwise = foldl step 0 cs
    where step acc c
            | isDigit c = 10*acc + (digitToInt c)
            | otherwise = error ("Not a digit: '" ++ [c] ++ "'")

-- ex. 2
type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either ('-':cs)
    | null cs = Left "Only minus sign provided"
    | otherwise = case (asInt_either cs) of
                    (Right val) -> Right (negate val)
                    left -> left
asInt_either cs
    | null cs = Left "Empty list"
    | otherwise = foldl step (Right 0) cs
    where step (Right acc) c
            | isDigit c = Right (10*acc + (digitToInt c))
            | otherwise = Left ("Not a digit: '" ++ [c] ++ "'")
          step (Left error_msg) c = (Left error_msg)

-- ex. 3
myConcat :: [[a]] -> [a]
myConcat xss = foldr (++) [] xss

-- ex. 4
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile pred (x:xs)
    | pred x = x:(myTakeWhile pred xs)
    | otherwise = []
myTakeWhile _ _ = []

myTakeWhile_fold :: (a -> Bool) -> [a] -> [a]
myTakeWhile_fold pred xs = foldr step [] xs
    where step x acc
            | pred x = x:acc
            | otherwise = []

-- ex. 5
myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy eqfun xs = foldr step [] xs
    where step x acc@(current_group:grouped_xs)
            | null current_group = [x]:grouped_xs
            | eqfun x (head current_group) = (x:current_group):grouped_xs
            | otherwise = [x]:acc
          step x grouped_xs = [x]:grouped_xs

-- ex. 6
myAny :: (a -> Bool) -> [a] -> Bool
myAny test = foldr (\a b -> test a || b) False

myCycle :: [a] -> [a]
myCycle xs = foldr (:) (myCycle xs) xs

myWords :: String -> [String]
myWords cs
    | null cs = []
    | otherwise = foldr step [[]] cs
        where step c ([]:words)
                | (isSpace c) = []:words
                | otherwise = [c]:words
              step c acc@(word:words)
                | (isSpace c) = []:acc
                | otherwise = (c:word):words

myUnlines :: [String] -> String
myUnlines css = foldr step [] css
    where step cs line = cs ++ '\n':line
