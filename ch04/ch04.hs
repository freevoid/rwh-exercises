import Data.List
import Data.Maybe

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
