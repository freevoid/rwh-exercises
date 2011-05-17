lastButOne :: [a] -> a
lastButOne [x, _] = x
lastButOne (x:xs) = lastButOne xs
lastButOne _ = error "List is too small!"


{-
lastButOne' :: [a] -> a
lastButOne [x, _] = x
lastButOne x:xs = lastButOne tail xs
lastButOne _ = ()
-}
