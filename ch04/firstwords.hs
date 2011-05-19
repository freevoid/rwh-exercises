import System.Environment (getArgs)
import Data.Maybe

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction input = unlines (firstWords input)

firstWords :: String -> [String]
firstWords cs = map (\cs -> fromMaybe [] (safeHead (words cs))) (lines cs)

filterNull :: [[a]] -> [[a]]
filterNull css = filter (not . null) css

safeHead :: [a] -> Maybe a
safeHead (x:xs) = (Just x)
safeHead _ = Nothing
