import System.Environment (getArgs)

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
        myFunction input = unlines (asciiTranspose (lines input))

asciiTranspose :: [String] -> [String]
asciiTranspose css
    | null css = []
    | any null css = []
    | otherwise = (map head css):(asciiTranspose (map tail css))
