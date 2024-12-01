module ReadInputFile where

readInputFileByName :: String -> IO String
readInputFileByName fileName = readFile ("inputs/" ++ fileName ++ ".txt")
