module Y2019.Day01 where
import ReadInputFile (readInputFileByName)

printY2019Day01Part1 :: IO ()
printY2019Day01Part1 = readInputFileByName "2019-01" >>= print . sum . map (massToFuel . read) . lines

printY2019Day01Part2 :: IO ()
printY2019Day01Part2 = readInputFileByName "2019-01" >>= print . getTotalFuelRequirementPartTwo . map read . lines

massToFuel :: Int -> Int
massToFuel mass = (mass `div` 3) - 2

zeroFloor :: Int -> Int
zeroFloor int
  | int > 0 = int
  | otherwise = 0

getTotalFuelRequirementPartTwo :: [Int] -> Int
getTotalFuelRequirementPartTwo [] = 0
getTotalFuelRequirementPartTwo masses =
  (sum . getFuelRequirements) masses
  + (getTotalFuelRequirementPartTwo . filter (/=0) . getFuelRequirements) masses
    where getFuelRequirements = map (zeroFloor . massToFuel)
