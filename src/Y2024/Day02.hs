module Y2024.Day02 where
import ReadInputFile (readInputFileByName)

type Input = String
type Level = Int
type Report = [Level]
type PreviousReport = [Level]
type Diffs = [Int]
type Freebies = Int
data Direction = Asc | Desc deriving (Eq)

printY2024Day02Part1 :: IO ()
printY2024Day02Part1 = readInputFileByName "2024-02" >>= print . length . filter id . map (isReportSafe 0) . parse

printY2024Day02Part2 :: IO ()
printY2024Day02Part2 = readInputFileByName "2024-02" >>= print . length . filter id . map (isReportSafe 1) . parse

isReportSafe :: Freebies -> Report -> Bool
isReportSafe f r = isSortedIshAndInRange f Asc r || isSortedIshAndInRange f Desc r
  where
    isSortedIshAndInRange :: Freebies -> Direction -> Report -> Bool
    isSortedIshAndInRange fr d = go fr d []
      where
        go :: Freebies -> Direction -> PreviousReport -> Report -> Bool
        go _ _ _ [] = True
        go _ _ _ [_] = True
        go freebies dir prev (x:y:zs)
          | isPassing = go freebies dir (prev ++ [x]) (y:zs)
          | freebies > 0 =
            go (freebies - 1) dir [] (prev ++ y:zs)
            || go (freebies - 1) dir [] (prev ++ x:zs)
          | otherwise = False
          where
            isPassing =
              (if dir == Asc then (<) else (>)) x y
              && abs (x - y) >= 1
              && abs (x - y) <= 3

parse :: Input -> [Report]
parse = map (map read . words) . lines
