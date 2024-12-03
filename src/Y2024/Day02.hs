module Y2024.Day02 where
import ReadInputFile (readInputFileByName)

type Input = String
type Level = Int
type Report = [Level]
type Diffs = [Int]
data Direction = Asc | Desc deriving (Eq)

printY2024Day02Part1 :: IO ()
printY2024Day02Part1 = readInputFileByName "2024-02" >>= print . length . filter id . map isReportSafe . parse
  where
    isReportSafe :: Report -> Bool
    isReportSafe r = (isSortedIsh Asc r || isSortedIsh Desc r) && areDiffsInRange (reportToDiffs r)
      where
        isSortedIsh :: Direction -> Report -> Bool
        isSortedIsh _ [] = True
        isSortedIsh _ [_] = True
        isSortedIsh d (x:y:zs) = (if d == Asc then (<) else (>)) x y && isSortedIsh d (y:zs)

        areDiffsInRange :: Diffs -> Bool
        areDiffsInRange d = not (any (< 1) d) && not (any (> 3) d)

        reportToDiffs :: Report -> Diffs
        reportToDiffs [] = []
        reportToDiffs [_] = [1]
        reportToDiffs (a:b:cs) = abs (a - b) : reportToDiffs (b:cs)

parse :: Input -> [Report]
parse = map (map read . words) . lines
