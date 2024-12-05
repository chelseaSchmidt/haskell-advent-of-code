module Y2024.Day04 where
import ReadInputFile (readInputFileByName)
import Data.Ix (Ix(range))
import Control.Monad (join)

type Sequence = String
type Sequences = [Sequence]
type Row = String
type ColIndex = Int
type Count = Int

printY2024Day04Part1 :: IO ()
printY2024Day04Part1 = readInputFileByName "2024-04" >>= print . sum . map countMatches . join . toSequenceLists . lines

toSequenceLists :: [Row] -> [Sequences]
toSequenceLists [] = []
toSequenceLists (r:rs) =
  [
    rows,
    map (extractColumn rows) colIndices,
    extractDiagsDown rows colIndices,
    map (extractDiagsAcross rows) diagonal1s,
    extractDiagsDown rows (reverse colIndices),
    map (extractDiagsAcross rows) diagonal2s
  ]
  where
    rows = r:rs
    rowLength = length r - 1
    colIndices = range (0, rowLength)
    rangeTo end start = range (start, end)
    rangeDown x = [x,(x-1)..0]
    diagonal1s :: [[ColIndex]] = map (rangeTo (min rowLength (length rows))) (drop 1 colIndices)
    diagonal2s :: [[ColIndex]] = map rangeDown (drop 1 (reverse colIndices))

extractColumn :: [Row] -> ColIndex -> Sequence
extractColumn rows colIndex = map (!! colIndex) rows

extractDiagsDown :: [Row] -> [ColIndex] -> [Sequence]
extractDiagsDown [] _ = []
extractDiagsDown _ [] = []
extractDiagsDown rows diagIndices = zipWith (curry valueAt) rows diagIndices:extractDiagsDown nextRows nextDiagIndices
  where
    nextRows = drop 1 rows
    nextRowCount = length nextRows
    nextDiagIndices = take (min nextRowCount (length diagIndices)) diagIndices

extractDiagsAcross :: [Row] -> [ColIndex] -> Sequence
extractDiagsAcross = zipWith (curry valueAt)

valueAt :: (Row, ColIndex) -> Char
valueAt (row, colIndex) = row !! colIndex

countMatches :: Sequence -> Int
countMatches s = go 0 s + go 0 (reverse s)
  where
    go :: Count -> Sequence -> Int
    go count [] = count
    go count ('X':'M':'A':'S':cs) =  go (count + 1) cs
    go count (_:cs) = go count cs
