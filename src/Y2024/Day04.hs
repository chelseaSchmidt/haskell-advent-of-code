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

printY2024Day04Part2 :: IO ()
printY2024Day04Part2 = readInputFileByName "2024-04" >>= print . length . filter isCrossedXMAS . extract3x3Blocks . lines

-- Part 1

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

-- Part 2

extract3x3Blocks :: [Row] -> [Sequence]
extract3x3Blocks rows = go rows rows
  where
    go :: [Row] -> [Row] -> [Sequence]
    go origRows (a:b:c:ds)
      | length a >= 3 = [head a, a !! 2, b !! 1, head c, c !! 2]:go origRows (b:c:ds)
      | otherwise = []
    go origRows _ = go (map (drop 1) origRows) (map (drop 1) origRows)

isCrossedXMAS :: Sequence -> Bool
isCrossedXMAS ['M','S','A','M','S'] = True
isCrossedXMAS ['S','M','A','S','M'] = True
isCrossedXMAS ['M','M','A','S','S'] = True
isCrossedXMAS ['S','S','A','M','M'] = True
isCrossedXMAS _ = False
