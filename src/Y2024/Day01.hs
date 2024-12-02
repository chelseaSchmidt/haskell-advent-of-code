module Y2024.Day01 where
import ReadInputFile (readInputFileByName)
import Control.Monad (join)
import Data.List (partition, sort)

data (:&:) a = a :&: a
type Location = Int
type SimilarityScore = Int
type ScoredLocation = (Location, SimilarityScore)
data ListSide = Left' | Right' deriving (Eq)

instance Functor (:&:) where
  fmap :: (a -> b) -> (:&:) a -> (:&:) b
  fmap f (a :&: b) = f a :&: f b

printY2024Day01Part1 :: IO ()
printY2024Day01Part1 = readInputFileByName "2024-01" >>= print
  . sum
  . map toDistance
  . zipLists
  . fmap sort
  . parse
  where
    zipLists (a :&: b) = zipWith (:&:) a b
    toDistance (a :&: b) = abs (a - b)

printY2024Day01Part2 :: IO ()
printY2024Day01Part2 = readInputFileByName "2024-01" >>= print
  . sum
  . toSimilarityScores
  . parse
  where
    toSimilarityScores :: (:&:) [Location] -> [SimilarityScore]
    toSimilarityScores (leftList :&: rightList) = map (scoreSimilarity rightList) leftList
      where
        scoreSimilarity :: [Location] -> Location -> SimilarityScore
        scoreSimilarity rightLocations leftLoc = leftLoc * length (filter (== leftLoc) rightLocations)

parse :: String -> (:&:) [Location]
parse = fmap (map fst)
  . tupleToTypedPair
  . partition ((==) Left' . snd)
  . join
  . map (toLocationMetadataList . toWordPair . words)
  . lines
  where
    toWordPair [a, b] = a :&: b
    toWordPair _ = error "Could not parse input"
    toLocationMetadataList (a :&: b) = [(read a, Left'), (read b, Right')]
    tupleToTypedPair (a, b) = a :&: b
