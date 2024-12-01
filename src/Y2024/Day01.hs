module Y2024.Day01 where
import ReadInputFile (readInputFileByName)
import Control.Monad (join)
import Data.List (partition, sort)
import Data.Bifunctor (Bifunctor(bimap))

data TypedTuple a = a :&: a
type Location = Int
type SimilarityScore = Int
type SimilarityScores = [Int]
type ScoredLocation = (Location, SimilarityScore)

printY2024Day01Part1 :: IO ()
printY2024Day01Part1 = readInputFileByName "2024-01" >>= print
  . sum
  . map toDistance
  . zipLists
  . bimap sort sort
  . parse
  where
    zipLists (a, b) = zipWith (:&:) a b
    toDistance (a :&: b) = abs (a - b)

printY2024Day01Part2 :: IO ()
printY2024Day01Part2 = readInputFileByName "2024-01" >>= print
  . sum
  . toSimilarityScores
  . parse
  where
    toSimilarityScores :: ([Location], [Location]) -> SimilarityScores
    toSimilarityScores (leftList, rightList) = map (scoreSimilarity rightList) leftList
      where
        scoreSimilarity :: [Location] -> Location -> SimilarityScore
        scoreSimilarity rightLocations leftLoc = leftLoc * length (filter (== leftLoc) rightLocations)

parse :: String -> ([Location], [Location])
parse = bimap (map fst) (map fst)
  . partition snd
  . join
  . map (toLocationMetadataList . toWordPair . words)
  . lines
  where
    toWordPair [a, b] = a :&: b
    toWordPair _ = error "Could not parse input"
    isLeft = True
    isRight = False
    toLocationMetadataList (a :&: b) = [(read a, isLeft), (read b, isRight)]
