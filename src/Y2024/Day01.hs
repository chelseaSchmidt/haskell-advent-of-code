module Y2024.Day01 where
import ReadInputFile (readInputFileByName)
import Control.Monad (join)
import Data.List (partition, sort)
import Data.Bifunctor (Bifunctor(bimap))

data TypedTuple a = a :&: a
type Location = Int

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
