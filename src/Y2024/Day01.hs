module Y2024.Day01 where
import ReadInputFile (readInputFileByName)
import Control.Monad (join)
import Data.List (partition, sort)

type LocationMetadata = (Location, Bool)
type Location = Int;

printY2024Day01Part1 :: IO ()
printY2024Day01Part1 = readInputFileByName "2024-01" >>= print
  . sum
  . map toDistance
  . zipLists
  . toLocationLists
  . partition isFromLeftList
  . join
  . map (toLocationMetadataPair . words)
  . lines

toLocationMetadataPair :: [String] -> [LocationMetadata]
toLocationMetadataPair [a, b] = [(read a, True), (read b, False)]
toLocationMetadataPair _ = error "Aaah"

isFromLeftList :: LocationMetadata -> Bool
isFromLeftList (_, source) = source

metadataToLocation :: LocationMetadata -> Location
metadataToLocation (location, _) = location

toLocationLists :: ([LocationMetadata], [LocationMetadata]) -> ([Location], [Location])
toLocationLists (a, b) = (mapAndSort a, mapAndSort b)
  where mapAndSort = sort . map metadataToLocation

zipLists :: ([Location], [Location]) -> [(Location, Location)]
zipLists (a, b) = zip a b

toDistance :: (Location, Location) -> Int
toDistance (a, b) = abs (a - b)
