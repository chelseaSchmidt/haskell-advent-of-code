module Y2024.Day03 where
import ReadInputFile (readInputFileByName)

type Instruction = (String, String)
type CorruptInstructions = String
type PartialInt = String
type UnparsedInt = String
type PartialInstructions = [Instruction]
type ValidInstructions = [Instruction]
data Status = Enabled | Disabled deriving (Eq)
data Override = AlwaysEnabled | NoOverride deriving (Eq)

printY2024Day03Part1 :: IO ()
printY2024Day03Part1 = readInputFileByName "2024-03" >>= print . sum . map toProduct . parse AlwaysEnabled

printY2024Day03Part2 :: IO ()
printY2024Day03Part2 = readInputFileByName "2024-03" >>= print . sum . map toProduct . parse NoOverride

parse :: Override -> CorruptInstructions -> ValidInstructions
parse = go [] Enabled
  where
    go :: PartialInstructions -> Status -> Override -> CorruptInstructions -> ValidInstructions
    go partialInst _ override ('d':'o':'(':')':cs) = go partialInst Enabled override cs
    go partialInst _ override ('d':'o':'n':'\'':'t':'(':')':cs) = go partialInst Disabled override cs
    go partialInst status override ('m':'u':'l':'(':cs)
      | areIntsValid = go (partialInst ++ [(firstInt, secondInt)]) status override remainderAfterProcessing
      | otherwise = go partialInst status override cs
      where
        firstInt = fst (parseInt cs)
        afterFirstInt = snd (parseInt cs)
        secondInt = fst (parseInt (drop 1 afterFirstInt))
        afterSecondInt = snd (parseInt (drop 1 afterFirstInt))
        remainderAfterProcessing = drop 1 afterSecondInt
        areIntsValid =
          (status == Enabled || override == AlwaysEnabled)
          && take 1 afterFirstInt == ","
          && take 1 afterSecondInt == ")"
          && not (null firstInt)
          && not (null secondInt)
    go partialInst status override (_:cs) = go partialInst status override cs
    go partialInst _ _ _ = partialInst

parseInt :: CorruptInstructions -> (UnparsedInt, CorruptInstructions)
parseInt = go ""
  where
    go :: PartialInt -> CorruptInstructions -> (UnparsedInt, CorruptInstructions)
    go partialInt [] = (partialInt, [])
    go partialInt (c:cs)
      | c `elem` "0123456789" = go (partialInt ++ [c]) cs
      | otherwise = (partialInt, c:cs)

toProduct :: Instruction -> Int
toProduct i = read (fst i) * read (snd i)
