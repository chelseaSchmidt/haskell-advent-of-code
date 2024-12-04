module Y2024.Day03 where
import ReadInputFile (readInputFileByName)

type Instruction = (String, String)
type CorruptInstructions = String
type PartialInt = String
type UnparsedInt = String
type AccumulatedInstructions = [Instruction]
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
    go :: AccumulatedInstructions -> Status -> Override -> CorruptInstructions -> ValidInstructions
    go accum _ override ('d':'o':'(':')':cs) = go accum Enabled override cs
    go accum _ override ('d':'o':'n':'\'':'t':'(':')':cs) = go accum Disabled override cs
    go accum status override ('m':'u':'l':'(':cs)
      | isInstructionValid = go ((firstInt, secondInt):accum) status override nextInstructions
      | otherwise = go accum status override cs
      where
        firstInt = fst (parseInt cs)
        charsAfterFirstInt = snd (parseInt cs)
        secondInt = fst (parseInt (drop 1 charsAfterFirstInt))
        charsAfterSecondInt = snd (parseInt (drop 1 charsAfterFirstInt))
        nextInstructions = drop 1 charsAfterSecondInt
        isInstructionValid =
          (status == Enabled || override == AlwaysEnabled)
          && take 1 charsAfterFirstInt == ","
          && take 1 charsAfterSecondInt == ")"
          && not (null firstInt)
          && not (null secondInt)
    go accum status override (_:cs) = go accum status override cs
    go accum _ _ _ = accum

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
