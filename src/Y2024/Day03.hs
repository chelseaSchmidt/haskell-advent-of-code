module Y2024.Day03 where
import ReadInputFile (readInputFileByName)

type Instruction = (String, String)
type CorruptInstructions = String
type PartialInt = String
type UnparsedInt = String
type PartialInstructions = [Instruction]
type ValidInstructions = [Instruction]
data Status = Enabled | Disabled deriving (Eq)
data StatusOverride = AlwaysEnabled | NoOverride deriving (Eq)

printY2024Day03Part1 :: IO ()
printY2024Day03Part1 = readInputFileByName "2024-03" >>= print . sum . map toProduct . parse AlwaysEnabled

printY2024Day03Part2 :: IO ()
printY2024Day03Part2 = readInputFileByName "2024-03" >>= print . sum . map toProduct . parse NoOverride

-- valid sequence: "m", "u", "l", "(", "\d+", "," "\d+", ")"
parse :: StatusOverride -> CorruptInstructions -> ValidInstructions
parse = go [] Enabled
  where
    go :: PartialInstructions -> Status -> StatusOverride -> CorruptInstructions -> ValidInstructions
    go partialInst _ statusOverride ('d':'o':'(':')':cs) = go partialInst Enabled statusOverride cs
    go partialInst _ statusOverride ('d':'o':'n':'\'':'t':'(':')':cs) = go partialInst Disabled statusOverride cs
    go partialInst status statusOverride ('m':'u':'l':'(':cs) =
      go
        (if areIntsValid then partialInst ++ [(firstInt, secondInt)] else partialInst)
        status
        statusOverride
        (if areIntsValid then remainingInstructions else cs)
      where
        firstInt = fst (parseInt cs)
        theRest = snd (parseInt cs)
        secondInt = fst (parseInt (drop 1 theRest))
        theRestOfTheRest = snd (parseInt (drop 1 theRest))
        maybeComma = if not (null theRest) then head theRest else 'x'
        maybeParen = if not (null theRestOfTheRest) then head theRestOfTheRest else 'x'
        remainingInstructions = drop 1 theRestOfTheRest
        areIntsValid =
          (status == Enabled || statusOverride == AlwaysEnabled)
          && maybeComma == ','
          && maybeParen == ')'
          && not (null firstInt)
          && not (null secondInt)
    go partialInst status statusOverride (_:cs) = go partialInst status statusOverride cs
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
