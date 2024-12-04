module Y2024.Day03 where
import ReadInputFile (readInputFileByName)

type Instruction = (String, String)
type CorruptInstructions = String
type PartialInt = String
type UnparsedInt = String
type PartialInstructions = [Instruction]
type ValidInstructions = [Instruction]

printY2024Day03Part1 :: IO ()
printY2024Day03Part1 = readInputFileByName "2024-03" >>= print . sum . map toProduct . parse

-- valid sequence: "m", "u", "l", "(", "\d+", "," "\d+", ")"
parse :: CorruptInstructions -> ValidInstructions
parse = go []
  where
    go :: PartialInstructions -> CorruptInstructions -> ValidInstructions
    go partialInst ('m':'u':'l':'(':cs) =
      go
        (if areIntsValid then partialInst ++ [(firstInt, secondInt)] else partialInst)
        (if areIntsValid then drop 1 theRestOfTheRest else cs)
      where
        firstInt = fst (parseInt cs)
        theRest = snd (parseInt cs)
        secondInt = fst (parseInt (drop 1 theRest))
        theRestOfTheRest = snd (parseInt (drop 1 theRest))
        maybeComma = if not (null theRest) then head theRest else 'x'
        maybeParen = if not (null theRestOfTheRest) then head theRestOfTheRest else 'x'
        isFirstIntValid =  not (null firstInt)
        isSecondIntValid =  not (null secondInt)
        areIntsValid = maybeComma == ',' && maybeParen == ')' && isFirstIntValid && isSecondIntValid
    go partialInst (_:cs) = go partialInst cs
    go partialInst _ = partialInst

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
