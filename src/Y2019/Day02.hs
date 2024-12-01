module Y2019.Day02 where
import ReadInputFile (readInputFileByName)
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty (NonEmpty((:|)))

type Input = String
type Program = NonEmpty Int
type InstrPointer = Int

printY2019Day02Part1 :: IO ()
printY2019Day02Part1 = readInputFileByName "2019-02" >>= print
  . NE.head
  . compute
  . getReconstructedCodes
  . stringToIntsCareOfBradley

stringToIntsCareOfBradley :: Input -> Program
stringToIntsCareOfBradley = NE.fromList . map read . words . map (\c -> if c == ',' then ' ' else c)

getReconstructedCodes :: Program -> Program
getReconstructedCodes (x:|_:_:xs) = x:|12:2:xs
getReconstructedCodes _ = error "No"

compute :: Program -> Program
compute = go 0
  where
    go :: InstrPointer -> Program -> Program
    go instrPointer program = case NE.drop instrPointer program of
      (99:_) -> program
      (opcode:inputOnePos:inputTwoPos:outputPos:_) -> case opcode of
        1 -> go (instrPointer + 4) $ updateProgram (+)
        2 -> go (instrPointer + 4) $ updateProgram (*)
        _ -> error "NO"
        where
          (pre, post) = case NE.splitAt outputPos program of
            (as, _:bs) -> (as, bs)
            _ -> error "Absolutely not"

          updateProgram :: (Int -> Int -> Int) -> Program
          updateProgram f = NE.fromList $ pre ++ [(program NE.!! inputOnePos) `f` (program NE.!! inputTwoPos)] ++ post
      _ -> error "NOO"
