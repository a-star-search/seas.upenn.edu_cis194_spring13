type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 source dest aux = []
hanoi 1 source dest aux = [(source, dest)]
hanoi diskCount source dest aux = 
  concat [(moveTopDisksFromSourcePegToAuxPeg (diskCount - 1) source dest aux),
          (moveBottomDiskFromSourcePegToDestPeg source dest),
          (moveTopDisksFromAuxPegToDestPeg (diskCount - 1) source dest aux)]

moveTopDisksFromSourcePegToAuxPeg :: Integer -> Peg -> Peg -> Peg -> [Move]
moveTopDisksFromSourcePegToAuxPeg diskCount originalSource originalDest originalAux = 
  hanoi diskCount originalSource originalAux originalDest

moveBottomDiskFromSourcePegToDestPeg :: Peg -> Peg -> [Move]
moveBottomDiskFromSourcePegToDestPeg source dest = [(source, dest)]

moveTopDisksFromAuxPegToDestPeg :: Integer -> Peg -> Peg -> Peg -> [Move]
moveTopDisksFromAuxPegToDestPeg diskCount originalSource originalDest originalAux = 
  hanoi diskCount originalAux originalDest originalSource
