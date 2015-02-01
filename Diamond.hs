module Diamond where

data DiamondSize = None | A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

sizes :: [DiamondSize]
sizes = [A .. Z]

diamond :: DiamondSize -> [[Char]]
diamond size = map (foldr1 (++) . map show) $ map (take rowLength . repeat) ([A .. previous] ++ [size, previous .. A])
    where
    previous = pred size
    rowLength = fromEnum size * 2 - 1
