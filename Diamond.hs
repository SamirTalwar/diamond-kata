module Diamond where

data DiamondSize = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

sizes :: [DiamondSize]
sizes = [minBound..maxBound]

diamond :: DiamondSize -> [[Char]]
diamond _ = map show sizes
