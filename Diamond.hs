module Diamond where

data DiamondSize = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    deriving (Eq, Read, Show)

diamond :: DiamondSize -> [[Char]]
diamond _ = undefined
