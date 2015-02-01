module Diamond where

data Element = None | A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

char None = ' '
char element = head $ show element

diamond :: Element -> [[Element]]
diamond element = mirrorDown $ mirrorLeft $ map pad $ [A .. element]
    where
    mirrorLeft = map (\row -> reverse (tail row) ++ row)
    mirrorDown rows = rows ++ tail (reverse rows)
    pad e = whitespace (fromEnum e - 1) ++ [e] ++ whitespace (fromEnum element - fromEnum e)
    whitespace n = replicate n None
