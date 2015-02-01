module Diamond where

data Element = None | A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

sizes :: [Element]
sizes = [A .. Z]

char None = ' '
char element = head $ show element

diamond :: Element -> [[Char]]
diamond size = map (map char) $ map expand $ ([A .. previous] ++ [size, previous .. A])
    where
    previous = pred size
    expand element
        | element == size = (take len . repeat) element
        | otherwise       = whitespace ++ [element] ++ whitespace
    whitespace = take (fromEnum size  - 1) $ repeat None
    len = fromEnum size * 2 - 1
