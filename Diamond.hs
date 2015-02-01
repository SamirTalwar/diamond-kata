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
    expand element = padding element ++ row element ++ padding element
    padding element = whitespace $ fromEnum size - fromEnum element
    row A = [A]
    row element = [element] ++ inner element ++ [element]
    inner element = whitespace $ fromEnum element * 2 - 3
    whitespace n = take n $ repeat None
