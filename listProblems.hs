-- Problem 1
-- (*) Find the last element of a list.
_last :: [a] -> a
_last = head . reverse

-- Problem 2
-- (*) Find the last but one element of a list.
_lastButOne :: [a] -> a
_lastButOne = head . tail . reverse

-- Problem 3
-- (*) Find the K'th element of a list. The first element in the list is number 1.
_elementAt :: [a] -> Int -> a
_elementAt xs i = xs !! (i - 1)

-- Problem 4
-- (*) Find the number of elements of a list.
_length :: [a] -> Int
_length [] = 0
_length xs = 1 + _length (tail xs)

-- Problem 5
-- (*) Reverse a list.
_reverse :: [a] -> [a]
_reverse [] = []
_reverse xs = (_reverse (tail xs)) ++ [head xs]

-- Problem 6
-- (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
_isPalindrome :: (Eq a) => [a] -> Bool
_isPalindrome xs = xs == _reverse xs

-- Problem 7
-- (**) Flatten a nested list structure.
data NestableList a = Element a                -- We have to define a new data type, because lists in Haskell are homogeneous.
                    | List    [NestableList a]
                    deriving Show

_listToNestableList :: [a] -> NestableList a -- Just for testing
_listToNestableList = List . map (Element)

_flatten :: NestableList a -> [a]
_flatten (Element e  ) = [e]
_flatten (List []    ) = [ ]
_flatten (List (x:xs)) = _flatten x ++ _flatten (List xs)

-- Problem 8
-- (**) Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
_compress :: (Eq a) => [a] -> [a]
_compress (x:y:r)
    | r == [ ]  =     _check
    | x ==  y   =     _compress (y : r)
    | otherwise = x : _compress (y : r)
    where
        _check
            | x == y    = [x]
            | otherwise = x : [y]

-- Problem 9
-- (**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
_takeWhile :: (a -> Bool) -> [a] -> [a]
_takeWhile _ [] = []
_takeWhile predicate (x:xs)
    | predicate x = x : _takeWhile predicate xs
    | otherwise   = []

_dropWhile :: (a -> Bool) -> [a] -> [a]
_dropWhile _ [] = []
_dropWhile predicate (x:xs)
    | predicate x = _dropWhile predicate xs
    | otherwise   = x : xs

_pack :: (Eq a) => [a] -> [[a]]
_pack []     = []
_pack (x:xs) = (x : _package) : _pack _rest
    where _package = _takeWhile (== x) xs
          _rest    = _dropWhile (== x) xs

-- Problem 10
-- (*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
_zip :: [a] -> [b] -> [(a, b)]
_zip [] [] = []
_zip xs [] = []
_zip [] ys = []
_zip (x:xs) (y:ys) = (x, y) : _zip xs ys

_encode :: (Eq a) => [a] -> [(Int, a)]
_encode [] = []
_encode xs = _zip (map _length _elements) (map head _elements)
    where _elements = _pack xs
