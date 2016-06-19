data List a = Cons a (List a) 
            | Nil
            deriving (Show)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : fromList xs

data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a)) 
            deriving (Show)

myLength :: [a] -> Integer
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

mean :: (Integral a, Fractional b) => [a] -> b
mean [] = 0
mean l = (fromIntegral (sum l)) / (fromIntegral (length l))

listToPalindrome :: [a] -> [a]
listToPalindrome [] = []
listToPalindrome (x:xs) = x : (listToPalindrome xs) ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = False
isPalindrome [x] = True
isPalindrome [x,y] = x == y 
--isPalindrome [x,y,z] = x == z 
isPalindrome (x:xs) = (x == last xs) && isPalindrome (init xs)

