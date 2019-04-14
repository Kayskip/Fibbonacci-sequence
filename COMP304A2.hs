------------------------------------------------------------
------------------ 1.1 Recursive Function ------------------
------------------------------------------------------------

{---
fiboN function:
takes an integer an returns the fibo no.
function extracted from the lecture slides
--}

fiboN :: Integer -> Integer
fiboN 0 = 0
fiboN n | n == 0 = 0
        | n == 1 = 1
        | n > 1 = fiboN (n-1) + fiboN (n-2)
        | otherwise = error"fibo : Illegal Argument"

------------------------------------------------------------
------------------ 1.2 Fibonacci Stream --------------------
------------------------------------------------------------

{--
fiboStream function:
calls the helpStream function, parsing 0 1 to start the sequences
--}

fiboStream :: [Integer]
fiboStream = helperStream 0 1

{--
helperStream function:
takes 2 arguments with start at 0 1, returning the stream of integers,
function recurses on its self, parsing through the new parameters.
We parse through the second integer in the first position and plus
the 2 integers together parsing it through the second parameter position.
this results in the fibo sequence as a stream.
--}

helperStream :: Integer -> Integer -> [Integer]
helperStream a b = a : helperStream b (a + b)

------------------------------------------------------------
------------------ 1.3 Performance -------------------------
------------------------------------------------------------

{--
Performance of fiboN is significantly worse than the fiboStream.
fiboN relies on 2 recursive calls, which occur an exponential amount of times
during the calculation of your fibo number.

Where as fiboStream recurses the amount of times of the integer that
you require. for example, fiboStream!!55 will recurse 55 times.
Making the fiboStream significantly faster than fiboN, during the 
calculation stage of your fibo number.
--}

------------------------------------------------------------
------------------ 2 Dictionary Implementation -------------
------------------------------------------------------------

data Dict a b = EmptyDict | Top a b (Dict a b) 
emptyDict :: Dict a b
emptyDict = EmptyDict

{--
comp :: Eq a => (Potential a) -> (Potential a) -> Bool
comp (Value x) (Value y) = (x == y)

hasKey :: Eq a => Dict a b -> a -> Bool

--check if a given key is present in a dictionary
hasKey =
getValue :: Eq a => Dict a b -> a -> b
--get the value of a given key in a dictionary
withKeyValue :: Eq a => Dict a b -> a -> b -> Dict a b
--add a new key-value pair to a dictionary
withoutKey :: Eq a => Dict a b -> a -> Dict a b
--remove a key-value pair from a dictionary
--}

------------------------------------------------------------
------------------ Extra Code ------------------------------
------------------------------------------------------------

-- This is an accumalative fibo function I made to help me further understand
-- how I should approach the fibo stream. This runs just as fast as my stream version. 
-- This is not included in the final hand in

fiboAccumulative :: Integer -> Integer
fiboAccumulative = fiboA 0 1

fiboA :: Integer -> Integer -> Integer -> Integer
fiboA f1 f2 0 = f1
fiboA f1 f2 n 
        | n > 0 = fiboA (f2) (f1+f2) (n-1)