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

t1fiboN = fiboN 0 == 0
t2fiboN = fiboN 5 == 5
t3fiboN = fiboN 1 == 1
        
testfiboN = [t1fiboN,t2fiboN,t3fiboN]
------------------------------------------------------------
------------------ 1.2 Fibonacci Stream --------------------
------------------------------------------------------------

{--
fiboStream function:
calls the helpStream function, parsing 0 1 to start the sequences
        --access using fiboStream!!n
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

t1helperStream = helperStream 0 1 /= []
testhelperStream = [t1helperStream]
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

{--
Dict type takes key and value, here it can either be an empty dict
or a Dict that takes a list of keys and a list of values
--}

data Dict key value = EmptyDict | Dict [key] [value]

{--
This emptyDict is extracted from (originally a stack implementation)
--}

emptyDict :: Dict a b
emptyDict = EmptyDict

{--
hasKey function:
-- hasKey takes 3 parameters, the keys, values and key we are checking
-- Empty list check
-- Empty list of keys that contain items still, return false BECAUSE IT DOESNT HAVE ANY KEYS
-- Simple loop of the list of keys, recursing and keeping the value and checking to see if the key exists
-- if it does return true, if not keep recursing on the list with the rest of the list
--}

hasKey :: Eq a => Dict a b -> a -> Bool
hasKey EmptyDict a = False 
hasKey (Dict [] x) y = False 
hasKey (Dict (x:xs) y) z
                       | x /= z = hasKey (Dict xs y) z
                       | otherwise = True

getValue :: Eq a => Dict a b -> a -> b
getValue EmptyDict a = error "Empty"
getValue (Dict [] y) a = error "Key is not present"
getValue (Dict (x:xs) (y:ys)) a
                              | x == a = y
                              | otherwise = getValue (Dict xs ys) a

withKeyValue :: Eq a => Dict a b -> a -> b -> Dict a b
withKeyValue (Dict x y) a b
                          | (hasKey (Dict x y) a) == True = withKeyValue (withoutKey (Dict x y) a) a b
                          | otherwise = Dict (x++[a]) (y++[b])

withoutKey :: Eq a => Dict a b -> a -> Dict a b
withoutKey (Dict (x:xs) (y:ys)) a
                                | x == a = Dict xs ys
                                | (hasKey (Dict xs ys) a) == True = withoutKey (Dict (xs++[x]) (ys++[y])) a 
                                | otherwise = error "Key is not present"


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