-- duple n x: create a list of n copies of x
-- E.g. duple 3 1 = [1,1,1]
duple :: Integer -> a -> [a]
duple n x = replicate (fromIntegral(n)) (x)

-- invert l: reverse all the two-element lists in l, leaving other
-- lists in l unchanged.
-- Example: invert [[1,2],[3,4,5],[6,7]] = [[2,1],[3,4,5],[7,6]]
invert :: [[a]] -> [[a]]
invert l = [ if length xz == 2 then reverse xz else xz | xz <- l]


-- triplize l: make 2 additional copies of each list element.
-- Example: triplize [1,2,3] = [1,1,1,2,2,2,3,3,3]
triplize :: [a] -> [a]
triplize [] = []
triplize l = (replicate 3 (head l)) ++ triplize (tail l)


-- listSet l n x: replace the n-th member of l by x; assume n >= 1.
-- Example: listSet ["a", "b", "c", "d"] 2 "e" = ["a","e","c","d"]
listSet :: [a] -> Integer -> a -> [a]
listSet l n x = 
	let (first',second') = splitAt ((fromIntegral(n))::Int) l
	in init (first') ++ [x] ++ second'


-- product xs ys: all pairs consisting of an element of xs followed by
-- an element of ys.
-- Example: prod [1,2,1] [3,4] = [(1,3),(1,4),(2,3),(1,3),(2,4),(1,4)]
-- The order of the pairs in the list doesn't matter.  You might want
-- to use ++, which appends two lists, e.g. [1,2]++[3,4] = [1,2,3,4]
prod :: [a] -> [b] -> [(a,b)]
prod xs ys = [ (x, y) | x <- xs, y <- ys]


-- down xs: make a singleton list of each member of xs
-- Example: [1,2,3] -> [[1],[2],[3]]
down :: [a] -> [[a]]
down [] = []
down [x] = [[x]]
down xs = [ [anElement] | anElement <- xs ]


-- union ls: the concatenation of all the lists in l
-- Example: union [[1,2,3], [4,5], [6]] = [1,2,3,4,5,6]
--union :: [[a]] -> [a]
union [] = []
union [[x]] = [x]
union ls = head ls ++ (union (tail ls))



