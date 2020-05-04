-- -------------------------------------------------------------------------- --
-- String Alignment --------------------------------------------------------- --
-- Code written by: --------------------------------------------------------- --
-- Joel Bäcker (jo4383ba-s) ------------------------------------------------- --
-- Victor Winkelmann (and vi6253wi-s) --------------------------------------- --
-- -------------------------------------------------------------------------- --

type AlignmentType = (String,String)

scoreMatch = 0
scoreMismatch = 0
scoreSpace = 0

-- Input: Two strings s and t, and values for scoreMatch, scoreMismatch, and scoreSpace.
-- Output: All optimal alignments between s and t.
optimalAlignments :: Int -> Int -> Int -> String -> String -> [AlignmentType]
optimalAlignments scoreMa scoreMi scoreSp s t = do
    let scoreMatch = scoreMa
    let scoreMismatch = scoreMi
    let scoreSpace = scoreSp
    similarityScore s t 
    ("hej", "hej")


-- Helper functions to similarityScore
sim :: [a] -> [a] -> Int
sim (x:xs) (y:ys) = max [sim xs ys + score x y, 
                          sim xs (y:ys) + score x '-', 
                          sim (x:xs) ys + score '-' y]

-- sim ((x:xs),(y:ys)) = sim (xs,ys) + score (x,y)
score :: a -> a -> Int
score c1 c2 
    | c1 == '-' = scoreSpace
    | c2 == '-' = scoreSpace
    | c1 == c2 = scoreMatch
    | otherwise = scoreMismatch

-- returns the score of the optimal alignment of the two strings
similarityScore :: String -> String -> Int
similarityScore string1 string2 = sim string1 string2


-- What does this function do?
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]


-- returns all maximum values in a list given by a function
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy _ _ = []
-- maximaBy valueFcn xs


-- returns a list of all optimal alignments between string1 and string2
optAlignments :: String -> String -> [AlignmentType]
optAlignments _ _ = []
-- optAlignments string1 string2


-- should output to the screen in a readable fashion
-- outputOptAlignments string1 string2

-------------------------------------------------------------------
