------------------------------------------
--- String Alignment ---------------------
--- Code written by: ---------------------
--- Joel Bäcker (jo4383ba-s) -------------
--- Victor Winkelmann (and vi6253wi-s) ---
------------------------------------------



-- Input: Two strings s and t, and values for scoreMatch, scoreMismatch, and scoreSpace.
-- Output: All optimal alignments between s and t.
optimalAlignments :: Int -> Int -> Int -> String -> String -> [AlignmentType]
optimalAlignments _ _ _ _ _ = []


-- returns the score of the optimal alignment of the two strings
similarityScore :: String -> String -> Int
similarityScore _ _ = 0
--similarityScore string1 string2

-- What does this function do?
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

-- returns all maximum values in a list given by a function
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy _ _ = []
-- maximaBy valueFcn xs


type AlignmentType = (String,String)

-- returns a list of all optimal alignments between string1 and string2
optAlignments :: String -> String -> [AlignmentType]
optAlignments _ _ = []
-- optAlignments string1 string2


-- should output to the screen in a readable fashion
-- outputOptAlignments string1 string2

-------------------------------------------------------------------
