-- -------------------------------------------------------------------------- --
-- String Alignment --------------------------------------------------------- --
-- Code written by: --------------------------------------------------------- --
-- Joel Bäcker (jo4383ba-s) ------------------------------------------------- --
-- Victor Winkelmann (and vi6253wi-s) --------------------------------------- --
-- -------------------------------------------------------------------------- --

-- Which parts of your code / functions were the hardest to write and why?,
-- Which parts of your code / functions are you the most proud of,

-- -------------------------------------------------------------------------- --

module StringAlignment where
import Data.Char

-- Global variables and types ----------------------------------------------- --
type AlignmentType = (String,String)

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1
string1 = "writers"
string2 = "vintner"
--------------------------------------------------------------------------------

-- Input: Two strings s and t, and values for scoreMatch, scoreMismatch,
-- and scoreSpace.
-- Output: All optimal alignments between s and t.
optimalAlignments :: Int -> Int -> Int -> String -> String -> [AlignmentType]
optimalAlignments scoreMa scoreMi scoreSp s t = []


-- 2a.
-- returns the score of the optimal alignment of the two strings
similarityScore :: String -> String -> Int
similarityScore string1 string2 = sim string1 string2

-- Helper functions to similarityScore
sim :: [Char] -> [Char] -> Int
sim [] [] = 0
sim (x:xs) [] = scoreSpace
sim [] (y:ys) = scoreSpace
sim (x:xs) (y:ys) = maximum [sim xs ys + (score x y),
                             sim xs (y:ys) + (score x '-'),
                             sim (x:xs) ys + (score '-' y)]


-- sim ((x:xs),(y:ys)) = sim (xs,ys) + score (x,y)
score :: Char -> Char -> Int
score '-' _ = scoreSpace
score _ '-' = scoreSpace
score c1 c2
    | c1 == c2 = scoreMatch
    | otherwise = scoreMismatch


-- 2b.
-- What does this function do?
-- The function takes two heads and a list with tuples, the elements in the
-- tupels are also lists. For all elements in the list it adds the first head to
--  the first element in the tuple as a new head to that list. It also does the
-- same for the second element in all the tuples but with the second head.
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]


-- 2c.
-- returns all maximum values in a list given by a function
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = [a | a <- xs, valueFcn a == maxList] where
    maxList = maximum $ map valueFcn xs

-- 2d.
-- returns a list of all optimal alignments between string1 and string2
optAlignments :: String -> String -> [AlignmentType]
optAlignments [] _ = []
optAlignments _ [] = []
optAlignments xs ys = maximaBy makeScore (optW xs ys)

optW :: String -> String -> [AlignmentType]
optW [] [] = [("","")]
optW (x:xs) [] = attachHeads x '-' (optW xs [])
optW [] (y:ys) = attachHeads '-' y (optW [] ys)
optW (x:xs) (y:ys) = concat [ attachHeads x   y (optW xs ys),
                              attachHeads '-' y (optW (x:xs) ys),
                              attachHeads x '-' (optW xs (y:ys))]

-- helper function to opt, calculates the score of two words
makeScore :: AlignmentType -> Int
makeScore ([], _) = 0
makeScore (_, []) = 0
makeScore ((x:xs), (y:ys)) = score x y  + makeScore (xs, ys)


-- 2e.
-- should output to the screen in a readable fashion
outputOptAlignments :: String -> String -> IO ()
outputOptAlignments s1 s2 = do
    let optAli = optAlignments s1 s2
    putStrLn ("There are " ++ (show $ length optAli) ++ " optimal alignments:\n\n")
    putStrLn (printAll optAli)
    putStrLn ("There were " ++ (show $ length optAli) ++ " optimal alignments!")
      where printAll optAli = foldl1 (++) (map printFormat optAli)

printFormat :: AlignmentType -> String
printFormat at = ((fst at) ++ "\n" ++ (snd at) ++ "\n\n")

-------------------------------------------------------------------

-- 3.
type OptAlignmentType = (Int, [AlignmentType])

-- need to optimise similarityScore and optAlignment

fastSimilarityScore :: String -> String -> Int

fastOptAlignment :: String -> String -> OptAlignmentType
