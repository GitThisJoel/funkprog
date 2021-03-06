module Chatterbot where
import Utilities
import System.Random
import Data.Char

-- hej jag tycker om dig din läckra strut
-- tjåfften de gay

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind bb = do
  r <- randomIO :: IO Float
  return $ rulesApply $ map (map2 (id, pick r)) bb

rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply trs phrs = concat $ transformationsApply "*" reflect trs phrs


reflect :: Phrase -> Phrase
reflect [] = []
reflect ps = [try (flip lookup reflections) p | p <- ps]

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = map $ map2 (words . map toLower, map words)

-- couldn't find another way to write this wihtout '.'


--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply reds = fix $ try $ transformationsApply "*" id reds


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute wc t s = concat [if c == wc then s else [c] | c <- t]



-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing

match wc (c1:p) (c2:s)
  | wc == c1 = singleWildcardMatch (c1:p) (c2:s) `orElse` longerWildcardMatch (c1:p) (c2:s)
  | c1 /= c2  = Nothing
  | otherwise = match wc p s

-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
-- singleWildcardMatch [] [] = Just []
-- singleWildcardMatch _ [] = Nothing
-- singleWildcardMatch [] _ = Nothing
singleWildcardMatch (wc:ps) (x:xs) = mmap (const [x]) (match wc ps xs)

-- longerWildcardMatch [] [] = Just []
-- longerWildcardMatch _ [] = Nothing
-- longerWildcardMatch [] _ = Nothing
longerWildcardMatch (wc:ps) (x:xs) = mmap (x :) (match wc (wc:ps) xs)



-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply _ _ [] _ = Nothing
transformationApply wc f s pres = mmap (substitute wc (snd pres)) (mmap f (match wc (fst pres) s)) -- mmap f (mmap ...)

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply wc f (p:pres) s =
  if (transformationApply wc f s p) /= Nothing then
    (transformationApply wc f s p)
  else
    transformationsApply wc f pres s

-- Denna lösning fungerar men jag tycker den är ful, finns det andra sätt att
-- lösa denna på. Eventuellt med typ `mmap`?
