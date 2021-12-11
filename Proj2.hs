{-| Author: Shiqi (Daniel) Lin <sllin2@student.unimelb.edu.au>
    Purpose: To provide a set of tools for simulating and solving a game 
    of "Musician"
    
    Description: This module provides a set of functions to simulate and play 
    the game of "Musician", and also contains an algorithm for solving the game.
    The algorithm has two parts, initialGuess and nextGuess; to use them, call
    initial guess at the first guess, and then call nextGuess repeatedly until
    the answer is found. The algorithm uses a GameState type to store the
    remaining possible targets and is passed from guess to guess.

    Musician is a logical guessing game where one player, the composer, comes up
    with a Chord as the target, and another player, the performer, tries to 
    guess the Chord. The performer receives feedback after each guess, which 
    includes the number of correct Pitches, the number of Pitches with the
    correct Notes but the wrong Octave, and the number of Pitches with the 
    correct Octave but the wrong Note.
    
    For a detailed description of the Musician game, see section 5. Appendix

    Section Outline: 
    1. Data Types
    2. Main Functions
    3. Helper Functions
    4. Debugging/Misc Tools
    5. Appendix
-}

module Proj2 (Pitch, toPitch, feedback,
              GameState, initialGuess, nextGuess) where
                
import Data.List as L
import Data.Map.Strict as M

-- | constants
chordLen = 3
numNotes = 7
numOctaves = 3


-------------------------------1. Data Types------------------------------------


{-| Each target and guess in Musician is a Chord.
    A Chord is represented as a list of 3 Pitches but not explicitly defined,
    A Pitch consists of a Note and an Octave,
    A Note is one of A, B, C, D, E, F, G
    An Octave is one of 1, 2, 3, note that here we use O1, O2, O3
-}

{-| GameState will store the remaining possible targets and be passed between
each guess.-}
type GameState = ([[Pitch]])

-- | A chord in the game is represented as a list of Pitch's
data Pitch = Pitch Note Octave deriving (Eq, Ord)
instance Show Pitch where
    show (Pitch note octave) = (show note) ++ (show octave)

data Note = A | B | C | D | E | F | G deriving (Show, Eq, Ord, Enum)

data Octave = O1 | O2 | O3 deriving (Eq, Ord, Enum)
instance Show Octave where
    show (O1) = "1"
    show (O2) = "2"
    show (O3) = "3"


------------------------------2. Main Functions---------------------------------


{-| initialGuess is called as the first guess, it gives a 
(Guess, GameState) where Guess is a list of Pithches 
Note: initialGuess is designed to eliminate the most number of possibilities,
found using testInitialGuess, see section 4. Debugging/Misc Tools -}
initialGuess :: ([Pitch],GameState)
initialGuess = ([(Pitch A O1), (Pitch B O1), (Pitch C O2)], 
                (allCombPitches chordLen))

{-| nextGuess is called repeatedly until the answer is found.
    It does so by finding the expected number of remaining targets of each 
    remaining target if it was the next guess, then choose the guess with the 
    lowest expectation. -}
nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)
nextGuess (prevGuess,gameState) score = (guess, (remaining))
  where
    (possibilities) = gameState
    remaining = L.filter ((==score).(flip feedback prevGuess)) possibilities
    (_, guess) = (minimum.L.map (\x -> (avgRemaining x remaining, x))) remaining

{-| feedback takes a target Chord and a guess Chord and provides feedback in
the form of a 3-tuple.
Note that the calculation of correctNote and correctOctave do not include 
Pitches that are already counted as correct (in correctPitch).
e.g. feedback [A1 B2 A3] [A1 A2 B1] = (1,2,1)-}
feedback :: [Pitch] -> [Pitch] -> (Int,Int,Int)
feedback target guess = (correctPitch, correctNote, correctOctave)
  where 
    commonPitches = intersect target guess
    remainingT = target L.\\ commonPitches  -- ^ remainingTargetes
    remainingG = guess L.\\ commonPitches  -- ^ remainingGuesses
    correctPitch = length commonPitches
    correctNote = countWith noteVal numNotes remainingT remainingG 
    correctOctave = countWith octaveVal numOctaves remainingT remainingG 


----------------------------3. Helper Functions---------------------------------


{-| Takes a hash function that identifies how a Pitch is counted, the max hash
value, two lists of Pitch and returns the number of Pitches in each list that 
have a corresponding Pitch in the other list with the same hash value. This is 
used in feedback to count the number of Pitches with the same Octave or the same
Note. 
hash functions defined in this module: noteVal, octaveVal
e.g. 
countWith noteVal [A1, B2, B3] [A3, G2, A2] = 1
countWith octaveVal [A1, B2, B3] [A3, G2, A2] = 2 -}
countWith :: (Pitch -> Int) -> Int -> [Pitch] -> [Pitch] -> Int
countWith hashPitch maxHash xs ys =
    let xsCountMap = buildMap hashPitch xs (initMap maxHash)
        ysCountMap = buildMap hashPitch ys (initMap maxHash)
    in
        sum (zipWith (min) (M.elems xsCountMap) (M.elems ysCountMap))

-- creates a Map of size specified, with all values being 0        
initMap :: Int -> (Map Int Int)
initMap 0 = empty
initMap size = M.insert size 0 (initMap (size - 1))

{-| update the values in the map based on a list of Pitch and a function that 
maps a Pitch to a Key -}
buildMap :: (Pitch -> Int) -> [Pitch] -> (Map Int Int) -> (Map Int Int)
buildMap hashPitch [] hashMap = hashMap
buildMap hashPitch (p:ps) hashMap = 
    adjust (+1) (hashPitch p) (buildMap hashPitch ps hashMap)

-- | * hash functions used in conjunction with buildMap / countWith

-- | noteVal hashes a Pitch based on its Note
noteVal :: Pitch -> Int
noteVal (Pitch note _)
    | note == A = 1
    | note == B = 2
    | note == C = 3
    | note == D = 4
    | note == E = 5
    | note == F = 6
    | note == G = 7
    | otherwise = error "value error: note value is not defined"
        
-- | octaveVal hashes a Pitch based on its Octave
octaveVal :: Pitch -> Int
octaveVal (Pitch _ octave)
    | octave == O1 = 1
    | octave == O2 = 2
    | octave == O3 = 3
    | otherwise = error "value error: octave value is not defined"

-- | * Helper functions to use with the data types

{-| toPitch turns a String into a Maybe Pitch,
is Nothing if the format is undefined
format: toPitch "{N}{O}" where {N} is the Note, {O} is the Octave
e.g. toPitch "A1", toPitch "B3"-}
toPitch :: String -> Maybe Pitch
toPitch (noteChar:octaveChar:[]) = 
    case (toNote noteChar, toOctave octaveChar) of
    (Just note, Just octave) -> Just (Pitch note octave)
    _ -> Nothing

{-| toOctave turns a String into a Maybe Octave,
is Nothing if the format is undefined-}
toOctave :: Char -> Maybe Octave
toOctave c
    | c == '1' = Just O1
    | c == '2' = Just O2
    | c == '3' = Just O3
    | otherwise = Nothing

{-| toNote turns a String into a Maybe Note,
is Nothing if the format is undefined-}
toNote :: Char -> Maybe Note
toNote c
    | c == 'A' = Just A
    | c == 'B' = Just B
    | c == 'C' = Just C
    | c == 'D' = Just D
    | c == 'E' = Just E
    | c == 'F' = Just F
    | c == 'G' = Just G
    | otherwise = Nothing

-- | determines if two Pitch have the same Note
sameNote :: Pitch -> Pitch -> Bool
sameNote (Pitch n1 _) (Pitch n2 _) = (n1 == n2)

-- | determines if two Pitch have the same Octave
sameOctave :: Pitch -> Pitch -> Bool
sameOctave (Pitch _ o1) (Pitch _ o2) = (o1 == o2)

{-| generates the list of all possible combinations of Pitches, must specify 
the length of such a combination
e.g. allCombPitches 2 = [[A1, A2], [A1, A3], [A1, B1]......]-}
allCombPitches :: Int -> [[Pitch]]
allCombPitches len = ((L.filter ((==len).length)).L.subsequences) allPitches

-- | generates the list of all possible Pitches [A1, A2, A3, B1......]
allPitches = [Pitch note octave | note <- [(A)..(G)], octave <- [(O1)..(O3)]]

{-| takes a guess and a list of possible targets and gives the expected number 
of remaining possibilities -}
avgRemaining :: (Fractional a) => [Pitch] -> [[Pitch]] -> a
avgRemaining guess possTargets = 
    (average.collect.L.map (flip feedback guess)) possTargets

{-| produces a list of counts of equal items. Note that the order of the result 
corresponds to the order of the items when sorted
e.g. 
collect [1,2,1,1,3,2] = [3,2,1]
collect ["A", "D", "G", "B", "B", "A"] = [2, 2, 1, 1] -}
collect :: Ord a => [a] -> [Int] 
collect xs = ((L.map length).group.sort) xs

-- | computes the average value of a list of Integrals
average :: (Integral a, Fractional b) => [a] -> b
average xs = fromIntegral (sum xs) / fromIntegral (length xs)


---------------------------Debugging/Misc Tools---------------------------------


{-| tests the algorithm all possible targets and print the average, most, and
least numer of guesses taken to solve the game-}
testProgram :: IO()
testProgram = putStrLn result
  where
    trials = L.map guessTest (allCombPitches chordLen)
    avg = average trials
    most = maximum trials
    least = minimum trials
    result = "Average = " ++ show avg ++ "\n max = " ++ show most ++
             "\n least = " ++ show least

{-| tests all possible initial guesses to find the one that reduces the most
number of possibilities on average. -}
testInitialGuess :: IO()
testInitialGuess = putStrLn result
  where
    result = (show.minimum) (L.map (\x -> (avgRemaining x 
             (allCombPitches chordLen), x)) (allCombPitches chordLen))


{-| the following code was adapted from Peter Schachte's COMP30020 Project 2 
assessment program, changed to give the number of guesses instead of printing it
they are used for debugging purposes only.
contact Peter Schachete at <schachte@unimelb.edu.au>
-}

-- | Guess the given target, returns the number of guesses taken
guessTest :: [Pitch] -> Int
guessTest target = do
    let (guess,other) = initialGuess
    loop target guess other 1
      
loop :: [Pitch] -> [Pitch] -> GameState -> Int -> Int
loop target guess other guesses
  | answer == (3,0,0) = guesses
  | otherwise = loop target guess' other' (guesses+1)
         where answer = feedback target guess
               (guess',other') = nextGuess (guess,other) answer
               
-----------------------------5. Appendix----------------------------------------
{-| Description of Musician
    Author: Peter Schachte <schachte@unimelb.edu.au>
    
    The Game of Musician
    Musician is a two-player logical guessing game.
    For a Musician game, one player is the composer and the other is the 
    performer. The composer begins by selecting a three-pitch musical chord, 
    where each pitch comprises a musical note, one of A, B, C, D, E, F, or G, 
    and an octave, one of 1, 2, or 3. This chord will be the target for the 
    game. The order of pitches in the target is irrelevant, and no pitch may 
    appear more than once. This game does not include sharps or flats, and no 
    more or less than three notes may be included in the target.

    Once the composer has selected the target chord, the performer repeatedly 
    chooses a similarly defined chord as a guess and tells it to the composer, 
    who responds by giving the performer the following feedback:

    how many pitches in the guess are included in the target (correct pitches)
    how many pitches have the right note but the wrong octave (correct notes)
    how many pitches have the right octave but the wrong note (correct octaves)
    In counting correct notes and octaves, multiple occurrences in the guess 
    are only counted as correct if they also appear repeatedly in the target. 
    Correct pitches are not also counted as correct notes and octaves. For 
    example, with a target of A1, B2, A3, a guess of A1, A2, B1 would be counted
    as 1 correct pitch (A1), two correct notes (A2, B1) and one correct octave 
    (A2). B1 would not be counted as a correct octave, even though it has the 
    same octave as the target A1, because the target A1 was already used to 
    count the guess A1 as a correct pitch. A few more examples:
    
    Target      Guess       Answer
    A1,B2,A3    A1,A2,B1    1,2,1
    A1,B2,C3    A1,A2,A3    1,0,2
    A1,B1,C1    A2,D1,E1    0,1,2
    A3,B2,C1    C3,A2,B1    0,3,3
    The game finishes once the performer guesses the correct chord (all three 
    pitches in the guess are in the target). The object of the game for the 
    performer is to find the target with the fewest possible guesses.
-}
