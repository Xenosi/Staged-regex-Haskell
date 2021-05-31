{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module Regex where

--import Data.String ()
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Language.Haskell.TH.Syntax



----------------------------
-- DATA TYPES & INSTANCES --
----------------------------


-- |A datatype which represents a regular expression.
--
-- There are 3 basic types of regex:
-- The empty set, the string of length zero, and a literal character.
--
-- We can combine these basic types into more advanced types with:
-- Concatenation, Iteration (Kleene star) or a Boolean function of 1 or 2 regex.
data Regex = Chr Char | Emp | Zero
           | Conc Regex Regex | Iter Regex | Func BoolFunc
            deriving Eq

-- |A datatype which represents the different possible Boolean functions a
-- regex can consist of.
--
-- The 3 available functions are Union, Intersection and Complement.
data BoolFunc = Union Regex Regex
              | Inter Regex Regex
              | Compl Regex
              deriving Eq


-- A Deterministic Finite Automaton is usually defined as a 5-tuple (Q,S,d,s,F):
--
-- Q is a set/collection of States.
-- S is the collection of characters which are used to make strings
--      but is omitted here because we implicitly define the alphabet to be
--      all 26 lowercase letters from the latin alphabet.
-- d is the delta/transition function, which represents every edge in the DFA.
--      It takes a state and a character as input and returns the state you reach
--      when the DFA is in the given state and the next character of the string
--      to be consumed is the given one.
--      We implement this function as a Map.
-- s is the state the DFA starts in.
-- F is the set/collection of accepting states.
data DFA = Auto (Set State) (Map (State, Char) State) State (Set State)


-- We define a State of a DFA by a regex.  
newtype State = MkState Regex


instance Show DFA where
    show (Auto q m s0 f) = "States:\n" ++
                           showSet q ++ "\n" ++
                           "Delta function:\n" ++
                           show m ++ "\n\n" ++
                           "Start state:\n" ++
                           show s0 ++ "\n\n" ++
                           "Accepting states:\n" ++
                           showSet f

-- A simple function which displays all the elements in the given order,
-- each on a new line. 
showSet :: Show a => Ord a => Set a -> String
showSet set
    | Set.size set == 0 = ""
    | otherwise = show x ++ "\n" ++ showSet (Set.deleteAt 0 set)
    where x = Set.elemAt 0 set

instance Show Regex where
    show (Chr a) = [a]
    show Emp = "φ"
    show Zero = "λ"
    show (Conc a b) = show a ++ show b
    show (Iter a) = "(" ++ show a ++ ")*"
    show (Func b) = show b

instance Show BoolFunc where
    show (Union a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Inter a b) = "(" ++ show a ++ " & " ++ show b ++ ")"
    show (Compl a)   = "(" ++ show a ++ ")'"

instance Ord Regex where
    a <= b = show a <= show b

instance Show State where
    show (MkState r) = "State " ++ show r

instance Eq State where
    MkState a == MkState b = reduce a == reduce b

instance Ord State where
    MkState a <= MkState b = reduce a <= reduce b


(<+>) :: Regex -> Regex
(<+>) r = Conc r (Iter r)

(<**>) :: Regex -> Regex
(<**>) = Iter

(<?>) :: Regex -> Regex
(<?>) r = Func (Union r Zero)

(<|>) :: Regex -> Regex -> Regex
a <|> b = Func (Union a b)

conc :: [Regex] -> Regex
conc = reduce . foldr Conc Zero


alphabet :: [Char]
alphabet = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

----------------------
-- REGEX FUNCTIONS  --
----------------------


-- |Creates a basic regex from a String, with support for
-- literal characters, and symbols for the empty string an the empty set.
strToRegex :: String -> Regex
strToRegex []     = Zero
strToRegex (x:xs)
    | x == 'φ' = reduce $ Conc Emp (strToRegex xs)
    | x == 'λ' = reduce $ Conc Zero (strToRegex xs)
    | otherwise = reduce $ Conc (Chr x) (strToRegex xs)


-- |Caclulates the Brzozowski derivative of a specific regex with respect to a
-- character. The given regex is first reduced to eliminate any unnecessary
-- parts and make calculating the derivative easier, which is done afterwards.
-- Lastly, we reduce the calculated regex again, to simplify possible newly
-- created redundancies.
--
-- The derivative of a regex with respect to a String can be found in the
-- function 'deriveString'
derivative :: Regex -> Char -> Regex
derivative r y =
    case reduce r of
        Emp              -> Emp
        Zero             -> Emp
        Chr a            -> if a == y then Zero else Emp
        Conc a b         -> reduce $ Func (Union (Conc (derivative a y) b) (Conc (nullable a) (derivative b y)))
        Iter r2          -> reduce $ Conc (derivative r2 y) (Iter r2)
        Func (Union a b) -> reduce $ Func (Union (derivative a y) (derivative b y))
        Func (Inter a b) -> reduce $ Func (Inter (derivative a y) (derivative b y))
        Func (Compl a)   -> reduce $ Func (Compl (derivative a y))


-- |Reduces the regex to a certain normal form
-- according to a number of identities, with bottom-up structural recursion.
-- 
-- We define the normal form for multiple of the same operation
-- as one on the left, and the rest to the right. This is done mostly to make
-- calculating derivatives easier and because of reading direction.
reduce :: Regex -> Regex
reduce Emp = Emp
reduce Zero = Zero
reduce (Chr x) = Chr x
reduce (Conc (Conc a b) c) = Conc (reduce a) (reduce (Conc b c))
reduce (Func (Union (Func (Union a b)) c)) = Func (Union (reduce a) (Func (Union (reduce b) (reduce c))))
reduce (Func (Inter (Func (Inter a b)) c)) = Func (Inter (reduce a) (Func (Inter (reduce b) (reduce c))))
reduce (Func (Compl (Func (Compl a)))) = reduce a
reduce (Conc a b)
    | Emp <- x = Emp
    | Emp <- y = Emp
    | Zero <- x = y
    | Zero <- y = x
    | otherwise = Conc x y
    where x = reduce a
          y = reduce b
reduce (Iter a)
    | Emp <- x = Emp
    | Zero <- x = Zero
    | otherwise = Iter x
    where x = reduce a
reduce (Func (Union a b))
    | x == y = x
    | Emp <- x = y
    | Emp <- y = x
    | otherwise = Func (Union x y)
    where x = reduce a
          y = reduce b
reduce (Func (Inter a b))
    | x == y = x
    | Emp <- x = Emp
    | Emp <- y = Emp
    | otherwise = Func (Inter x y)
    where x = reduce a
          y = reduce b
reduce (Func (Compl a)) = Func (Compl (reduce a))


-- |Calculates the derivative of a regex with respect to a String, by
-- calculating the derivatives of every character in the String from left to
-- right.
--
-- See also: 'derivative'
deriveString :: Regex -> String -> Regex
deriveString = foldl derivative


-- |Checks if the given regex contains the empty string.
nullable :: Regex -> Regex
nullable r =
    case reduce r of
        Chr _            -> Emp
        Emp              -> Emp
        Zero             -> Zero
        Conc a b         -> reduce $ Func (Inter (nullable a) (nullable b))
        Iter _           -> Zero
        Func (Union a b) -> reduce $ Func (Union (nullable a) (nullable b))
        Func (Inter a b) -> reduce $ Func (Inter (nullable a) (nullable b))
        Func (Compl a)   -> if (reduce . nullable) a == Zero then Emp else Zero


-- |Tries to match a String against a regex, and returns a 'Bool' with the
-- result. This is done by deriving the regex with respect to the given String,
-- and seeing if the resulting regex is nullable.
match :: Regex -> String -> Bool
match r s = nullable (deriveString r s) == Zero


-- |Calculates all the characteristic derivatives of a given regex.
--
-- See also: 'derivSearch' and 'deriveSet'
charDerivatives :: Regex -> Set Regex
charDerivatives r = derivSearch (Set.singleton r)


-- |Keeps calculating the derivatives of all regex in the given set with
-- respect to a single character until no new regex are found.
derivSearch :: Set Regex -> Set Regex
derivSearch xs
    | Set.size xs == Set.size x = x
    | otherwise = derivSearch x
    where x = Set.union xs (deriveSet xs)


-- |Takes a set of regular expressions, and calculates all the derivatives of
-- each regex, with respect to a single character. This is then collapsed into
-- a single set, by folding the union function over the set of sets.
--
-- This doesn't support non-alphabetic characters yet, shouldn't be too much
-- trouble to add though.
deriveSet :: Set Regex -> Set Regex
deriveSet xs = foldl Set.union Set.empty (Set.map (\r -> Set.fromList [derivative r a | a <- alphabet]) xs)



-------------------
-- DFA FUNCTIONS --
-------------------


-- Takes a DFA and a State, and returns true if the given state accepts the
-- empty string (if the regex associated with that state contains it).
accepting :: DFA -> State -> Bool
accepting (Auto _ _ _ f) s = Set.member s f


-- This function takes a DFA and returns its starting state.
initial :: DFA -> State
initial (Auto _ _ s0 _) = s0


-- Takes a DFA, State and a character and returns the state which is the result
-- of the delta function for the tuple (state,char).
step :: DFA -> State -> Char -> State
step (Auto _ d _ _) s c = d Map.! (s, c)



----------------------
-- DFA CONSTRUCTION --
----------------------


-- This function takes a regex and constructs an equivalent DFA.
constructDFA :: Regex -> DFA
constructDFA r = Auto states delta s0 f
    where s0 = MkState (reduce r)
          (delta, states) = statesAndMap Map.empty (Set.singleton s0) Set.empty
          f = Set.filter (\(MkState r) -> nullable r == Zero) states

{- constructDFA' :: Regex -> DFA
constructDFA' r = Auto states delta s0 f
    where s0 = MkState (reduce r)
          (delta, states) = statesAndMap' Map.empty Set.empty (Set.singleton s0)
          f = Set.filter (\(MkState r) -> nullable r == Zero) states -}


-- This function calculates all the states and transitions between these states
-- as Map entries, when given a partially filled Map and an incomplete
-- Set of States belonging to the DFA.
--
-- It does this by taking a state for which there exist transitions which have
-- not been found yet (this is tracked by also giving a set of finished states
-- as an input argument), and computing all the states which can be reached 
-- from the chosen state, along with the transitions. These states and
-- transitions are then added to the Set of found States and the Map, and these
-- steps are repeated until there are no more States which haven't been finished.
statesAndMap :: Map (State, Char) State -> Set State -> Set State -> (Map (State, Char) State, Set State)
statesAndMap map found finished
    | Set.size found == Set.size finished = (map, found)
    | otherwise = statesAndMap newMap newFound newFinished
    where state = Set.elemAt 0 (Set.difference found finished)
          (newMap, states) = searchStep state map found
          newFound = Set.union found states
          newFinished = Set.insert state finished


{- statesAndMap' :: Map (State, Char) State -> Set State -> Set State -> (Map (State, Char) State, Set State)
statesAndMap' map finished todo
    | Set.size todo == 0 = (map, finished)
    | otherwise = statesAndMap' newMap newFinished newTodo
    where state = Set.elemAt 0 todo
          (newMap, states) = searchStep state map finished
          newFinished = Set.insert state finished
          newTodo = Set.union (Set.delete state todo) states -}


-- This function takes a State, a Map of transitions and a Set of States,
-- and computes all the single derivatives for the given State. These
-- derivatives are then added to the Set as States, along with the transitions
-- being added to the Map. A tuple of the new Map and Set is then returned.  
searchStep :: State -> Map (State, Char) State -> Set State -> (Map (State, Char) State, Set State)
searchStep s0 m set = (listInsert m cand2, newSet)
    where sd = singleDerivatives s0
          cand1 = filter (\a -> Set.notMember (fst a) set) sd
          cand2 = filter (\a -> Map.notMember (s0, snd a) m) sd

          newSet = Set.union set (Set.fromList (map fst cand1))

          listInsert :: Map (State, Char) State -> [(State,Char)] -> Map (State, Char) State
          listInsert m [] = m
          listInsert m (x:xs) = listInsert (Map.insert (s0, snd x) (fst x) m) xs


-- Takes a State and returns a list of tuples with the states you can reach
-- from the given one, along with the character you need to parse to go to this
-- new State.
singleDerivatives :: State -> [(State,Char)]
singleDerivatives (MkState r) = [ (MkState (derivative r a), a) | a <- alphabet ]


-- Takes a String and a DFA and checks if the DFA accepts the given String.
matchDFA :: DFA -> String -> Bool
matchDFA auto str = go auto str (initial auto)
    where go :: DFA -> String -> State -> Bool
          go _ _ (MkState Emp) = False
          go auto [] s = accepting auto s
          go auto (c:cs) s = go auto cs (step auto s c)


test1 :: Regex
-- test1 = conc [(<+>) $ Chr 'r', Chr 'f', (<**>) $ Chr 'U', Chr 'b', (<+>) $ Chr 'h', Chr 'j', (<+>) $ Chr 't'] 
test1 = (<+>) (Chr '8' <|> (Chr '9' <|> (Chr 'T' <|> (Chr 't' <|> Chr 'l'))))

states1 :: Set State
delta1 :: Map (State, Char) State
s01 :: State
acc1 :: Set State
(Auto states1 delta1 s01 acc1) = constructDFA test1
stateList1 :: [State]
stateList1 = Set.toList states1

alphabetMap :: Map State [Char]
alphabetMap = alphabetMap2 stateList1 Map.empty

alphabetMap2 :: [State] -> Map State [Char] -> Map State [Char]
alphabetMap2 [] m = m
alphabetMap2 (c:cs) m = alphabetMap2 cs newmap
    where newmap = Map.insert c [x | x <- alphabet, delta1 Map.! (c,x) /= MkState Emp ] m


acceptMap :: [State] -> Map State Exp -> Map State Exp
acceptMap cs m = foldl (\ m c -> Map.insert c (f c) m) m cs

f :: State -> Exp
f s
    | Set.member s acc1 = ConE 'True
    | otherwise         = ConE 'False

----------
-- TODO --
----------
-- show instance map
-- benchmarks regex
-- write code by hand that TH has to generate
-- code cleanup (Haskellify it)
-- non-hacky Regex ordering (total order on regex is arbitrary, maybe conc lowest, phi highest)
-- expand strToRegex with full support for string parsing to regex
-- expand deriveSet with all single characters
-- use QuickCheck
