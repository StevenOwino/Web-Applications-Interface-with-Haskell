-- file: Determine the Godel Number of a term of a context-free language by enumeration ordering
-- Proof is an Equality
-- imports/extensions, a data type representing the grammar, and a pretty-printer
-- APPROACH:
-- Code a formula as a list of Natural Numbers, then
-- Code that list using Cantors' pairing function to index into infinite list of inhabitants


{-# LANGUAGE TypeSynonymInstances #-}
import Control.Applicative
import Data.Universe.Helpers

type S      = Add
data Add    = Mul Mul       | Add :+ Mul       deriving (Eq, Ord, Show, Read)
data Mul    = Term   Term   | Mul :* Term      deriving (Eq, Ord, Show, Read)
data Term   = Number Number | Parentheses S    deriving (Eq, Ord, Show, Read)
data Number = Digit  Digit  | Digit ::: Number deriving (Eq, Ord, Show, Read)
data Digit  = D0 | D1 | D2                     deriving (Eq, Ord, Show, Read, Bounded, Enum)

class PP a where pp :: a -> String
instance PP Add where
    pp (Mul m) = pp m
    pp (a :+ m) = pp a ++ "+" ++ pp m
instance PP Mul where
    pp (Term t) = pp t
    pp (m :* t) = pp m ++ "*" ++ pp t
instance PP Term where
    pp (Number n) = pp n
    pp (Parentheses s) = "(" ++ pp s ++ ")"
instance PP Number where
    pp (Digit d) = pp d
    pp (d ::: n) = pp d ++ pp n
instance PP Digit where pp = show . fromEnum

-- let's define the enumeration order. We'll use two basic combinators:
-- +++ for interleaving two lists (mnemonic: the middle character is a sum,
-- so we're taking elements from either the first argument or the second) and
-- +*+ for the diagonalization (mnemonic: the middle character is a product, so we're taking elements
-- from both the first and second arguments)
-- One invariant we'll maintain is that our lists 
-- with the exception of digits -- are always infinite. This will be important later.

ss = adds
adds =  (Mul <$> muls      )    +++ (uncurry (:+)  <$> adds +*+ muls)
muls  = (Term   <$> terms  )    +++ (uncurry (:*)  <$> muls +*+ terms)
terms = (Number <$> numbers)    +++ (Parentheses   <$> ss)
numbers = (Digit <$> digits)    ++ interleave [[d ::: n | n <- numbers] | d <- digits]
digits  = [D0, D1, D2]

-- Lets see a few terms: In GHCI , 
-- *Main> mapM_ (putStrLn . pp) (take 15 ss)
-- Let's assume we have two infinite lists a and b.
-- First, in a +++ b, all the even indices come from a, and all the odd indices come from b.
-- So we can look at the last bit of an index to see which list to look in,
-- and the remaining bits to pick an index in that list.
-- Second, in a +*+ b, we can use the standard bijection between pairs of numbers and 
-- single numbers to translate between indices in the big list and pairs of indices in the a and b lists
-- We'll define a class for Godel-able things that can be translated back and forth between numbers
-- indices into the infinite list of inhabitants.
--  Later we'll check that this translation matches the enumeration we defined above.

type Nat = Integer 
class Godel a where
    to :: a -> Nat
    from  :: Nat -> a

instance Godel Nat where to = id; from = id

instance (Godel a, Godel b) => Godel (a, b) where
    to (m_, n_) = (m + n) * (m + n + 1) `quot` 2 + m where
        m = to m_
        n = to n_
    from p = (from m, from n) where
        issqrt     = floor . sqrt . fromIntegral
        base       = (issqrt (1 + 8 * p) - 1) `quot` 2
        triangle   = base * (base + 1) `quot` 2
        m = p - triangle
        n = base - m
-- The instance for pairs here is the standard Cantor diagonal.
-- Use the triangle numbers to figure out where you're going/coming from using algebra.
-- We now build up instances for this class. Numbers are represented in base 3.


-- This instance is a lie! there aren't infinitely many Digits.
-- But we'll be careful about how we use it

instance Godel Digit where
    to   = fromIntegral . fromEnum
    from = toEnum . fromIntegral

instance Godel Number where
    to (Digit d) = to d
    to (d ::: n) = 3 + to d + 3 * to n
    from n
        | n < 3     = Digit (from n)
        | otherwise = let (q, r) = quotRem (n - 3) 3 in from r ::: from q

-- For the remaining three types, we will, as suggested above, 
-- check the tag bit to decide which constructor to emit,
-- and use the remaining bits as indices into a diagonalized list
-- All three instances necessarily look very similar.

instance Godel Term where
    to (Number n)      = 2 * to n
    to (Parentheses s) = 1 + 2 * to s
    from n = case quotRem n 2 of
        (q, 0) -> Number (from q)
        (q, 1) -> Parentheses (from q)

instance Godel Mul where
    to (Term t) = 2 * to t
    to (m :* t) = 1 + 2 * to (m, t)
    from n = case quotRem n 2 of
        (q, 0) -> Term (from q)
        (q, 1) -> uncurry (:*) (from q)

instance Godel Add where
    to (Mul m) = 2 * to m
    to (m :+ t) = 1 + 2 * to (m, t)
    from n = case quotRem n 2 of
        (q, 0) -> Mul (from q)
        (q, 1) -> uncurry (:+) (from q)

-- And that's it!
-- We can now "efficiently" translate back and forth between parse trees 
-- and their Godel numbering for this grammar. 
-- Moreover, this translation matches the above enumeration, as you can verify:
-- *Main> map from [0..29] == take 30 ss ; PROOF THAT THE FUNCTION WORKS

-- PROPERTIES of this particular grammar: 
-- nonterminals had infinitely many derivations
-- except for the instance for (Nat, Nat), 
-- these Godel numberings look at/produce one bit (or trit) at a time. 
-- So you could imagine doing some streaming.
-- But the (Nat, Nat) one is pretty nasty:
-- You have to know the whole number ahead of time to compute the sqrt.
-- You actually can turn this into a streaming, too, without losing the property of being dense 
-- (every Nat being associated with a unique (Nat, Nat)),
   
-- REFERENCE: Essential Incompletenes of Arithmetic Verified by Coq -- Russel O'Connor    









