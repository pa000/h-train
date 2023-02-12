-- |
-- Module    : System.Random.MRG32K3A.Simple
-- Copyright : (c) 2015 Mathias Koerner
-- License   : BSD3
--
-- Maintainer  : mkoerner@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Portable pseudo-random number generator with small state and
-- skip-ahead that is used on CPUs and GPUs. The pupose of the code
-- is to provide a reference to test faster generators and to generate
-- the skip matrices as currently none of the Haskell random number
-- generators provide a skip-ahead.
module Simple
  ( State,
    Step,
    advance,
    stepBy,
    seed,
    nextWith,
    next,

    -- * References
    -- $references
  )
where

import Data.Bits
import Data.Matrix

type HalfState = Matrix Integer

type HalfStep = Matrix Integer

-- | State of MRG32k3a pseudo-random number generator
newtype State = State (HalfState, HalfState)
  deriving (Show)

-- | Step of MRG32k3a pseudo-random number generator
newtype Step = Step (HalfStep, HalfStep)
  deriving (Show)

-- Constants required by the generator
m1, m2, a12, a13m, a13, a21, a23m, a23 :: Integer
m1 = 2 ^ 32 - 209
m2 = 2 ^ 32 - 22853
a12 = 1403580
a13m = -810728
a13 = m1 + a13m
a21 = 527612
a23m = -1370589
a23 = m2 + a23m

halfStep1, halfStep2 :: HalfStep
halfStep1 =
  fromLists
    [ [0, a12, a13],
      [1, 0, 0],
      [0, 1, 0]
    ]
halfStep2 =
  fromLists
    [ [a21, 0, a23],
      [1, 0, 0],
      [0, 1, 0]
    ]

gen :: Step
gen = Step (halfStep1, halfStep2)

-- Start states for the random number generator.
--
-- The seed gives the number of skip aheads to perform before
-- generating random numbers.
--
start1, start2 :: HalfState
start1 = fromList 3 1 [16 ..]
start2 = fromList 3 1 [512 ..]

start :: State
start = State (start1, start2)

(.*.) :: Num a => (Matrix a, Matrix a) -> (Matrix a, Matrix a) -> (Matrix a, Matrix a)
(x1, x2) .*. (y1, y2) = (x1 * y1, x2 * y2)

(.%.) :: Integral a => (Matrix a, Matrix a) -> (a, a) -> (Matrix a, Matrix a)
(x1, x2) .%. (m1, m2) = (fmap (flip mod m1) x1, fmap (flip mod m2) x2)

-- | Return step to jump ahead or for leapfrogging.
-- Provides an efficient way to generate the stepping information to
-- either advance the random number stream by @n@ number or to generate
-- every @n@-th number.
stepBy :: Integer -> Step
stepBy n = go n id gen
  where
    id = Step (identity 3, identity 3)
    go :: Integer -> Step -> Step -> Step
    go 0 accGen pow2Gen = accGen
    go n (Step accGen) (Step pow2Gen) = go (shiftR n 1) (Step accGen') (Step pow2Gen')
      where
        accGen' = if even n then accGen else (accGen .*. pow2Gen) .%. (m1, m2)
        pow2Gen' = (pow2Gen .*. pow2Gen) .%. (m1, m2)

-- | Advance the state by @n@ random number generation steps.
advance :: Integer -> State -> State
advance n s = snd $ nextWith (stepBy n) s

-- | Advance the default start state by @n@ random number generation steps.
seed :: Integer -> State
seed n = advance n start

-- | Generate a random number.
-- Generates the next random number with the given step. The random number
-- is in the range @0 .. m1 - 1@.
nextWith :: Step -> State -> (Integer, State)
nextWith (Step g) (State s) = (r, State s')
  where
    s' = (g .*. s) .%. (m1, m2)
    r = (fst s' ! (1, 1) + snd s' ! (1, 1)) `mod` m1

-- | Generate a random number.
-- Generates the next random number. The random number is in the range @0 .. m1 - 1@.
next :: State -> (Integer, State)
next = nextWith gen

-- $references
--
-- * L'Ecuyer, P. (2006) "Good parameter sets for combined multiple recursive random number generators",
--   Operations Research 47 (1) (1999) 159-164, <http://www.iro.umontreal.ca/~lecuyer/myftp/papers/combmrg2.ps>
--
-- * Bradley, T. et. al. (2011) "Parallelization Teechniques for Random Number Generators",
--   GPU Computing Gems, <http://dx.doi.org/10.1016/B978-0-12-384988-5.00016-4>
