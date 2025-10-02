module Main where

-- import Prelude
--   ( 
--     getContents
--   , ($)
--   , String, (++), concat
--   , Bool(..), (||)
--   , Eq
--   , Show
--   , IO, putStrLn
--   , Functor(..)
--   , Applicative(..)
--   )

-- import Control.Monad

-- To create your own monad you need to start from your
-- a type for which you want to hide typical boilerplate code.
-- We use here a type of flagged data. Most of the time the
-- flag is omitted and all operations manipulate only the actual
-- data, but from time to time the flag is raised or lowered.
data Flagged a = FData a Bool
  deriving (Eq,Show)

-- In order to create a monad one needs to implement some
-- basic blocks that make the creation of the monad possible.
-- First of them is the operation which lifts any transformation
-- *f* of non-wrapped data into a transformation within the monad.
-- This is done by means of the *fmap* operation in the typeclass
-- Functor.
instance Functor Flagged where -- Flagged must be a type name
  fmap f (FData d b) = FData (f d) b

-- As mentioned before monads enable sequential computation, which
-- is associative. The basic interface of sequential operation is
-- described by the *Applicative* typeclass. We therefore define
-- two basic operations of the typeclass.
instance Applicative Flagged where                     -- Flagged must be a
                                                       -- type name
  pure = \x -> FData x False                           -- operation to insert
                                                       -- pure data into the
                                                       -- wrapping structure

  (FData f b) <*> (FData d b') = FData (f d) (b || b') -- definition on how
                                                       -- functions on data
                                                       -- wrapped in the
                                                       -- structure work
                                                       -- on data.



-- We can now create an instance of the monad associated with
-- the Flagged data type.
instance Monad Flagged where -- Flagged must be a type name
  return d = pure d        -- Default insert operation is to insert
                           -- unflagged piece of data.
  (FData d b) >>= f = f d  -- this define how to operate on data

-- In addition we define operations to raise and lower the flag
raiseF :: a -> Flagged a
raiseF d = FData d True

lowerF :: a -> Flagged a
lowerF d = FData d False


-- At last we use an operation to retrieve data and flag:
getData :: Flagged a -> a
getData (FData d _) = d

getFlag :: Flagged a -> Bool
getFlag (FData _ b) = b

-- We can now make some operations using the monad
someComputations :: Flagged String -> Flagged String
someComputations d =
  do
    v <- d                   -- retrieve the bare data into v
    w <- return (v ++ "FFF") -- compute with v and store the result into
                             -- the monad
    raiseF w                 -- do some extra operation



main :: IO ()
main = do
    v <- getContents
    w <- let res = someComputations $ return v in
         if getFlag res
         then return ("!!!" ++ (getData res) ++ "!!!")
         else return (getData res)
    putStrLn ("> " ++ w ++ "ggg")
