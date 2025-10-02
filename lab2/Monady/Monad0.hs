
-- If you want to generate an executable file then you have to define a
-- module called *Main* with a definition of a function *main* of type
-- IO ().
module Main where 

-- If you omit import Prelude, you just import everything from the
-- module, which maybe too much.
--
-- import Prelude
--   ( 
--     getContents
--   , String, (++), concat
--   , IO, (>>), (>>=), putStrLn
--   , Maybe(..)
--   )

-- Monad is a type constructor (technically an instance of a Haskell class)
-- that makes it possible to generate an entity with four
-- important features:
-- * values wrapped in some complicated context with an easy interface
--   to the most standard operations;
-- * standard operation of data insert into the monad;
-- * standard operation of sequential processing;
-- * sometimes, monad has a some operation of data retrieval.

-- To achieve these goals a type constructor m
-- * has a type parameter (e.g. a), the values of this
--   type can be wrapped into the internal structure of the monad,
-- * in order to wrap data in the internal structure of the monad one uses
--   the return operation:
--   return :: a -> m a
-- * in order to sequenially process data within the wrapping structure
--   one uses the operation
--   (>>=) :: m a -> (a -> m b) -> m b
--   this operation is slightly more general than one needed for
--   simple sequential processing, but it is anyway very useful 
--   to process data to obtain result in a different type.


main :: IO ()
main =
    getContents >>= \v -> putStrLn ("> " ++ v )
