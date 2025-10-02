module Main where

-- Typically one does not use return and >>= directly, but
-- through the do notation. The program in Monad0 can be
-- equivalently written as:


main :: IO ()
main = do
    v <- getContents          -- The value wrapped in monad is unpacked to identifier v.
    putStrLn ("> " ++ v )     -- The identifier is used to build the function which
                              -- transforms the value in the required way.
