module Main where

-- This is a version of the interpreter for Tiny in which Maybe
-- monad is used to reduce some of the boilerplate resulting
-- from error propagation.

import Prelude
  ( ($), (.)
  , Either(..)
  , Int, Integer, (>), (+), (-), (*), (<=)
  , Bool(..), (&&), (||), not
  , String, (++), concat, unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath
  , getContents, readFile
  , Maybe(..)
  )

import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( return )
import qualified Data.Map as Map
import qualified GHC.Integer (leInteger) 


import Tiny.Abs ( Expr(..), BExpr(..), Stmt(..), Ident(..) )
import Tiny.Lex   ( Token, mkPosToken )
import Tiny.Par   ( pExpr, pBExpr, pStmt, myLexer )
import Tiny.Print ( Print, printTree )
import Tiny.Skel  ()

type State = Map.Map String Integer

-- Step function for expressions
stepE :: Expr -> State -> Maybe Expr
stepE (EPlus (ENum n) (ENum n0)) st = return $ ENum (n + n0)
stepE (EPlus (ENum n) exp) st =  -- in non-monadic definition one has to
                                 -- repeat a case instruction akin to
                                 case stepE exp st of 
                                   Just nexp ->  Just $ EPlus (ENum n) nexp
                                   Nothing   -> Nothing
                                 -- the monadic version is
                                 -- do v <- stepE exp st
                                 --    return $ EPlus (ENum n) nexp
stepE (EPlus exp0 exp) st = do nexp0 <- stepE exp0 st
                               return $ EPlus nexp0 exp
stepE (EMinus (ENum n) (ENum n0)) st = return $ ENum (n - n0)
stepE (EMinus (ENum n) exp) st = do nexp <- stepE exp st 
                                    return $ EMinus (ENum n) nexp
stepE (EMinus exp0 exp) st = do nexp0 <- stepE exp0 st
                                return $ EMinus nexp0 exp
stepE (EMul (ENum n) (ENum n0)) st = return $ ENum (n * n0)
stepE (EMul (ENum n) exp) st = do nexp <- stepE exp st 
                                  return $ EMul (ENum n) nexp
stepE (EMul exp0 exp) st = do nexp0 <- stepE exp0 st
                              return $ EMul nexp0 exp
stepE (ENum n) st = return $ ENum n
stepE (EVar (Ident x)) st = do v <- Map.lookup x st
                               return $ ENum v

-- Step function for boolean expressions
stepB :: BExpr -> State -> Maybe BExpr
stepB (BAnd BTrue bexp) st = return bexp
stepB (BAnd BFalse bexp) st = return BFalse
stepB (BAnd bexp0 bexp) st = do nbexp0 <- stepB bexp0 st
                                return $ BAnd nbexp0 bexp
stepB (BNot BTrue) st = return BFalse
stepB (BNot BFalse) st = return BTrue
stepB (BNot bexp) st = do nbexp <- stepB bexp st
                          return $ BNot nbexp
stepB (BLeq (ENum n1) (ENum n2)) st = if n1 <= n2
                                      then return BTrue
                                      else return BFalse
stepB (BLeq (ENum n1) ex) st = do nex <- stepE ex st 
                                  return $ BLeq (ENum n1) nex
stepB (BLeq ex0 ex) st = do nex0 <- stepE ex0 st 
                            return $ BLeq nex0 ex
stepB BTrue st = return BTrue
stepB BFalse st = return BFalse


-- Step function for statements
type Terminal = State
type NTerminal = (Stmt, State)
type Conf = Either (Stmt, State) Terminal
step :: NTerminal -> Maybe Conf
step (SAssgn (Ident x) (ENum n), st) = return $ Right (Map.insert x n st)
step (SAssgn (Ident x) ex, st) = do nex <- stepE ex st
                                    return $ Left (SAssgn (Ident x) nex, st)
step (SSkip, st) = return $ Right st
step (SIf BTrue i1 i2, st) = return $ Left (i1, st)
step (SIf BFalse i1 i2, st) = return $ Left (i2, st)
step (SIf bex i1 i2, st) = do nbex <- stepB bex st
                              return $ Left (SIf nbex i1 i2, st)
step (SWhile BTrue i, st) = return $ Left ((SSeq i (SWhile BTrue i)), st)
step (SWhile BFalse i, st) = return $ Right st
step (SWhile bex i, st) = return $ Left ((SIf bex (SSeq i (SWhile bex i)) SSkip), st)
step (SSeq i1 i2, st) = do ni <- step (i1, st)
                           case ni of
                             Left (i1a, st1) -> return $ Left (SSeq i1a i2, st1)
                             Right st1 -> return $ Left (i2, st1)

closure :: NTerminal -> Maybe Terminal
closure c = do ni <- step c
               case ni of
                 Left nc -> closure nc
                 Right nc -> return nc

main :: IO ()
main = do
    getContents >>= compute
    putStrLn ""

st0 = Map.fromList [("x", 1), ("y", 2), ("z", 3)]
--st = Map.empty

compute s =
    case pStmt (myLexer s) of
        Left err -> do
            putStrLn "\nParse              Failed...\n"
            putStrLn err
            exitFailure
        Right e -> do
            putStrLn "\nParse Successful!"
            putStrLn $ show (closure (e, st0))
