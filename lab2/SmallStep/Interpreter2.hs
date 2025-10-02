module Main where

-- This is a version of the interpreter for Tiny in which the small
-- step reduction is defined for all the syntactical categories.

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
import Control.Monad      ( when )
import qualified Data.Map as Map
import qualified GHC.Integer (leInteger) 


import Tiny.Abs ( Expr(..), BExpr(..), Stmt(..), Ident(..) )
import Tiny.Lex   ( Token, mkPosToken )
import Tiny.Par   ( pExpr, pBExpr, pStmt, myLexer )
import Tiny.Print ( Print, printTree )
import Tiny.Skel  ()

type State = Map.Map String Integer

-- Step function for expressions
stepE :: Expr -> State -> Expr
stepE (EPlus (ENum n) (ENum n0)) st = ENum (n + n0)
stepE (EPlus (ENum n) exp) st = let nexp = stepE exp st in
                                EPlus (ENum n) nexp
stepE (EPlus exp0 exp) st = let nexp0 = stepE exp0 st in
                            EPlus nexp0 exp
stepE (EMinus (ENum n) (ENum n0)) st = ENum (n - n0)
stepE (EMinus (ENum n) exp) st = let nexp = stepE exp st in
                                EMinus (ENum n) nexp
stepE (EMinus exp0 exp) st = let nexp0 = stepE exp0 st in
                            EMinus nexp0 exp
stepE (EMul (ENum n) (ENum n0)) st = ENum (n * n0)
stepE (EMul (ENum n) exp) st = let nexp = stepE exp st in
                                EMul (ENum n) nexp
stepE (EMul exp0 exp) st = let nexp0 = stepE exp0 st in
                            EMul nexp0 exp
stepE (ENum n) st = ENum n
stepE (EVar (Ident x)) st = case Map.lookup x st of
                                Just v -> ENum v
                                Nothing -> ENum 0


-- Step function for boolean expressions
stepB :: BExpr -> State -> BExpr
stepB (BAnd BTrue bexp) st = bexp
stepB (BAnd BFalse bexp) st = BFalse
stepB (BAnd bexp0 bexp) st = let nbexp0 = stepB bexp0 st in
                              BAnd nbexp0 bexp
stepB (BNot BTrue) st = BFalse
stepB (BNot BFalse) st = BTrue
stepB (BNot bexp) st = let nbexp = stepB bexp st in
                        BNot nbexp
stepB (BLeq (ENum n1) (ENum n2)) st = if n1 <= n2 then BTrue else BFalse
stepB (BLeq (ENum n1) ex) st = let nex = stepE ex st in
                                (BLeq (ENum n1) nex)
stepB (BLeq ex0 ex) st = let nex0 = stepE ex0 st in
                          (BLeq nex0 ex)
stepB BTrue st = BTrue
stepB BFalse st = BFalse


-- Step function for statements
type Terminal = State
type NTerminal = (Stmt, State)
type Conf = Either (Stmt, State) Terminal
step :: NTerminal -> Conf
step (SAssgn (Ident x) (ENum n), st) = Right (Map.insert x n st)
step (SAssgn (Ident x) ex, st) = let nex = stepE ex st in
                                 Left (SAssgn (Ident x) nex, st)
step (SSkip, st) = Right st
step (SIf BTrue i1 i2, st) = Left (i1, st)
step (SIf BFalse i1 i2, st) = Left (i2, st)
step (SIf bex i1 i2, st) = let nbex = stepB bex st in
                           Left (SIf nbex i1 i2, st)
step (SWhile BTrue i, st) = Left ((SSeq i (SWhile BTrue i)), st)
step (SWhile BFalse i, st) = Right st
step (SWhile bex i, st) = Left ((SIf bex (SSeq i (SWhile bex i)) SSkip), st)
step (SSeq i1 i2, st) = case step (i1, st) of
                            Left (i1a, st1) -> Left (SSeq i1a i2, st1)
                            Right st1 -> Left (i2, st1)

closure :: NTerminal -> Terminal
closure c = case step c of
                Left nc -> closure nc
                Right nc -> nc

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
