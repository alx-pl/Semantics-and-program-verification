module Main where

import Prelude
  ( ($), (.)
  , Either(..)
  , Int, Integer, (>), (+), (-), (*)
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

-- Semantics of expressions
eE :: Expr -> State -> Integer
eE ex st = case ex of
  EPlus exp0 exp  -> eE exp0 st + eE exp st 
  EMinus exp0 exp  -> eE exp0 st - eE exp st 
  EMul exp0 exp  -> eE exp0 st * eE exp st 
  ENum n  -> n
  EVar (Ident x)  -> case Map.lookup x st of
                Just v -> v
                Nothing -> 0

-- Semantics of boolean expressions
eB :: BExpr -> State -> Bool
eB bx st = case bx of
  BAnd bexp0 bexp  -> eB bexp0 st && eB bexp st 
  BNot bexp  -> not $ eB bexp st
  BLeq exp0 exp  -> GHC.Integer.leInteger (eE exp0 st) (eE exp st)
  BTrue  -> True
  BFalse -> False

-- Step function for statements
type Terminal = State
type NTerminal = (Stmt, State)
type Conf = Either (Stmt, State) Terminal
step :: NTerminal -> Conf
step (SAssgn (Ident x) ex, st) = Right (Map.insert x (eE ex st) st)
step (SSkip, st) = Right st
step (SIf bex i1 i2, st) = if eB bex st 
                            then Left (i1, st)
                            else Left (i2, st)
step (SWhile bex i, st) = if eB bex st 
                           then Left (SSeq i (SWhile bex i), st)
                           else Right st
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
