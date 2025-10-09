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

-- Semantics of statements
iS :: Stmt -> State -> State
iS (SAssgn (Ident x) ex) st = Map.insert x (eE ex st) st
iS (SSkip) st = st
iS (SIf bex i1 i2) st = if eB bex st 
                          then iS i1 st
                          else iS i2 st
iS (SWhile bex i) st = if eB bex st 
                         then iS (SWhile bex i) (iS i st)
                         else st
iS (SSeq i1 i2) st = iS i2 (iS i1 st)


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
            putStrLn $ show (iS e st0)
