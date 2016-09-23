module Main where

import LogicInterp
import System.Environment

import LogicExpr.Abs
import LogicExpr.Lex
import LogicExpr.Par
import LogicExpr.ErrM


lexit :: String -> [Token]
lexit s = myLexer s

parse :: [Token] -> Program
parse xs = case pProgram xs of
  Bad err -> error $ "щ(`Д´щ;) \n" ++ err
  Ok p    -> p


compileLogic :: String -> IO ()
compileLogic str = evalProg $ parse $ lexit str


main :: IO ()
main = do
  args <- getArgs
  case args of
    (f:_) -> tcFile f
    []    -> do
      putStrLn "(#｀ε´)<( No argument )"
  where tcFile f = readFile f >>= compileLogic 
