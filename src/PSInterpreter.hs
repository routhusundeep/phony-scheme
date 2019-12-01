module PSInterpreter
  (evalString,
   eval,
   runIOThrows,
   readExpr,
   readExprList) where

import           Control.Monad.Except
import           PSEvaluator
import           PSParser
import           PSTypes
import           Text.ParserCombinators.Parsec hiding (spaces)

trapError :: IOThrowsError String -> IOThrowsError String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

