{-|
Module: Scheme.Environment
Description: Environment management for variable binding and lookup
Purpose: This module provides functions for managing the environment,
         which maps variable names to their values. It includes functions
         for creating empty environments, extending environments with new
         bindings, looking up variables, and defining new variables.
         The environment is implemented as a simple association list,
         but the interface allows for future optimization.

Exports:
  - emptyEnv: Create an empty environment
  - extendEnv: Extend an environment with new bindings
  - lookupVar: Look up a variable in the environment
  - lookupVarWithRecursion: Look up with special handling for recursive functions
  - setVar: Set an existing variable in the environment
  - defineVar: Define a new variable (creates if doesn't exist)
-}

module Scheme.Environment
  ( emptyEnv
  , extendEnv
  , lookupVar
  , lookupVarWithRecursion
  , setVar
  , defineVar
  ) where

import Scheme.Core (Value(..), SchemeError(..), Environment)
import Data.Text (Text)
import qualified Data.Text as T

-- | Create an empty environment
emptyEnv :: Environment
emptyEnv = []

-- | Extend an environment with new bindings
extendEnv :: Environment -> [(Text, Value)] -> Environment
extendEnv env bindings = bindings ++ env

-- | Look up a variable in the environment
lookupVar :: Environment -> Text -> Either SchemeError Value
lookupVar [] name = Left $ UnboundVariable name
lookupVar ((var, val):rest) name
  | var == name && val /= Nil = Right val
  | var == name && val == Nil = lookupVar rest name  -- Skip Nil placeholder
  | otherwise = lookupVar rest name

-- | Look up a variable in the environment, with special handling for recursive functions
lookupVarWithRecursion :: Environment -> Text -> Either SchemeError Value
lookupVarWithRecursion env name = 
  case lookupVar env name of
    Right (Function n body params funcEnv) -> 
      -- For recursive functions, ensure they can find themselves
      Right (Function n body params env)
    Right val -> Right val
    Left err -> Left err

-- | Set an existing variable in the environment
setVar :: Environment -> Text -> Value -> Either SchemeError Environment
setVar [] name _ = Left $ UnboundVariable name
setVar ((var, val):rest) name newVal
  | var == name = Right $ (var, newVal) : rest
  | otherwise = do
      newRest <- setVar rest name newVal
      return $ (var, val) : newRest

-- | Define a new variable in the environment (creates if doesn't exist)
defineVar :: Environment -> Text -> Value -> Environment
defineVar env name val = (name, val) : env 