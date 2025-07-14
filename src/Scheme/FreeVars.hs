{-|
Module: Scheme.FreeVars
Description: Free variable analysis for Scheme expressions
Purpose: This module provides functions to analyze Scheme expressions and
         determine which variables are free (referenced but not bound)
         within a given scope. This enables efficient closure creation
         by only capturing the variables that are actually needed.

Exports:
  - freeVars: Find free variables in an expression
  - freeVarsInBody: Find free variables in a function body
  - captureFreeVars: Create a minimal environment with only referenced variables
-}

module Scheme.FreeVars
  ( freeVars
  , freeVarsInBody
  , captureFreeVars
  ) where

import Scheme.Core (Value(..), Environment)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set

-- | Find all free variables in an expression
freeVars :: Value -> Set Text
freeVars val = case val of
  Number _ -> Set.empty
  String _ -> Set.empty
  Bool _ -> Set.empty
  Nil -> Set.empty
  Quote _ -> Set.empty  -- Quoted expressions don't evaluate variables
  Symbol name -> Set.singleton name
  List elements -> Set.unions (map freeVars elements)
  Function _ _ _ _ -> Set.empty
  Primitive _ _ -> Set.empty

-- | Find free variables in a function body, excluding parameters
freeVarsInBody :: [Value] -> [Text] -> Set Text
freeVarsInBody body params = 
  let paramSet = Set.fromList params
      bodyVars = Set.unions (map freeVars body)
  in Set.difference bodyVars paramSet

-- | Create a minimal environment containing only the referenced variables
captureFreeVars :: Environment -> Set Text -> Environment
captureFreeVars env freeVarSet = 
  filter (\(name, _) -> Set.member name freeVarSet) env 