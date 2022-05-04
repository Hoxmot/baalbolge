module Interpreter
    ( -- * Functions
      interpret
  ) where

import           Control.Monad (foldM)

import qualified Baalbolge.Abs as BG

import           Types

interpret :: BG.Exps -> Err Result
interpret (BG.Program _ exps) = interpretExps exps

interpretExps :: [BG.Exp] -> Err Result
interpretExps = foldM interpretExpsMapper RUnit

interpretExpsMapper :: Result -> BG.Exp -> Err Result
interpretExpsMapper RUnit e = interpretExp e
interpretExpsMapper r _     = return r

interpretExp :: BG.Exp -> Err Result
interpretExp (BG.EUnit _) = return RUnit
interpretExp (BG.EInt _ v) = return $ RInt v
interpretExp (BG.EBool _ b) = interpretBool b

interpretExp e = Left $ "Checking types of exp: " ++ show e ++ " is not yet implemented"

interpretBool :: BG.Bool -> Err Result
interpretBool (BG.BTrue _)  = return $ RBool True
interpretBool (BG.BFalse _) = return $ RBool False
