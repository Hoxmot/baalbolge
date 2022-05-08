module Interpreter
    ( -- * Functions
      interpret
  ) where

import qualified Data.Map             as M

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import qualified Baalbolge.Abs        as BG

import           Baalbolge.Print
import           Types

{- | Interprets the whole program and returns its result or an error.
-}
interpret :: BG.Exps -> Err Result
interpret (BG.Program _ exps) = runReader (runExceptT $ interpretExps exps) M.empty

{- | Interprets a list of expressions and resturns the result value.

-}
interpretExps :: [BG.Exp] -> InterpreterReader
interpretExps exps = do
    env <- ask
    case evalState (runExceptT $ interpretExpsRec RUnit exps) env of
        Left l  -> throwError l
        Right r -> return r

{- | Recursively computes the results of expressions on a list of expressions.
In Baalbolge, value of a list of expressions is the first value, which type isn't unit.
Therefore, if we get such value, we stop evaluating the rest of expressions on the list.
-}
interpretExpsRec :: Result -> [BG.Exp] -> InterpreterState
interpretExpsRec RUnit (h:t) = do
    r <- interpretExp h
    case r of
        RUnit -> interpretExpsRec RUnit t
        rv    -> return rv
-- | if the type of TUnit or TBool, or the list is empty, we simply return the type
interpretExpsRec r _ = return r

{- | Interprets a single expression.

$setup
>>> let p = Just (2,2)
>>> let b = (BG.BTrue p)
>>> test e = evalState (runExceptT e) M.empty

>>> test $ interpretExp (BG.EUnit p)
Right RUnit

>>> test $ interpretExp (BG.EInt p 42)
Right (RInt 42)

>>> test $ interpretExp (BG.EBool p b)
Right (RBool True)
-}
interpretExp :: BG.Exp -> InterpreterState
interpretExp (BG.EUnit _) = return RUnit
interpretExp (BG.EInt _ v) = return $ RInt v
interpretExp (BG.EBool _ b) = interpretBool b
interpretExp var@(BG.EVar pos v) = interpretVar v `catchError` varNotFoundHandler var pos
interpretExp (BG.EInternal _ int) = interpretIFunc int
interpretExp e = throwError $ "Interpretation of exp: " ++ show e
    ++ " is not yet implemented"

{- | Interprets an internal function usage in Baalbolge.
-}
interpretIFunc :: BG.InternalFunc -> InterpreterState
interpretIFunc iFunc@(BG.IVarDecl pos t (BG.Var v) e) = do
    st <- get
    ex <- interpretExp e
    if typeEq t ex
        then put (M.insert v ex st) >> return RUnit
        else throwError $ typesError iFunc "VariableDeclaration" pos (pprintType t) (pprintResult ex)
interpretIFunc iFunc@(BG.IWhen pos cond e) = do
    condVal <- interpretExp cond
    case condVal of
        RBool b -> if b then interpretExp e else return RUnit
        _ -> throwError $ typesError iFunc "when statement" pos "bool" (pprintResult condVal)

interpretIFunc e = throwError $ "Interpretation of internal function: " ++ show e
    ++ " is not yet implemented"

{- | Interprets a variable and returns its value. If there's no variable of the given name
in the state, an error is thrown.

$setup
>>> test st e = evalState (runExceptT e) st

>>> test (M.singleton "x" (RBool True)) $ interpretVar (BG.Var "x")
Right (RBool True)

>>> test (M.singleton "x" (RInt 42)) $ interpretVar (BG.Var "x")
Right (RInt 42)

>>> test (M.singleton "x" RUnit) $ interpretVar (BG.Var "x")
Right RUnit
-}
interpretVar :: BG.Var -> InterpreterState
interpretVar (BG.Var v) = do
    st <- get
    case M.lookup v st of
        Just val -> return val
        Nothing  -> throwError $ "variable '" ++ v ++ "' not found!"

{- | Interprets the value of a bool.

$setup
>>> let p = Just (2,2)
>>> test e = evalState (runExceptT e) M.empty

>>> test $ interpretBool (BG.BTrue p)
Right (RBool True)

>>> test $ interpretBool (BG.BFalse p)
Right (RBool False)
-}
interpretBool :: BG.Bool -> InterpreterState
interpretBool (BG.BTrue _)  = return $ RBool True
interpretBool (BG.BFalse _) = return $ RBool False

{- | Compares the type of variable and the value we try to assign to it. It's an extension
of type checking in the TypeChecker as type var can produce runtime type incompatibilities.
-}
typeEq :: BG.Type -> Result -> Bool
typeEq (BG.TInt _) (RInt _)   = True
typeEq (BG.TBool _) (RBool _) = True
typeEq (BG.TVar _) _          = True
typeEq (BG.TUnit _) RUnit     = True
typeEq (BG.TUnit _) _         = False
typeEq _ _                    = False

{- | Handler of 'variable not found' error, which adds details about the error
for more readability.
-}
varNotFoundHandler :: Print a => a -> BG.BNFC'Position -> String -> InterpreterState
varNotFoundHandler ex pos er = throwError $ varNotFoundError ex er pos

-- | Creates a message about types error in the code.
typesError :: Print a => a -> String -> BG.BNFC'Position -> String -> String -> String
typesError ex op (Just (line, col)) t1 t2 =
    "Runtime exception! Types don't match! In operation '" ++ op
    ++ "' in line " ++ show line ++ ", column " ++ show col ++ ":\n  Expected '"
    ++ t1 ++ "' but got '" ++ t2 ++ "'!\n    (" ++ printTree ex ++ ")"
typesError ex op Nothing t1 t2 =
    "Runtime exception! Types don't match! In operation '" ++ op
    ++ "' at undetermined position:\n  Expected '" ++ t1
    ++ "' but got '" ++ t2 ++ "'!\n    (" ++ printTree ex ++ ")"

{- | Extends a simple information regaring 'variable not found' error with information
position of the erorr and the code itself.
-}
varNotFoundError :: Print a => a -> String -> BG.BNFC'Position -> String
varNotFoundError ex er (Just (line, col)) = er ++ "\n  In line " ++ show line
    ++ ", column " ++ show col ++ ":\n    " ++ printTree ex
varNotFoundError ex er Nothing = er ++ "\n  At undetermined position:\n    "
    ++ printTree ex

-- | Pretty prints the type used in Baalbolge
pprintType :: BG.Type -> String
pprintType (BG.TInt _)  = "int"
pprintType (BG.TBool _) = "bool"
pprintType (BG.TUnit _) = "unit"
pprintType (BG.TVar _)  = "var"
-- TODO: implementation of lists
pprintType _            = undefined

-- | Pretty prints the type of a result of an expression
pprintResult :: Result -> String
pprintResult RUnit     = "unit"
pprintResult (RInt _)  = "int"
pprintResult (RBool _) = "bool"
