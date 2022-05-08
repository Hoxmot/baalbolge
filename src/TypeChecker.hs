module TypeChecker
    ( -- * Functions
      checkTypes
  ) where

import qualified Data.Map             as M

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import qualified Baalbolge.Abs        as BG

import           Baalbolge.Print
import           Types

{- | Checks the types in the given program. The check is pefrormed in a static manner.
If some of the types are unknown due to usage of `var` type, the check is a success and
any problems with types are found and thrown during execution of the program.

For the whole program, any type can be returned.
-}
checkTypes :: BG.Exps -> Err BG.Exps
checkTypes prog@(BG.Program _ exps) = do
    _ <- runReader (runExceptT $ checkTypesExps exps) M.empty
    return prog

{- | A function, which checks the return type of the list of Exps. The check is run for
each element of the list of Exps untill we get a type which terminates computation.

For the list of Exps, any type can be returned.
-}
checkTypesExps :: [BG.Exp] -> CheckTypeReader
checkTypesExps exps = do
    env <- ask
    case evalState (runExceptT $ checkTypesExpsRec TUnit exps) env of
        Left l  -> throwError l
        Right r -> return r

{- | A function, which maps Exps from the list and determines the return type of the whole
list. In Baalbolge, the first Exp to return a type other than Unit determines the return
Type of the whole list of Exps.

Since the computation is terminated when we get a first value, which isn't unit, we can
terminate the type checking when we get such a value. However, var can any type including
unit, so we continue checking the types until we get a type which definetely isn't unit.
-}
checkTypesExpsRec :: Type -> [BG.Exp] -> CheckTypeState
checkTypesExpsRec TUnit (h:t) = do
    r <- checkTypesExp h
    case r of
        TUnit -> checkTypesExpsRec TUnit t
        TVar  -> checkTypesExpsRec TVar t
        tp    -> return tp
checkTypesExpsRec TVar (h:t) = do
    r <- checkTypesExp h
    case r of
        TUnit -> checkTypesExpsRec TVar t
        TVar  -> checkTypesExpsRec TVar t
        _     -> return TVar
-- | if the type of TUnit or TBool, or the list is empty, we simply return the type
checkTypesExpsRec tp _ = return tp

{- | A function, which determines the return type of a single Exp.

$setup
>>> let p = Just (2,2)
>>> let b = (BG.BTrue p)
>>> test e = evalState (runExceptT e) M.empty

>>> test $ checkTypesExp (BG.EUnit p)
Right unit

>>> test $ checkTypesExp (BG.EInt p 42)
Right int

>>> test $ checkTypesExp (BG.EBool p b)
Right bool
-}
checkTypesExp :: BG.Exp -> CheckTypeState
checkTypesExp (BG.EUnit _) = return TUnit
checkTypesExp (BG.EInt _ _) = return TInt
checkTypesExp (BG.EBool _ _) = return TBool
checkTypesExp var@(BG.EVar pos v) = checkTypesVar v `catchError` varNotFoundHandler var pos
checkTypesExp (BG.EInternal _ int) = checkTypesIFunc int
checkTypesExp e = throwError $ "Checking types of exp: " ++ show e ++ " is not yet implemented"

{- | Checks the type for internal function usage in Baalbolge.
-}
checkTypesIFunc :: BG.InternalFunc -> CheckTypeState
{- | For variable declaration, the type of variable has to be the same as the type
of the expression we try assign to the variable.
-}
checkTypesIFunc iFunc@(BG.IVarDecl pos t (BG.Var v) e) = do
    tDecl <- checkTypesType t
    tExp <- checkTypesExp e
    st <- get
    if tDecl == tExp
      then put (M.insert v tDecl st) >> return TUnit
      else throwError $ typesError iFunc "VariableDeclaration" pos tDecl tExp
{- | For the when statement, we don't have any assumptions of the return type. We just
check that the condition expression is a bool and that there aren't any problems with
the expression provided as the second argument.

The type of the when statement is var, because we get either the type of
the second argument or unit if the condition is False. In theory, the type can be unit if
the type of the expression is also unit, but such code wouldn't make much sense.
-}
checkTypesIFunc iFunc@(BG.IWhen pos cond e) = do
    condT <- checkTypesExp cond
    if condT == TBool
        then checkTypesExp e >>= \t -> return $ tSum TUnit t
        else throwError $ typesError iFunc "when statement" pos TBool condT
checkTypesIFunc iFunc@(BG.IIf pos cond e1 e2) = do
    condT <- checkTypesExp cond
    if condT == TBool
        then do
            e1T <- checkTypesExp e1
            e2T <- checkTypesExp e2
            return $ tSum e1T e2T
        else throwError $ typesError iFunc "if statement" pos TBool condT
checkTypesIFunc e = throwError $ "Checking types for internal function: " ++ show e
    ++ " is not yet implemented"

{- | Returns the type of variable from the state. If there's no variable of the given name
in the state, an error is thrown.

$setup
>>> test st e = evalState (runExceptT e) st

>>> test (M.singleton "x" TBool) $ checkTypesVar (BG.Var "x")
Right bool

>>> test (M.singleton "x" TInt) $ checkTypesVar (BG.Var "x")
Right int

>>> test (M.singleton "x" TUnit) $ checkTypesVar (BG.Var "x")
Right unit

>>> test (M.singleton "x" TVar) $ checkTypesVar (BG.Var "x")
Right var
-}
checkTypesVar :: BG.Var -> CheckTypeState
checkTypesVar (BG.Var v) = do
    st <- get
    case M.lookup v st of
        Just t -> return t
        Nothing -> throwError $ "variable '" ++ v ++ "' not found!"

{- | Maps a type from code generated by BNF Converter to internal type of the type checker
implementation.

$setup
>>> let p = Just (2,2)
>>> test e = evalState (runExceptT e) M.empty

>>> test $ checkTypesType (BG.TInt p)
Right int

>>> test $ checkTypesType (BG.TUnit p)
Right unit

>>> test $ checkTypesType (BG.TVar p)
Right var

>>> test $ checkTypesType (BG.TBool p)
Right bool
-}
checkTypesType :: BG.Type -> CheckTypeState
checkTypesType (BG.TInt _) = return TInt
checkTypesType (BG.TUnit _) = return TUnit
checkTypesType (BG.TVar _) = return TVar
checkTypesType (BG.TBool _) = return TBool
checkTypesType e = throwError $ "Checking types of type: " ++ show e
    ++ " is not yet implemented"

{- | Handler of 'variable not found' error, which adds details about the error
for more readability.
-}
varNotFoundHandler :: Print a => a -> BG.BNFC'Position -> String -> CheckTypeState
varNotFoundHandler ex pos er = throwError $ varNotFoundError ex er pos

-- | Creates a message about types error in the code.
typesError :: Print a => a -> String -> BG.BNFC'Position -> Type -> Type -> String
typesError ex op (Just (line, col)) t1 t2 = "Types don't match! In operation '" ++ op
    ++ "' in line " ++ show line ++ ", column " ++ show col ++ ":\n  Expected '"
    ++ show t1 ++ "' but got '" ++ show t2 ++ "'!\n    (" ++ printTree ex ++ ")"
typesError ex op Nothing t1 t2 = "Types don't match! In operation '" ++ op
    ++ "' at undetermined position:\n  Expected '" ++ show t1
    ++ "' but got '" ++ show t2 ++ "'!\n    (" ++ printTree ex ++ ")"

{- | Extends a simple information regaring 'variable not found' error with information
position of the erorr and the code itself.
-}
varNotFoundError :: Print a => a -> String -> BG.BNFC'Position -> String
varNotFoundError ex er (Just (line, col)) = er ++ "\n  In line " ++ show line
    ++ ", column " ++ show col ++ ":\n    " ++ printTree ex
varNotFoundError ex er Nothing = er ++ "\n  At undetermined position:\n    "
    ++ printTree ex
