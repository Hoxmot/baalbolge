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
checkTypesExp (BG.EInternal _ d) = checkTypesIFunc d
checkTypesExp e = throwError $ "Checking types of exp: " ++ show e ++ " is not yet implemented"

checkTypesIFunc :: BG.InternalFunc -> CheckTypeState
checkTypesIFunc iFunc@(BG.IVarDecl pos t (BG.Var v) e) = do
    tDecl <- checkTypesType t
    tExp <- checkTypesExp e
    st <- get
    if tDecl == tExp
      then put (M.insert v tDecl st) >> return TUnit
      else throwError $ typesError iFunc "VariableDeclaration" pos tDecl tExp
checkTypesIFunc e = throwError $ "Checking types for internal function: " ++ show e
    ++ " is not yet implemented"

{- | Maps a type from code generated by BNF Converter to internal type of the type checker
implementation.

$setup
>>> let p = Just (2,2)
>>> test e = evalState (runExceptT e) M.empty

>>> test $ checkTypesType (BG.TInt p)
Right int
-}
checkTypesType :: BG.Type -> CheckTypeState
checkTypesType (BG.TInt _) = return TInt
checkTypesType e = throwError $ "Checking types of type: " ++ show e
    ++ " is not yet implemented"

{- | Creates a message about types error in the code.
-}
typesError :: Print a => a -> String -> BG.BNFC'Position -> Type -> Type -> String
typesError ex op (Just (line, col)) t1 t2 = "Types don't match! In operation '" ++ op
    ++ "' in line " ++ show line ++ ", column " ++ show col ++ ":\n  Expected '"
    ++ show t1 ++ "' but got '" ++ show t2 ++ "'!\n    (" ++ printTree ex ++ ")"
typesError ex op Nothing t1 t2 = "Types don't match! In operation '" ++ op
    ++ "' at undetermined position:\n  Expected '" ++ show t1
    ++ "' but got '" ++ show t2 ++ "'!\n    (" ++ printTree ex ++ ")"
