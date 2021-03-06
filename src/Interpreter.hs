module Interpreter
    ( -- * Functions
      interpret
      , interpretExp
  ) where


import qualified Data.Map             as M

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import qualified Baalbolge.Abs        as BG

import           Interpreter.Types
import           Interpreter.Util
import           Types
import           Util                 (getExpPos, printShow)


{- | Interprets the whole program and returns its result or an error.
-}
interpret :: BG.Exps -> Err Result
interpret (BG.Program _ exps) = do
    ret <- lift $ runReaderT (runExceptT $ interpretExps exps) initialInterpreterMem
    case ret of
        Left left -> throwError left
        Right right -> case right of
            RFunc {} -> throwError "Runtime exception! The program cannot return a function!"
            RBFunc _ -> throwError "Runtime exception! The program cannot return a function!"
            result -> return result



{- | Interprets a list of expressions and resturns the result value.

-}
interpretExps :: [BG.Exp] -> InterpreterReader
interpretExps exps = do
    mem <- ask
    res <- lift $ lift $ evalStateT (runExceptT $ interpretExpsRec RUnit exps) mem
    case res of
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
>>> test e = evalStateT (runExceptT e) M.empty

>>> test $ interpretExp (BG.EUnit p)
Right unit

>>> test $ interpretExp (BG.EInt p 42)
Right int 42

>>> test $ interpretExp (BG.EBool p b)
Right bool True
-}
interpretExp :: BG.Exp -> InterpreterState
interpretExp (BG.EInt _ v) = return $ RInt v
interpretExp (BG.EBool _ b) = interpretBool b
interpretExp e@(BG.EFunc pos var args) = interpretFuncCall var args `catchError` varNotFoundHandler e pos
interpretExp (BG.EInternal _ int) = interpretIFunc int
interpretExp var@(BG.EVar pos v) = interpretVar v `catchError` varNotFoundHandler var pos
interpretExp (BG.EUnit _) = return RUnit
interpretExp e = throwError $ "Interpretation of exp: " ++ show e
    ++ " is not yet implemented"


-- | Interpret a function from the state.
interpretFuncCall :: BG.Var -> [BG.Exp] -> InterpreterState
interpretFuncCall (BG.Var v) args = do
    mem <- get
    case M.lookup v mem of
        Just obj -> case obj of
            BuiltIn f -> f args
            f@(Func t argsList exps funcState) -> interpretFunction t argsList exps (M.insert v f funcState) args
            Var (RFunc t argsList exps funcState) ->
                interpretFunction t argsList exps (M.insert v (Func t argsList exps funcState) funcState) args
            Var (RBFunc f) -> f args
            Var _ -> throwError $ "'" ++ v ++ "' is not a function!"
        Nothing -> throwError $ "Function '" ++ v ++ "' not found!"


-- | Interprets a function written in Baalbolge
interpretFunction :: BG.Type -> [Arg] -> [BG.Exp] -> InterpreterMemory -> [BG.Exp] -> InterpreterState
interpretFunction ft argsList exps funcState args = do
    funcMem <- createFuncEnv funcState argsList args
    res <- lift $ lift $ runReaderT (runExceptT $ interpretFuncBody ft exps) funcMem
    case res of
        Left l  -> throwError l
        Right r -> return r

{- | Create an environment for the function to execute. The environment has to be separate
from the state of the main program, because it can't modify it.
-}
createFuncEnv :: InterpreterMemory -> [Arg] -> [BG.Exp] -> InterpreterMemoryState
createFuncEnv env ((Arg t v):als) (a:as) = do
    arg <- interpretExp a
    if typeEq t arg
        then createFuncEnv (M.insert v (Var arg) env) als as
        else throwError $ "Types don't match! Expected '" ++ pprintType t
            ++ "', but got '" ++ pprintResult arg ++ "'!"
createFuncEnv env [] [] = return env
createFuncEnv _ [] _ = throwError "Too many arguments!"
createFuncEnv _ _ _ = throwError "Partial function application is not supported yet!"

-- | Interprets the body of the function
interpretFuncBody  :: BG.Type -> [BG.Exp] -> InterpreterReader
interpretFuncBody t exps = do
    res <- interpretExps exps
    if typeEq t res
        then return res
        else throwError $ "Types don't match! Expected '" ++ pprintType t
            ++ "', but got '" ++ pprintResult res ++ "'!"


{- | Interprets an internal function usage in Baalbolge.
-}
interpretIFunc :: BG.InternalFunc -> InterpreterState

{- | Variable declaration computes the value of the expression and checks if it matches
the requested type. If there's a match, the variable is created and the value is assigned.
If the types don't match, runtime exception is thrown.

The value of the variable declaration is unit.
-}
interpretIFunc iFunc@(BG.IVarDecl pos t (BG.Var v) e) = do
    mem <- get
    ex <- interpretExp e
    if typeEq t ex
        then put (M.insert v (Var ex) mem) >> return RUnit
        else throwError $ typesError iFunc "variable declaration" pos (pprintType t) (pprintResult ex)

{- | When statement computes the condition first. Then, it checks whether the type is
bool. If it is, then the computation continues and depending on the value (True/False),
the given expression is computed or not. If the type of the condition isn't bool, runtime
exception is thrown.

The result of the when statement is the value of the expression (if the condition is True),
or unit (if the condition is False).
-}
interpretIFunc iFunc@(BG.IWhen pos cond e) = do
    condVal <- interpretExp cond
    case condVal of
        RBool b -> if b then interpretExp e else return RUnit
        _ -> throwError $ typesError iFunc "when statement" pos "bool" (pprintResult condVal)

{- | If statement computes the condition first. Then, it checks whether the type is
bool. If it is, then the computation continues and depending on the value (True/False),
one of the expressions is computed. If the type of the condition isn't bool, runtime
exception is thrown.

The result of the if statement is the value of the computed expression.
-}
interpretIFunc iFunc@(BG.IIf pos cond e1 e2) = do
    condVal <- interpretExp cond
    case condVal of
        RBool b -> if b then interpretExp e1 else interpretExp e2
        _ -> throwError $ typesError iFunc "if statement" pos "bool" (pprintResult condVal)

{- | While loop computes the condition first. Then, it checks whether the type is
bool. If it is, then the computation continues and the given expression is evalueated
if the condition is True. If the type of the condition isn't bool, runtime exception
is thrown.

If the value of the given expression is other than unit, the while loop stops and the value
is returned. If it's unit, the while loop computes again.

The result of the while loop is the value of the expression (if the condition is True and
the value of the expression is not unit), or unit (if the condition is False).
-}
interpretIFunc iFunc@(BG.IWhile pos cond e) = do
    condVal <- interpretExp cond
    case condVal of
        RBool b -> if b
            then do
                val <- interpretExp e
                case val of
                    RUnit -> interpretIFunc iFunc
                    _     -> return val
            else return RUnit
        _ -> throwError $ typesError iFunc "while loop" pos "bool" (pprintResult condVal)

{- | Function declaration just creates the memory object of a function and puts it in the
memory. No additional checks.

The resulf of the function declaration is unit.
-}
interpretIFunc (BG.IFuncDecl _ t (BG.Var v) (BG.AList _ argsList) exps) = do
    mem <- get
    put (M.insert v (Func t (map argsMapper argsList) exps mem) mem)
    return RUnit
  where
    argsMapper (BG.AArg _ at (BG.Var av)) = Arg at av

{- | Lambda declaration just creates the object of a function and returns it.
No additional checks.

The resulf of the function declaration is the function object.
-}
interpretIFunc (BG.ILambda _ t (BG.AList _ argsList) exps) =
    gets $ RFunc t (map argsMapper argsList) exps
  where
    argsMapper (BG.AArg _ at (BG.Var av)) = Arg at av

{- | Prints the values of expressions given as arguments. Cannot print functions.

The result of the print function is unit.
-}
interpretIFunc iFunc@(BG.IPrint pos exps) =
    printExps `catchError` printTypesErrorHandler iFunc pos
  where
    printExps = do
        toPrint <- mapM printTypeChecker exps
        lift $ lift $ putStrLn $ unwords $ map printShow toPrint
        return RUnit

interpretIFunc e = throwError $ "Interpretation of internal function: " ++ show e
    ++ " is not yet implemented"

{- | Interprets a variable and returns its value. If there's no variable of the given name
in the state, an error is thrown.

$setup
>>> test st e = evalStateT (runExceptT e) st

>>> test (M.singleton "x" (Var (RBool True))) $ interpretVar (BG.Var "x")
Right bool True

>>> test (M.singleton "x" (Var (RInt 42))) $ interpretVar (BG.Var "x")
Right int 42

>>> test (M.singleton "x" (Var RUnit)) $ interpretVar (BG.Var "x")
Right unit
-}
interpretVar :: BG.Var -> InterpreterState
interpretVar (BG.Var v) = do
    mem <- get
    case M.lookup v mem of
        Just obj -> case obj of
            Var val -> return val
            Func t argsList exps funcState -> return $ RFunc t argsList exps funcState
            BuiltIn f -> return $ RBFunc f
        Nothing  -> throwError $ "variable '" ++ v ++ "' not found!"


{- | Interprets the value of a bool.

$setup
>>> let p = Just (2,2)
>>> test e = evalStateT (runExceptT e) M.empty

>>> test $ interpretBool (BG.BTrue p)
Right bool True

>>> test $ interpretBool (BG.BFalse p)
Right bool False
-}
interpretBool :: BG.Bool -> InterpreterState
interpretBool (BG.BTrue _)  = return $ RBool True
interpretBool (BG.BFalse _) = return $ RBool False


{- | A function, which checks the type of the expression we'd like to print. We need it
as we can't print functions (yet), which are also valid expressions.

If the value is a function, we throw an error. If it's not, we simply return it.
-}
printTypeChecker :: BG.Exp -> InterpreterState
printTypeChecker ex = do
    res <- interpretExp ex
    case res of
        RBFunc _ -> throwError $ notPrintableError ex (getExpPos ex) $ pprintResult res
        RFunc {} -> throwError $ notPrintableError ex (getExpPos ex) $ pprintResult res
        val      -> return val


initialInterpreterMem :: InterpreterMemory
initialInterpreterMem = M.fromList interpreterMemElems

interpreterMemElems :: [(Name, MemoryObj)]
interpreterMemElems = [
    ("+", BuiltIn interpretAdd)
    , ("-", BuiltIn interpretSub)
    , ("*", BuiltIn interpretMul)
    , ("/", BuiltIn interpretDiv)
    , ("%", BuiltIn interpretMod)
    , (">", BuiltIn interpretGt)
    , ("=", BuiltIn interpretEq)
  ]

interpretAdd :: BuiltInFunction
interpretAdd [e1, e2] = do
    v1 <- interpretExp e1
    v2 <- interpretExp e2
    case (v1, v2) of
        (RInt x, RInt y) -> return (RInt $ x + y)
        (RInt _, t2) -> throwError $ "Types don't match! Expected 'int', but got '" ++ pprintResult t2 ++ "'!"
        (t1, _) -> throwError $ "Types don't match! Expected 'int', but got '" ++ pprintResult t1 ++ "'!"
interpretAdd (_:_:_:_) = throwError "Too many arguments!"
interpretAdd _ = throwError "Partial function application is not supported yet!"

interpretSub :: BuiltInFunction
interpretSub [e1, e2] = do
    v1 <- interpretExp e1
    v2 <- interpretExp e2
    case (v1, v2) of
        (RInt x, RInt y) -> return (RInt $ x - y)
        (RInt _, t2) -> throwError $ "Types don't match! Expected 'int', but got '" ++ pprintResult t2 ++ "'!"
        (t1, _) -> throwError $ "Types don't match! Expected 'int', but got '" ++ pprintResult t1 ++ "'!"
interpretSub (_:_:_:_) = throwError "Too many arguments!"
interpretSub _ = throwError "Partial function application is not supported yet!"

interpretMul :: BuiltInFunction
interpretMul [e1, e2] = do
    v1 <- interpretExp e1
    v2 <- interpretExp e2
    case (v1, v2) of
        (RInt x, RInt y) -> return (RInt $ x * y)
        (RInt _, t2) -> throwError $ "Types don't match! Expected 'int', but got '" ++ pprintResult t2 ++ "'!"
        (t1, _) -> throwError $ "Types don't match! Expected 'int', but got '" ++ pprintResult t1 ++ "'!"
interpretMul (_:_:_:_) = throwError "Too many arguments!"
interpretMul _ = throwError "Partial function application is not supported yet!"

interpretDiv :: BuiltInFunction
interpretDiv [e1, e2] = do
    v1 <- interpretExp e1
    v2 <- interpretExp e2
    case (v1, v2) of
        (RInt x, RInt y) -> if y == 0
            then throwError "Division by 0 is not allowed!"
            else return (RInt $ div x y)
        (RInt _, t2) -> throwError $ "Types don't match! Expected 'int', but got '" ++ pprintResult t2 ++ "'!"
        (t1, _) -> throwError $ "Types don't match! Expected 'int', but got '" ++ pprintResult t1 ++ "'!"
interpretDiv (_:_:_:_) = throwError "Too many arguments!"
interpretDiv _ = throwError "Partial function application is not supported yet!"

interpretMod :: BuiltInFunction
interpretMod [e1, e2] = do
    v1 <- interpretExp e1
    v2 <- interpretExp e2
    case (v1, v2) of
        (RInt x, RInt y) -> return (RInt $ mod x y)
        (RInt _, t2) -> throwError $ "Types don't match! Expected 'int', but got '" ++ pprintResult t2 ++ "'!"
        (t1, _) -> throwError $ "Types don't match! Expected 'int', but got '" ++ pprintResult t1 ++ "'!"
interpretMod (_:_:_:_) = throwError "Too many arguments!"
interpretMod _ = throwError "Partial function application is not supported yet!"

interpretGt :: BuiltInFunction
interpretGt [e1, e2] = do
    v1 <- interpretExp e1
    v2 <- interpretExp e2
    case (v1, v2) of
        (RInt x, RInt y) -> return (RBool $ x > y)
        (RInt _, t2) -> throwError $ "Types don't match! Expected 'int', but got '" ++ pprintResult t2 ++ "'!"
        (t1, _) -> throwError $ "Types don't match! Expected 'int', but got '" ++ pprintResult t1 ++ "'!"
interpretGt (_:_:_:_) = throwError "Too many arguments!"
interpretGt _ = throwError "Partial function application is not supported yet!"

interpretEq :: BuiltInFunction
interpretEq [e1, e2] = do
    v1 <- interpretExp e1
    v2 <- interpretExp e2
    case (v1, v2) of
        (RInt x, RInt y) -> return (RBool $ x == y)
        (RInt _, t2) -> throwError $ "Types don't match! Expected 'int', but got '" ++ pprintResult t2 ++ "'!"
        (t1, _) -> throwError $ "Types don't match! Expected 'int', but got '" ++ pprintResult t1 ++ "'!"
interpretEq (_:_:_:_) = throwError "Too many arguments!"
interpretEq _ = throwError "Partial function application is not supported yet!"
