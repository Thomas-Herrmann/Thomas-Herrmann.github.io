module Interpret
    (
      interpret
    ) where

import Control.Monad.Except
import Data.Matrix as Matrix
import Data.Map as Map
import Ast


data Error = UnknownVarError String
           | ApplyNonFunctionError
           | NonExhaustiveGuardsError
           | MisusedConstantError Constant


instance Show Error where
    show (UnknownVarError x)        = "Variable '" ++ x ++ "' is unknown at this path"
    show ApplyNonFunctionError      = "Cannot apply a non-function value"
    show NonExhaustiveGuardsError   = "Non-exhaustive guarded expressions"

    show (MisusedConstantError Not) = "Function can only be applied to boolean values" -- TODO: extend to other constants


type InterError = (Path, String)

type Inter a = Except InterError a


interpret :: Env -> PaddedExp -> Inter Value
interpret _ (_, NumExp n)            = return $ NumValue n
interpret _ (_, ConstExp TrueConst)  = return $ BoolValue True
interpret _ (_, ConstExp FalseConst) = return $ BoolValue False
interpret _ (_, ConstExp c)          = return (ConstValue c Nothing)

interpret env (p, VarExp x) =
    case Map.lookup x env of
        Just v  -> return v
        Nothing -> throwError (p, show (UnknownVarError x))

interpret env (p, AppExp pe1 pe2) = do
    fun <- interpret env pe1
    arg <- interpret env pe2
    case fun of
        FunValue x pe1' env' -> interpret (Map.insert x arg env') pe1'
        ConstValue c f       -> applyConstant p c f arg
        _                    -> throwError (p, show ApplyNonFunctionError) 

interpret env (_, FunExp x pe) = return (FunValue x pe env)

interpret env (_, MatrixExp pe2D) = do 
    val2D <- sequence (Matrix.matrix r c f)
    return $ MatrixValue val2D

    where 
        r = Matrix.nrows pe2D
        c = Matrix.ncols pe2D
        f (i, j) = (interpret env (Matrix.getElem i j pe2D))

interpret env (p, GuardExp ((Just guard, pe):ges)) = do
    value <- interpret env guard
    case value of
        ConstValue TrueConst _ -> interpret env pe
        _ -> interpret env (p, GuardExp ges)

interpret env (_, GuardExp ((Nothing, pe):_)) = interpret env pe

interpret _ (p, GuardExp []) = throwError (p, show NonExhaustiveGuardsError)


applyConstant :: Path -> Constant -> Maybe (Value -> Value) -> Value -> Inter Value 
applyConstant _ Not Nothing (BoolValue b)  = return (BoolValue (not b))
applyConstant _ Less Nothing (NumValue n) = return (ConstValue Less (Just f))
    where
        f (NumValue n') = BoolValue $ n < n'

applyConstant p Less (Just f) v@(NumValue n) = return $ f v

applyConstant p c _ _ = throwError (p, show (MisusedConstantError c))

