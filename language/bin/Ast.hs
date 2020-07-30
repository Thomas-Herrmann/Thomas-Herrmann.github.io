module Ast 
    (
    PaddedExp
    , Exp(..)
    , Constant(..)
    , Value(..)
    , Env
    , Path
    ) where


import Data.Map as Map
import Data.Matrix as Matrix


data Constant = TrueConst | FalseConst | Not | Less | Greater | LessEq | GreaterEq | Eq | NotEq | Land | Lor deriving Show

type Path = [Int]

type PaddedExp = (Path, Exp)

data Exp = NumExp Float
         | ConstExp Constant
         | VarExp String
         | AppExp PaddedExp PaddedExp
         | FunExp String PaddedExp
         | MatrixExp (Matrix PaddedExp)
         | GuardExp [(Maybe PaddedExp, PaddedExp)]
         | HoleExp
         deriving Show -- TODO, manual

data Value = NumValue Float
           | BoolValue Bool
           | ConstValue Constant (Maybe (Value -> Value))
           | FunValue String PaddedExp Env
           | MatrixValue (Matrix Value)

type Env = Map String Value


instance Show Value where
   show (NumValue n)      = show n
   show (BoolValue True)  = "true"
   show (BoolValue False) = "false"
   show (ConstValue c _)  = show c
   show (FunValue s _ _)  = "f(" ++ s ++ ")"
   show (MatrixValue vs)  = show vs
