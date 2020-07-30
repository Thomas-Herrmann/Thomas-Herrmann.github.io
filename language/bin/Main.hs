module Main where

import Ast
import Interpret
import Data.Map as Map


testTree1 = ([], AppExp ([0], ConstExp Not) ([1], ConstExp TrueConst)) -- should be false
testTree2 = ([], AppExp ([0], ConstExp Not) ([1], NumExp 13.2)) -- should fail
testTree3 = ([], AppExp ([0], ConstExp Not) ([1], ConstExp FalseConst)) -- should be true


main :: IO ()
main =
    case calculate Map.empty testTree3 of
        Left (p, err) -> putStrLn $ "error: " ++ err
        Right v       -> putStrLn $ "value: " ++ (show v)
