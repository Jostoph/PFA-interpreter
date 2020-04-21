module Interpreter where
  
  import Data.Char
  import Data.Map.Strict as Map

  -- Expression

  data Expr = 
    Const Int
    | Sum Expr Expr
    | Prod Expr Expr
    | Mod Expr Expr
    | Equal Expr Expr
    | Div Expr Expr
    | Sub Expr Expr
    | Greater Expr Expr
    | Less Expr Expr
    | Comb Expr Expr
    | Not Expr
    | Fact Expr
    | Abs Expr
    | Let String Expr Expr
    | Var String
    | Cond Expr Expr Expr

  -- utils

  fact 0 = 1
  fact n = n * fact (n - 1)

  -- Expression evaluation

  -- Binary

  eval _ (Const x) = x
  eval env (Sum x y) = eval env x + eval env y
  eval env (Prod x y) = eval env x * eval env y
  eval env (Mod x y) = eval env x `mod` eval env y
  eval env (Equal x y) = if eval env x == eval env y then 1 else 0
  eval env (Div x y) = eval env x `div` eval env y
  eval env (Sub x y) = eval env x - eval env y
  eval env (Greater x y) = if eval env x > eval env y then 1 else 0
  eval env (Less x y) = if eval env x < eval env y then 1 else 0
  eval env (Comb n k) =
    let
      n' = eval env n
      k' = eval env k
    in
      if k' < 0 || n' < 0 then error "Combination of negative values."
      else if k' == 0 then 1
      else if k' > n' then 0
      else (fact n') `div` ((fact k') * (fact (n' - k')))

  -- Unary

  eval env (Not x) = if eval env x == 0 then 1 else 0
  eval env (Fact n) =
    let 
      x = eval env n
    in 
      if x < 0 then error "Factorial of negative value." else fact (eval env n)
  eval env (Abs x) = let x' = eval env x in if x' < 0 then -x' else x'

  -- Variable

  eval env (Let name value y) = eval (Map.insert name value env)  y
  eval env (Var name) =
    let
      x = (Map.lookup name env)
    in
      case x of
        Nothing -> error ("The variable " ++ name ++ "was not found in this environment.")
        Just v -> eval env v

  -- Conditional

  eval env (Cond predica x y) = if eval env predica > 0 then eval env x else eval env y

  -- Show

  instance Show Expr where
    show (Const x) = show x
    show (Sum x y) = show x ++ " + " ++ show y
    show (Prod x y) = show x ++ " * " ++ show y
    show (Mod x y) = show x ++ " % " ++ show y
    show (Equal x y) = show x ++ " == " ++ show y
    show (Div x y) = show x ++ " / " ++ show y
    show (Sub x y) = show x ++ " - " ++ show y
    show (Greater x y) = show x ++ " > " ++ show y
    show (Less x y) = show x ++ " < " ++ show y
    show (Comb n k) = "C(" ++ show n ++ ", " ++ show k ++ ")"
    show (Not x) = "not " ++ show x
    show (Fact n) = "factorial(" ++ show n ++ ")"
    show (Abs x) = "abs(" ++ show x ++ ")"
    show (Let name value e) = "let " ++ name ++ " == " ++ show value ++ " in " ++ show e
    show (Var name) = name
    show (Cond predica x y) = "if " ++ show predica ++ " then " ++ show x ++ " else " ++ show y
