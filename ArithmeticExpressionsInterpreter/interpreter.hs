data Expr = ETrue
          | EFalse
          | If Expr Expr Expr
          | EZero
          | Succ Expr
          | Pred Expr
          | IsZero Expr
          | Error
          deriving(Eq, Show)


-- Check if Expr is numeric
isNumerical :: Expr -> Bool
isNumerical EZero = True
isNumerical (Succ e1) = isNumerical(e1)
isNumerical _ = False

-- Depcretaded
value :: Expr -> Bool
value ETrue = True
value EFalse = True
value e = isNumerical(e)

-- Evaluate function
eval :: Expr -> Expr
eval (If ETrue e2 _) = e2
eval (If EFalse _ e3) = e3
eval (If e1 e2 e3) = eval (If (eval e1) e2 e3)
eval (Succ e) = Succ(eval e)
eval (EZero) = EZero
eval (Pred EZero) = EZero
eval (Pred (Succ e)) = if isNumerical e then e else Error
eval (Pred e) = Pred (eval e)
eval (IsZero EZero) = ETrue
eval (IsZero (Succ e)) = if isNumerical e then EFalse else Error
eval (IsZero e) = if (eval e) == EZero then ETrue else EFalse

-- Main function: interpreter
interp :: Expr -> Expr
interp e = eval e