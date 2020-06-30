module LambdaTypeChecker where
type NameBind = String

data Binding = NameBind
             | VarBind Ty
             deriving (Eq, Show)

data Ty = TyBool
        | TyInt
        | TyVar
        | TyArr Ty Ty
        | TyError
        deriving (Eq, Show)

data Term = TTrue
          | TFalse
          | TIf Term Term Term
          | TVar String
          | TAbs String Term
          | TApp Term Term
          deriving (Eq, Show)

-- Add binding to variable
addbinding :: String -> [(String, Binding)] -> Binding -> [(String, Binding)]
addbinding x ctx bind = (x, bind):ctx

-- Get Binding of variable
getTypeFromContext :: String -> [(String, Binding)] -> Ty
getTypeFromContext x [] = TyError
getTypeFromContext x ((a, VarBind b):ctx) | x == a    = b
                                          | otherwise = getTypeFromContext x ctx
getTypeFromContext x ((a, NameBind):ctx) = TyError


-- Check if variable is already type binded
ctxContains :: String -> [(String, Binding)] -> Bool
ctxContains a []     = False
ctxContains a ((b,_):xs) = if a == b then True else ctxContains a xs

-- Abstract the type from the context
checkType :: String -> [(String, Binding)] -> Ty
checkType "true"  ctx = TyBool
checkType "false" ctx = TyBool
checkType a       ctx = if ctxContains a ctx then getTypeFromContext a ctx else TyVar 

-- Type verification function
typeOf :: Term -> [(String, Binding)] -> Ty
typeOf (TIf a b c) ctx   | and (typeOf a ctx == TyBool, typeOf c ctx == t2) = t2
                         | otherwise         =  TyError
                         where t2            =  typeOf b ctx
typeOf (TVar a) ctx      = getTypeFromContext a ctx
typeOf (TAbs x t) ctx = let ty = checkType x ctx
                            ctx' = addbinding x ctx (VarBind ty)
                            ty2  = typeOf t ctx' in TyArr ty ty2
                            where ty = typeOf (TVar x) ctx
typeOf (TApp t1 t2) ctx  = let ty1  = typeOf t1 ctx
                               ty2  = typeOf t2 ctx in case ty1 of TyArr a b -> b
                                                                   _         -> TyError

-- Function to call the Type Checker With an empty list of bindings
lambdaType :: Term -> Ty
lambdaType t = typeOf t []