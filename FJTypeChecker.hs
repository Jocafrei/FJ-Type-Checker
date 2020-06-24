module FJTypeChecker where
import Data.List

type Field                = String
type ClassName            = String
type MethodName           = String

data ClassDeclaration     = Class String String [(Ty, String)] Constructor [Method]
                          deriving(Eq, Show)

data InterfaceDeclaration = String [String] [Header]
                          deriving(Eq, Show)

data Method               = Met Header [Command] Term
                          deriving(Eq, Show)

data Header               = Head Ty String [(Ty, String)]
                          deriving(Eq, Show)

data Constructor          = Cons String [(Ty, String)] [String] [(String, String)]
                          deriving(Eq, Show)

type ClassTable           = [(ClassName, ClassDeclaration)]

type InterfaceTable       = [(String, [String])]

type Context              = [(String, Ty)]

data Term                 = TVar String
                          | TFields [(ClassName, Field)]
                          | TFieldAcc Term String
                          | TMethodInv Term String [Term]
                          | TObjCre String [Term]
                          | TCast String Term
                          | TValue Value
                          deriving(Eq, Show)

data Command              = ComWhile Expression [Command]
                          | ComAssign String Operation
                          | ComDeclare Ty String Operation
                          deriving(Eq, Show)

data Operation            = OpString String
                          | OpInt Int
                          | Oper Operation Operation
                          deriving(Eq, Show)

data Expression           = Expr String Int
                          deriving(Eq, Show)

data Var                  = Var String
                          deriving(Eq, Show)

data Value                = ProperValue
                          | PureLambdaExpression [Parameter] Term
                          | PureLambdaExprOp [Parameter] Operation
                          deriving(Eq, Show)

data ProperValue          = TObjValueCreation String [Value]
                          | DecoratedLambdaExpression Ty [Parameter] Term
                          deriving(Eq, Show)

data Parameter            = Untyped String
                          | Typed Ty String
                          deriving(Eq, Show)

data Ty                   = TyBool
                          | TyInt
                          | TyArr Ty Ty
                          | TyError
                          | TyClass String
                          deriving (Eq, Show)

getDeclaration :: ClassName -> ClassTable -> Maybe ClassDeclaration
getDeclaration c []                          = Nothing
getDeclaration c ((d, Class e f x y z):xs) = if c == d 
                                             then Just (Class e f x y z) 
                                             else getDeclaration c xs

subtyping :: String -> String -> ClassTable -> Bool
subtyping _ "Object" _ = True
subtyping "Object" _ _ = False
subtyping c d ct | c == d = True
                 | otherwise = case (getDeclaration c ct) of 
                                    Just (Class _ f _ _ _) -> if (d == f) 
                                                                  then True 
                                                                else (subtyping d f ct)
                                    Nothing -> False

searchInterfaceList :: String -> String -> (String, [String]) -> Bool
searchInterfaceList i1 i2 (a, [])     = False
searchInterfaceList i1 i2 (a, (x:xs)) = if i2 == x then True else searchInterfaceList i1 i2 (a, xs)

subtypingInterface :: String -> String -> InterfaceTable -> Bool
subtypingInterface i1 i2 []              = False
subtypingInterface i1 i2 ((a,(i3:b)):xs) = if a == i3
                                           then searchInterfaceList i1 i2 (a,(i3:b))
                                           else subtypingInterface i1 i2 xs

getTypeFromContext :: String -> Context -> Ty
getTypeFromContext x []                   = TyError
getTypeFromContext x ((a, b):ctx)         | x == a    = b
                                          | otherwise = getTypeFromContext x ctx

getExtendedFields :: [(Ty, String)] -> Maybe [(Ty, String)]
getExtendedFields []         = Just []
getExtendedFields ((a,b):xs) = Just [(a,b)]

getFields :: ClassName -> ClassTable -> Maybe [(Ty, String)]
getFields "Object" _ = Just []
getFields c ((d, Class e f x y z):xs) = if (c == d) 
                                        then (case (getFields f ((d, Class e f x y z):xs)) of 
                                                    Just a -> Just (a ++ x)
                                                    Nothing -> Nothing)
                                        else (getFields c xs)
getFields _ _ = Nothing

findField :: String -> [(Ty, String)] -> Bool
findField s [] = False
findField s ((_, b):xs) | s == b    = True
                        | otherwise = findField s xs

getTypes :: [(Ty, String)] -> [Ty]
getTypes [] = []
getTypes ((a, b):xs) = a:(getTypes xs)

findMethod :: String -> [Method] -> Maybe (Ty, [Ty])
findMethod mName [] = Nothing
findMethod mName ((Met h _ _):ml) = case findOnHeader mName h of Just a-> Just a 
                                                                 Nothing -> findMethod mName ml

findOnHeader :: String -> Header -> Maybe (Ty, [Ty])
findOnHeader mName (Head t m l) = if mName == m then Just (t, getTypes l) else Nothing

mType :: String -> String -> ClassTable -> Maybe (Ty, [Ty])
mtype _ "Object" _ = Nothing
mType mName c ct = case (getDeclaration c ct) of 
                        Just (Class _ d _ _ m) -> findMethod mName m
                        Nothing -> Nothing

allRight :: [(Term, Ty)] -> Context -> ClassTable -> Bool
allRight [] _ _= True
allRight ((a,b):xs) ctx ct = if (typeOf a ctx ct TyError) == b 
                             then allRight xs ctx ct
                             else False

typeOf :: Term -> Context -> ClassTable -> Ty -> Ty
typeOf (TValue (PureLambdaExpression _ t)) ctx ct t1 = if t1 == (typeOf t ctx ct t1)
                                                        then t1 
                                                     else TyError
typeOf (TValue (PureLambdaExprOp p t)) ctx ct t1     = if t1 == TyInt && allInt t ctx
                                                        then t1 
                                                     else TyError
typeOf (TVar s) ctx _ _             = getTypeFromContext s ctx
typeOf (TFieldAcc t s) ctx ct _     = let t1 = typeOf t ctx ct TyError 
                                    in case t1 of 
                                            TyClass cls -> case getFields cls ct of 
                                                                Just a ->  if (findField s a)
                                                                               then TyClass cls
                                                                           else TyError
                                                                Nothing -> TyError
                                            TyInt -> case getTypeFromContext s ctx of
                                                          TyInt -> TyInt
                                                          _     -> TyError
typeOf (TMethodInv t s tl) ctx ct _ = case (typeOf t ctx ct TyError) of 
                                            TyClass cls -> case (mType s cls ct) of 
                                                                Just (ret, arg) -> let zipped = zipWith (\a b -> (a, b)) tl arg in 
                                                                                       if (allRight zipped ctx ct) 
                                                                                          then ret 
                                                                                       else TyError
                                            _ -> TyError
typeOf (TObjCre s t) ctx ct _ = case (getFields s ct) of 
                                    Just f -> let zipped = zipWith(\a (b,c) -> (a, b)) t f in 
                                                  if (all (\(t, TyClass cls) -> case (typeOf t ctx ct (TyClass cls)) of 
                                                                                     (TyClass cls2) -> subtyping cls2 cls ct
                                                                                     _ -> False
                                                                                     ) zipped) 
                                                    then (TyClass s)
                                                  else TyError
                                    Nothing -> TyError                    
typeOf (TCast s t) ctx ct _  = let t1 = typeOf t ctx ct (TyClass s)in 
                                   case t1 of 
                                        TyClass cls -> if (subtyping cls s ct) 
                                                          then (TyClass s) 
                                                       else (if (subtyping s cls ct) 
                                                                then (TyClass cls) 
                                                            else TyError)
typeOf _ _ _ _ = TyError

typeOfExpression :: Expression -> Context -> Bool
typeOfExpression (Expr a b) ctx = if (getTypeFromContext a ctx) == TyInt then True else False

allInt :: Operation -> Context -> Bool
allInt (OpInt _) _ = True
allInt (OpString x) ctx = if (getTypeFromContext x ctx) == TyInt then True else False
allInt (Oper a b) ctx = (allInt a ctx) && (allInt b ctx)

typeOfCommands :: [Command] -> Context -> Bool
typeOfCommands [] _ = True
typeOfCommands ((ComAssign s i):xs) ctx = if (getTypeFromContext s ctx) == TyInt && (allInt i ctx) 
                                            then (typeOfCommands xs ctx) 
                                          else False
typeOfCommands ((ComDeclare t s i):xs) ctx = if (getTypeFromContext s ctx) == TyError && (allInt i ctx) 
                                            then (typeOfCommands xs ctx) 
                                          else False
typeOfCommands ((ComWhile e cms):xs) ctx = if (typeOfExpression e ctx) 
                                            then (typeOfCommands cms ctx) && (typeOfCommands xs ctx) 
                                          else False

updateContext :: [(Ty, String)] -> Context -> Context
updateContext [] ctx = ctx
updateContext ((a, b):xs) ctx = (b,a):(updateContext xs ctx) 

updateContextCommand :: [Command] -> Context -> Context
updateContextCommand [] ctx = ctx
updateContextCommand ((ComDeclare t s _):xs) ctx = (s, t):(updateContextCommand xs ctx)
updateContextCommand ((_):xs) ctx = updateContextCommand xs ctx 

typeOfMethod :: ClassDeclaration -> Context -> ClassTable -> Method -> Bool
typeOfMethod (Class c d _ _ _) ctx ct (Met (Head (TyClass t1) m p) comms t) = let ctx1 = (map(\(a, b) -> (b, a)) p)++ctx
                                                                                  ctx2 = ("this", (TyClass c)):ctx1
                                                                                  ctx3 = updateContext p ctx2
                                                                                  ctx4 = updateContextCommand comms ctx3
                                                                                  in case (typeOf t ctx4 ct (TyClass c)) of 
                                                                                          (TyClass cls) -> if (subtyping cls t1 ct) && (typeOfCommands comms ctx) 
                                                                                                            then case (mType m d ct) of 
                                                                                                                      Just (ret, arg) -> ret == (TyClass t1) && arg == (map (\(t,n) -> t) p)
                                                                                                                      _ -> True
                                                                                                            else False
                                                                                          _ -> False

typeOfMethod (Class c d _ _ _) ctx ct (Met (Head TyInt m p) comms t) = let  ctx1 = (map(\(a, b) -> (b, a)) p)++ctx
                                                                            ctx2 = ("this", (TyInt)):ctx1
                                                                            ctx3 = updateContext p ctx2
                                                                            ctx4 = updateContextCommand comms ctx3
                                                                            in case (typeOf t ctx4 ct (TyInt)) of 
                                                                                    TyInt -> case (mType m d ct) of 
                                                                                                  Just (ret, arg) -> ret == TyInt && arg == (map (\(t,n) -> t) p) && (typeOfCommands comms ctx)
                                                                                                  _ -> True
                                                                                    TyError -> False

typeOfClass :: ClassDeclaration -> Context -> ClassTable -> Bool
typeOfClass cls@(Class c d a (Cons s ts ls lss) m) ctx ct = case  (getFields d ct) of
                                                                  Just f -> if (checkLists ts (f ++ a))
                                                                            then if (all(\(a, b) -> a == b) lss) 
                                                                                  then let p2 = map(\(a, b) -> b) ts
                                                                                           p3 = ls ++ (map(\(a, b) -> a) lss)
                                                                                           in ((p2 == p3) && (all (typeOfMethod cls ctx ct) m))
                                                                                  else True
                                                                            else False
                                                                  Nothing -> False

checkLists :: (Eq a) => [a] -> [a] -> Bool
checkLists xs ys = null (xs \\ ys) && null (ys \\ xs)

typeCheck :: (ClassDeclaration, Context, ClassTable) -> Bool
typeCheck (a, b, c) = typeOfClass a b c

getCtx :: ClassDeclaration -> Context -> [(String, Ty)]
getCtx (Class c d [] (Cons s ts ls lss) m) ctx = ctx
getCtx (Class c d ((a,b):xs) (Cons s ts ls lss) m) ctx = let ctx' = (b,a):ctx in getCtx (Class c d (xs) (Cons s ts ls lss) m) ctx'

getCT :: ClassDeclaration -> ClassTable
getCT cls@(Class c d a (Cons s ts ls lss) m) = [(c, cls)]

buildTables :: ClassDeclaration -> Bool
buildTables a = typeCheck (a, getCtx a [], getCT a)