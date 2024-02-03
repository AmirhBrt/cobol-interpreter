module Environment where
import Parser

data Value
    = NumValue      Float
    | StrValue      String
    | StrList       Integer [String]
    | NumList       Integer [Float]
    | ParValue      Statements
    | FunValue      Variables Expression Environment
    | CondValue     Bool
    | NothingValue
    | EXIT_VALUE
    deriving (Show, Eq, Ord)


---------------------------------------------
----------------- ADDED CODE ----------------
checkValueTypeEquality :: Value -> Value -> Bool
checkValueTypeEquality (NumValue v1) (NumValue v2)    = True
checkValueTypeEquality (StrValue v1) (StrValue v2)    = True
checkValueTypeEquality (NumList s v1) (NumList s1 v2) = True
checkValueTypeEquality (StrList s v1) (StrList s1 v2) = True
checkValueTypeEquality (CondValue v1) (CondValue v2)  = True
checkValueTypeEquality t1 t2                          = False

errorOnDifferenctTypes :: Value -> Value -> Bool
errorOnDifferenctTypes (NumValue v1) (NumValue v2)    = True
errorOnDifferenctTypes (StrValue v1) (StrValue v2)    = True
errorOnDifferenctTypes (NumList s v1) (NumList s1 v2) = True
errorOnDifferenctTypes (StrList s v1) (StrList s1 v2) = True
errorOnDifferenctTypes (CondValue v1) (CondValue v2)  = True
errorOnDifferenctTypes v1 v2 = error ("Cannot assign a variable with type " ++ show v1 ++ " an expression with type " ++ show v2)
----------------- ADDED CODE ----------------
---------------------------------------------

data Environment = Environment [String] [Value] deriving (Show, Eq, Ord)

initEnv :: Environment
initEnv = Environment ["_prints_"] [StrList 0 []]

joinEnv :: Environment -> Environment -> Environment
joinEnv (Environment x1 y1) (Environment x2 y2) = Environment (x1 ++ x2) (y1 ++ y2)

searchEnv :: Environment -> String -> Value
searchEnv (Environment [] _) key = error ("Using variable " ++ key ++ " before declaration!")
searchEnv (Environment _ []) key = error ("Using variable " ++ key ++ " before declaration!")
searchEnv (Environment (x:xs) (y:ys)) key = if x == key then y else searchEnv (Environment xs ys) key

extendUpdateEnv :: Environment -> String -> Value -> Environment
extendUpdateEnv (Environment [] x) key info = Environment [key] (info:x)
extendUpdateEnv (Environment x []) key info = Environment (key:x) [info]
extendUpdateEnv (Environment (x:xs) (y:ys)) key info = if x == key then Environment (x:xs) (info:ys)
                                                 else joinEnv (Environment [x] [y]) (extendUpdateEnv (Environment xs ys) key info)

typedExtendUpdateEnv :: Environment -> String -> Value -> Environment
typedExtendUpdateEnv (Environment [] x) key info = Environment [key] (info:x)
typedExtendUpdateEnv (Environment x []) key info = Environment (key:x) [info]
typedExtendUpdateEnv (Environment (x:xs) (y:ys)) key info = 
                                                if x == key 
                                                then 
                                                    if (checkValueTypeEquality info y) 
                                                    then Environment (x:xs) (info:ys) 
                                                    else error ("Trying To Assign " ++ show info ++ " to variable of type same as " ++ show y)
                                                else joinEnv (Environment [x] [y]) (extendUpdateEnv (Environment xs ys) key info)


updateEnv :: Environment -> String -> Value -> Environment
updateEnv (Environment [] x) key info = error ("Assignment to variable " ++ key ++ " before declaration!")
updateEnv (Environment x []) key info = error ("Assignment to variable " ++ key ++ " before declaration!")
updateEnv (Environment (x:xs) (y:ys)) key info = if x == key then Environment (x:xs) (info:ys)
                                                 else joinEnv (Environment [x] [y]) (updateEnv (Environment xs ys) key info)

extendEnv :: Environment -> String -> Value -> Environment
extendEnv (Environment [] x) key info = Environment [key] (info:x)
extendEnv (Environment x []) key info = Environment (key:x) [info]
extendEnv (Environment (x:xs) (y:ys)) key info = if x == key then error ("Duplicate declaration of variable " ++ key ++ "!")
                                                 else joinEnv (Environment [x] [y]) (extendEnv (Environment xs ys) key info)

popEnv :: Environment -> String -> Environment
popEnv (Environment [] x) key = error ("Variable " ++ key ++ " cannot be removed because it's not in the environment!")
popEnv (Environment x []) key = error ("Variable " ++ key ++ " cannot be removed because it's not in the environment!")
popEnv (Environment (x:xs) (y:ys)) key = if x == key then Environment xs ys else joinEnv (Environment [x] [y]) (popEnv (Environment xs ys) key)