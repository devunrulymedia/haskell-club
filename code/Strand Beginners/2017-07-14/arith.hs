
data Expr
	= Error String
        | Lit Int
        | BLit Bool
        | Equal Expr Expr
	| Add Expr Expr deriving Show

data Value
	= IValue Int
	| BValue Bool deriving (Show, Eq)

castInt :: Value -> Either String Int
castInt (IValue x) = Right x
castInt _ = Left "Type error: not an int"

safeEquals :: Value -> Value ->  Either String Value
safeEquals (IValue x) (IValue y) = Right (BValue (x == y))
safeEquals (BValue x) (BValue y) = Right (BValue (x == y))
safeEquals _ _ = Left "Type mismatch"

evalExpr :: Expr -> Either String Value
evalExpr (Error x) = Left x 
evalExpr (Lit n) = Right $ IValue n
evalExpr (BLit b) = Right $ BValue b
evalExpr (Add e1 e2) = do x <- evalExpr e1 >>= castInt   
                          y <- evalExpr e2 >>= castInt
			  return $ IValue (x + y)
evalExpr (Equal e1 e2) = do x <- evalExpr e1
			    y <- evalExpr e2
			    safeEquals x y


string :: Expr -> String
string (Lit n) = show n
string (Add e1 e2) = string e1 ++ " + " ++ string e2

double :: Expr -> Expr
double a = Add a a

data StackOp
	= Push Int
	| StackAdd
	deriving Show

type Program = [StackOp]
type Stack = [Int]

evalStack' :: Program -> Stack -> Maybe Int	
evalStack' [] [x] = Just $ x
evalStack' (StackAdd:moreOps) (x:y:stack) 
	= evalStack' moreOps ((x+y):stack)
evalStack' ((Push x):moreOps) stack = evalStack' moreOps (x:stack)
evalStack' _ _ = Nothing 

evalStack :: [StackOp] -> Maybe Int
evalStack ops = evalStack' ops [] 	
