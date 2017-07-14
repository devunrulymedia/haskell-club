
data Expr
	= Lit Int
	| Add Expr Expr deriving Show


evalExpr :: Expr -> Int
evalExpr (Lit n) = n
evalExpr (Add e1 e2) = (evalExpr e1) + (evalExpr e2)

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
evalStack ops = evalStack' ops [] where
	
