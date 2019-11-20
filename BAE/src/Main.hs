import BAE.Parser as Parser
import BAE.Dynamic as Dynamic
import BAE.Sintax as Sintax
import BAE.Type as Type
import Data.List
import System.Environment

--Combinador Y
fix :: Sintax.Expr
fix = Sintax.Fn "f" (Sintax.App (Sintax.Fn "y" (Sintax.App (Sintax.V "f") (Sintax.App (Sintax.V "y") (Sintax.V "y")))) (Sintax.Fn "y" (Sintax.App (Sintax.V "f") (Sintax.App (Sintax.V "y") (Sintax.V "y")))))

parserExprToExpr :: Parser.Expr -> Sintax.Expr
parserExprToExpr (Parser.V n) = Sintax.V n
parserExprToExpr (Parser.I x) = Sintax.I (fromIntegral x)
parserExprToExpr (Parser.B b) = Sintax.B b
parserExprToExpr (Parser.Void) = Sintax.Void
parserExprToExpr (Parser.Fn x e) = Sintax.Fn x (parserExprToExpr e)
parserExprToExpr (Parser.RecurFn f x e) = Sintax.App fix (Sintax.Fn f (Sintax.Fn x (parserExprToExpr e)))
parserExprToExpr (UnaryE Parser.Not e) = Sintax.Not (parserExprToExpr e)
parserExprToExpr (UnaryE Parser.Succ e) = Sintax.Succ (parserExprToExpr e)
parserExprToExpr (UnaryE Parser.Pred e) = Sintax.Pred (parserExprToExpr e)
parserExprToExpr (UnaryE Parser.Alloc e) = Sintax.Alloc (parserExprToExpr e)
parserExprToExpr (UnaryE Parser.Deref e) = Sintax.Deref (parserExprToExpr e)
parserExprToExpr (BinaryE Parser.And e1 e2) = Sintax.And (parserExprToExpr e1) (parserExprToExpr e2)
parserExprToExpr (BinaryE Parser.Or e1 e2) = Sintax.Or (parserExprToExpr e1) (parserExprToExpr e2)
parserExprToExpr (BinaryE Parser.Add e1 e2) = Sintax.Add (parserExprToExpr e1) (parserExprToExpr e2)
parserExprToExpr (BinaryE Parser.Mul e1 e2) = Sintax.Mul (parserExprToExpr e1) (parserExprToExpr e2)
parserExprToExpr (RelationalE Parser.Gt e1 e2) = Sintax.Gt (parserExprToExpr e1) (parserExprToExpr e2)
parserExprToExpr (RelationalE Parser.Lt e1 e2) = Sintax.Lt (parserExprToExpr e1) (parserExprToExpr e2)
parserExprToExpr (RelationalE Parser.Eq e1 e2) = Sintax.Eq (parserExprToExpr e1) (parserExprToExpr e2)
parserExprToExpr (BinaryE Parser.Assig e1 e2) = Sintax.Assig (parserExprToExpr e1) (parserExprToExpr e2)
parserExprToExpr (BinaryE Parser.Seq e1 e2) = Sintax.Seq (parserExprToExpr e1) (parserExprToExpr e2)
parserExprToExpr (BinaryE Parser.App e1 e2) = Sintax.App (parserExprToExpr e1) (parserExprToExpr e2)
parserExprToExpr (Parser.If e1 e2 e3) = Sintax.If (parserExprToExpr e1) (parserExprToExpr e2) (parserExprToExpr e3)
parserExprToExpr (Parser.Let x e1 e2) = Sintax.Let x (parserExprToExpr e1) (parserExprToExpr e2)
parserExprToExpr (Parser.While e1 e2) = Sintax.While (parserExprToExpr e1) (parserExprToExpr e2)

parserTypeToType :: Parser.Type -> Type.Type
parserTypeToType (Parser.Integer) = Type.Integer
parserTypeToType (Parser.Boolean) = Type.Boolean

main = do
  args <- getArgs
  case args of
    [file] -> do
      x <- parseFile file
      let (Typed parserE parserT) = x
      let e = parserExprToExpr parserE
      let t = parserTypeToType parserT
      putStrLn "Program:"
      putStrLn $ " ⊢ " ++ (show e) ++ " : " ++ (show t)
      putStrLn "Evaluation:"
      -- putStrLn $ show (eval e t)
      -- POR AHORA NO VERIFICA EL TIPADO.
      putStrLn $ show (eval e)
    _ -> putStrLn "Error: Invalid file name."
