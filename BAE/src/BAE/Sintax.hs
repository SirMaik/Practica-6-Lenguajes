-- Practica 
-- Integrando el núcleo funcional a EAB

module BAE.Sintax where

-- Para ayudarnos
import Data.List


-- Definiremos las expresiones del cálculo lambda del siguiente modo;
type Identifier = String


-- Agregaremos a las expresiones los constructores de una abstracción
-- lambda (que de ahora en adelante llamaremos función) y el de la aplicación
-- Una expresión puede ser:
-- V : Una Variable
-- Fn : Una abstracción lambda (función)
-- App : Una aplicación
data Expr =  V Identifier | I Int | B Bool
          | Add Expr Expr | Mul Expr Expr | Succ Expr | Pred Expr
          | And Expr Expr | Or  Expr Expr | Not  Expr
          | Lt  Expr Expr | Gt  Expr Expr | Eq Expr Expr
          | If  Expr Expr Expr
          --Funcionales
          | Let Identifier Expr Expr
          | Fn Identifier Expr
          | App Expr Expr
          --Inperativos
          | Void
          | L Int
          | Alloc Expr
          | Deref Expr
          | Assig Expr Expr
          | Seq Expr Expr
          | While Expr Expr
          deriving (Eq)



-- Instancia de la clase Show para Expr
instance Show Expr where
    show e = case e of
                Void  -> "()"
                (V x) -> "V[" ++ x ++ "]"
                (I n) -> "num["  ++ (show n) ++ "]"
                (B b) -> "bool[" ++ (show b) ++ "]"
                (L n) -> "L " ++ (show n)  
                --Aridad 1
                (Succ  e) -> "succ(" ++ (show e) ++ ")"
                (Pred  e) -> "pred(" ++ (show e) ++ ")"
                (Not   e) -> "not("  ++ (show e) ++ ")"
                (Alloc e) -> "alloc " ++ (show e)
                (Deref e) -> '!':(show e)
                --Aridad 2
                (Seq e1 e2) -> (show e1) ++ ";" ++ (show e2)
                (Add e1 e2) -> "add(" ++ (show e1) ++ "," ++(show e2) ++ ")"
                (Mul e1 e2) -> "mul(" ++ (show e1) ++ "," ++(show e2) ++ ")"
                (And e1 e2) -> "and(" ++ (show e1) ++ "," ++(show e2) ++ ")"
                (Or  e1 e2) -> "or("  ++ (show e1) ++ "," ++(show e2) ++ ")"
                (Lt  e1 e2) -> "lt("  ++ (show e1) ++ "," ++(show e2) ++ ")"
                (Gt  e1 e2) -> "gt("  ++ (show e1) ++ "," ++(show e2) ++ ")"
                (Eq  e1 e2) -> "eq("  ++ (show e1) ++ "," ++(show e2) ++ ")"
                (While e1 e2) -> "while(" ++ (show e1) ++ "," ++(show e2) ++ ")"
                (Assig e1 e2) -> (show e1) ++ " := " ++ (show e2)
                (Let a e1 e2) -> "let(" ++ (show e1) ++ "," ++ a ++ "." ++ (show e2) ++ ")"
                --Aridad 3
                (If  e1 e2 e3) -> "if("  ++ (show e1) ++ "," ++ (show e2) ++ "," ++ (show e3) ++ ")"
                (Fn x e) -> "fn(" ++ x ++ "." ++ (show e) ++ ")"
                (App e1 e2) -> "app(" ++ (show e1) ++ ", " ++ (show e2) ++ ")"



-- Definiremos el tipo sustitución del siguiente modo:
type Substitution = (Identifier, Expr)


-- Obtiene el conjunto de variables libres de una expresión
frVars :: Expr -> [Identifier]
frVars (V x) = [x]
frVars Void  = []
frVars (L _) = []
frVars (I _) = []
frVars (B _) = []
frVars (Succ  e) = frVars e
frVars (Pred  e) = frVars e
frVars (Not   e) = frVars e
frVars (Alloc e) = frVars e 
frVars (Deref e) = frVars e
frVars (Add   e1 e2) = (frVars e1) `union` (frVars e2)
frVars (Mul   e1 e2) = (frVars e1) `union` (frVars e2)
frVars (And   e1 e2) = (frVars e1) `union` (frVars e2)
frVars (Or    e1 e2) = (frVars e1) `union` (frVars e2)
frVars (Lt    e1 e2) = (frVars e1) `union` (frVars e2)
frVars (Gt    e1 e2) = (frVars e1) `union` (frVars e2)
frVars (Eq    e1 e2) = (frVars e1) `union` (frVars e2)
frVars (Assig e1 e2) = (frVars e1) `union` (frVars e2)
frVars (Seq   e1 e2) = (frVars e1) `union` (frVars e2)
frVars (While e1 e2) = (frVars e1) `union` (frVars e2)
frVars (Let a e1 e2) = (frVars e1) `union` (frVars e2) \\ [a]
frVars (If e1 e2 e3) = (frVars e1) `union` (frVars e2) `union` (frVars e3)
frVars (Fn x e) = [w | w <- (frVars e), w /= x]
frVars (App e1 e2) = [w | w <- (frVars e1 `union` frVars e2)]


--Dado un identificador, si este no termina en número  le agrega el sufijo 1,
--en caso contrario toma el valor del número y lo incrementa en 1.  
incrVar :: Identifier -> Identifier
incrVar id = (merge . incrementSnd . separate . parse) (id, "", "", "")
  where
    parse :: (Identifier, Identifier, Identifier, Identifier) -> (Identifier, Identifier, Identifier, Identifier)
    parse (toParse , accT, accP, accN) = case toParse of
                                           ""     -> (toParse, accT, accP, accN) 
                                           (x:xs)
                                             | x `elem` ['1'..'9'] -> parse (xs, x:accT, accP, x:accN)
                                             | otherwise           -> parse (xs, x:accT, x:accT, "")
    separate :: (Identifier, Identifier, Identifier, Identifier) -> (Identifier, Identifier)
    separate (_, _, s, n) = (s, n)
    incrementSnd :: (Identifier, Identifier) -> (Identifier, Identifier)
    incrementSnd (s, n) = (s, increment n)
      where 
          increment n = case n of
                        ""     -> "1"
                        '9':xs -> '0':(increment xs)
                        x:xs   -> (succ x):xs
    merge :: (Identifier, Identifier) -> Identifier
    merge (s, n) = reverse s ++ reverse n


-- Toma una expresión que involucre el ligado de una variable y devuelve una
-- α-equivalente utilizando la función incrVar hasta encontrar un nombre
-- que no aparezca en el cuerpo
alphaExpr :: Expr -> Expr
alphaExpr (Fn id a)      = let id' = incrVar id
                           in Fn id' (subst a (id , (V id')))
alphaExpr (Let id e1 e2) = let id' = incrVar id
                           in Let id' e1 (subst e2 (id, (V id')))
alphaExpr e = e



-- Aplica la sustitución a la expresión dada (Utiliza la función alphaExpr
-- para implementar una función total)
subst :: Expr -> Substitution -> Expr
subst expr s@(x, r) = let subst' = flip subst s
                      in case expr of
                            --Aridad 0
                           Void -> Void
                           (V y)
                             | x == y    -> r
                             | otherwise -> expr
                           I{} -> expr
                           B{} -> expr
                           L{} -> expr
                           --Aridad 1
                           (Succ  e) -> Succ  (subst' e)
                           (Pred  e) -> Pred  (subst' e)
                           (Not   e) -> Not   (subst' e)
                           (Alloc e) -> Alloc (subst' e)
                           (Deref e) -> Deref (subst' e)
                           --Aridad 2
                           (Add   e1 e2)  -> Add    (subst' e1) (subst' e2)
                           (Mul   e1 e2)  -> Mul    (subst' e1) (subst' e2)
                           (And   e1 e2)  -> And    (subst' e1) (subst' e2)
                           (Or    e1 e2)  -> Or     (subst' e1) (subst' e2)
                           (Lt    e1 e2)  -> Lt     (subst' e1) (subst' e2)
                           (Gt    e1 e2)  -> Gt     (subst' e1) (subst' e2)
                           (Eq    e1 e2)  -> Eq     (subst' e1) (subst' e2)
                           (Assig e1 e2)  -> Assig  (subst' e1) (subst' e2)
                           (Seq   e1 e2)  -> Seq    (subst' e1) (subst' e2)
                           (While e1 e2)  -> While  (subst' e1) (subst' e2)
                           (Let a e1 e2)
                             | x == a            -> Let a (subst' e1) e2
                             | a `elem` frVars r -> subst' (alphaExpr expr)
                             | otherwise         -> Let a (subst' e1) (subst' e2)
                           --Aridad 3
                           (If e1 e2 e3) -> If (subst' e1) (subst' e2) (subst' e3)
                           (Fn a e)
                             | a == x              -> expr
                             | a `elem` (frVars r) -> subst' (alphaExpr expr)
                             | otherwise -> (Fn a (subst e (x, r)))
                           (App e1 e2) -> App (subst' e1) (subst' e2)




