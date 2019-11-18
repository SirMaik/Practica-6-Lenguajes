-- Practica 6
-- Máquina K con continuaciones y excepciones

module BAE.Sintax where

-- Para ayudarnos
import Data.List


type Identifier = String
type Pending    = ()

--Marcos
data Frame = SuccF      Pending      | PredF        Pending
           | NotF       Pending      | RaiseF       Pending
           | AllocF     Pending      | DerefF       Pending
           | AddFL      Pending Expr | AddFR        Expr   Pending
           | MulFL      Pending Expr | MulFR        Expr   Pending
           | AndFL      Pending Expr | AndFR        Expr   Pending
           | OrFL       Pending Expr | OrFR         Expr   Pending
           | LtFL       Pending Expr | LtFR         Expr   Pending
           | GtFL       Pending Expr | GtFR         Expr   Pending
           | EqFL       Pending Expr | EqFR         Expr   Pending
           | AppFL      Pending Expr | AppFR        Expr   Pending
           | AssigFL    Pending Expr | AssigFR      Expr   Pending
           | ContinueFL Pending Expr | ContinueFR   Expr Pending
           | SeqF       Pending Expr
           | IfF        Pending Expr Expr
           | LetF       Identifier Pending Expr
           | HandleF    Pending Identifier Expr 
           | FnF        Identifier   Pending
           deriving (Eq) 

--Pila de ejecución
type Stack = [Frame]

--Las expresiones del lenguaje
data Expr =  V Identifier | I Int | B Bool
          | Add Expr Expr | Mul Expr Expr | Succ Expr | Pred Expr
          | And Expr Expr | Or  Expr Expr | Not  Expr
          | Lt  Expr Expr | Gt  Expr Expr | Eq Expr Expr
          | If  Expr Expr Expr
          --Funcionales
          | Let Identifier Expr Expr
          | Fn Identifier Expr
          | App Expr Expr
          --Imperativos
          | Void
          | L Int
          | Alloc Expr
          | Deref Expr
          | Assig Expr Expr
          | Seq   Expr Expr
          | While Expr Expr 
          --Excepciones
          | Raise  Expr
          | Handle Expr Identifier Expr
          --Continuaciones
          | LetCC Identifier Expr
          | Continue Expr Expr
          | Cont Stack
          deriving (Eq)

-- Instancia de la clase Show para Frame
instance Show Frame where
  show f = case f of
             SuccF        _  -> "succ"  ++ showUnary
             PredF        _  -> "pred"  ++ showUnary
             NotF         _  -> "not"   ++ showUnary
             RaiseF       _  -> "raise" ++ showUnary
             AllocF       _  -> "alloc"   ++ showUnary
             DerefF       _  -> "deref" ++ showUnary
             AddFR      e1 _ -> "add" ++ showBinaryR e1
             AddFL      _ e2 -> "add" ++ showBinaryL e2
             MulFR      e1 _ -> "mul" ++ showBinaryR e1
             MulFL      _ e2 -> "mul" ++ showBinaryL e2
             AndFR      e1 _ -> "and" ++ showBinaryR e1
             AndFL      _ e2 -> "and" ++ showBinaryL e2
             OrFR       e1 _ -> "or"  ++ showBinaryR e1
             OrFL       _ e2 -> "or"  ++ showBinaryL e2
             GtFR       e1 _ -> "gt"  ++ showBinaryR e1
             GtFL       _ e2 -> "gt"  ++ showBinaryL e2
             EqFR       e1 _ -> "eq"  ++ showBinaryR e1
             EqFL       _ e2 -> "eq"  ++ showBinaryL e2
             LtFR       e1 _ -> "lt"  ++ showBinaryR e1
             LtFL       _ e2 -> "lt"  ++ showBinaryL e2
             AppFR      e1 _ -> "app" ++ showBinaryR e1
             AppFL      _ e2 -> "app" ++ showBinaryL e2
             AssigFR    e1 _ -> "assig" ++ showBinaryR e1
             AssigFL    _ e2 -> "assig" ++ showBinaryL e2
             ContinueFR e1 _ -> "continue" ++ showBinaryR e1
             ContinueFL _ e2 -> "continue" ++ showBinaryL e2
             SeqF       _ e2 -> "seq" ++ showBinaryL e2
             FnF        x _  -> "fn(" ++ x ++ ".-)"
             LetF       x _  e  -> "let(-,"    ++ x ++ "." ++ (show e) ++ ")"
             HandleF    _ x  e  -> "handle(-," ++ x ++ "." ++ (show e) ++ ")"
             IfF        _ e2 e3 -> "if(-,"  ++ (show e2) ++ "," ++ (show e3) ++ ")"  
    where
      showUnary :: String
      showUnary  = "(-)"
      showBinaryL :: Expr -> String
      showBinaryL e2 = "(-," ++ (show e2) ++ ")"
      showBinaryR :: Expr -> String
      showBinaryR e1 = "(" ++ (show e1) ++ ",-)"

--Función para mostrar pilas
showStack :: Stack -> String
showStack []     = "\9633"
showStack (x:xs) = (show x) ++ ";" ++ showStack xs
                       
-- Instancia de la clase Show para Expr
instance Show Expr where
  show e = case e of
             Void  -> "()"
             (V x) -> "V[" ++ x ++ "]"
             (I n) -> "num["  ++ (show n) ++ "]"
             (B b) -> "bool[" ++ (show b) ++ "]"
             (L n) -> "L " ++ (show n)
             --Aridad 1
             (Succ  e') -> "succ" ++ showUnary e'
             (Pred  e') -> "pred" ++ showUnary e'
             (Not   e') -> "not"  ++ showUnary e'
             (Alloc e)  -> "alloc " ++ show e
             (Deref e)  -> '!':(show e)
             (Raise e') -> "raise" ++ showUnary e'
             (Cont  p)  -> "cont"  ++ showStack p
             --Aridad 2
             (Seq e1 e2)      -> (show e1) ++ ";" ++ (show e2)
             (Add e1 e2)      -> "add"   ++ showBinary e1 e2
             (Mul e1 e2)      -> "mul"   ++ showBinary e1 e2
             (And e1 e2)      -> "and"   ++ showBinary e1 e2
             (Or  e1 e2)      -> "or"    ++ showBinary e1 e2
             (Lt  e1 e2)      -> "lt"    ++ showBinary e1 e2
             (Gt  e1 e2)      -> "gt"    ++ showBinary e1 e2
             (Eq  e1 e2)      -> "eq"    ++ showBinary e1 e2
             (While    e1 e2) -> "while" ++ showBinary e1 e2
             (Assig    e1 e2) -> (show e1) ++ " := " ++ show e2
             (App      e1 e2) -> "app"   ++ showBinary e1 e2
             (Fn       x  e') -> "fn("      ++ x ++ "." ++ (show e) ++ ")"
             (LetCC    x  e') -> "letcc("   ++ x ++ "." ++ (show e) ++ ")"
             (Continue e1 e2) -> "continue" ++ showBinary e1 e2
             --Aridad 3
             (If     e1 e2 e3) -> "if("  ++ (show e1) ++ "," ++ (show e2) ++ "," ++ (show e3) ++ ")"
             (Let    x  e1 e2) -> "let(" ++ (show e1) ++ "," ++ x ++ "." ++ (show e2) ++ ")"
             (Handle e1  x e2) -> "handle(" ++ (show e1) ++ ", " ++ x ++ "." ++ (show e2) ++ ")"
    where
      showUnary  :: Expr -> String
      showUnary e = "(" ++ (show e) ++ ")"
      showBinary :: Expr -> Expr -> String
      showBinary e1 e2 = "(" ++ (show e1) ++ "," ++ (show e2) ++ ")"

-- Definiremos el tipo sustitución del siguiente modo:
type Substitution = (Identifier, Expr)

-- Obtiene el conjunto de variables libres de una expresión
frVars :: Expr -> [Identifier]
frVars (V x) = [x]
frVars Void  = []
frVars (L _) = []
frVars (I _)     = []
frVars (B _)     = []
frVars (Cont _)  = []
frVars (Succ  e) = frVars e
frVars (Pred  e) = frVars e
frVars (Not   e) = frVars e
frVars (Raise e) = frVars e
frVars (Alloc e) = frVars e 
frVars (Deref e) = frVars e
frVars (Add   e1 e2)    = (frVars e1) `union` (frVars e2)
frVars (Mul   e1 e2)    = (frVars e1) `union` (frVars e2)
frVars (And   e1 e2)    = (frVars e1) `union` (frVars e2)
frVars (Or    e1 e2)    = (frVars e1) `union` (frVars e2)
frVars (Lt    e1 e2)    = (frVars e1) `union` (frVars e2)
frVars (Gt    e1 e2)    = (frVars e1) `union` (frVars e2)
frVars (Eq    e1 e2)    = (frVars e1) `union` (frVars e2)
frVars (Assig e1 e2)    = (frVars e1) `union` (frVars e2)
frVars (Seq   e1 e2)    = (frVars e1) `union` (frVars e2)
frVars (While e1 e2)    = (frVars e1) `union` (frVars e2)
frVars (Continue e1 e2) = (frVars e1) `union` (frVars e2)
frVars (Let x e1 e2)    = delete x $ (frVars e1) `union` (frVars e2)
frVars (If e1 e2 e3)    = (frVars e1) `union` (frVars e2) `union` (frVars e3)
frVars (Fn x e)         = [w | w <- (frVars e), w /= x]
frVars (LetCC  k e)     = [w | w <- (frVars e), w /= k]
frVars (App e1 e2)      = [w | w <- (frVars e1 `union` frVars e2)]
frVars (Handle e1 x e2) = (frVars e1) `union` [w | w <- (frVars e2), w /= x]


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
alphaExpr (Fn id e)         = let id' = incrVar id
                              in Fn id' (subst e (id , (V id')))
alphaExpr (LetCC id e)      = let id' = incrVar id
                              in LetCC id' (subst e (id , (V id')))   
alphaExpr (Let id e1 e2)    = let id' = incrVar id
                              in Let id' e1 (subst e2 (id, (V id')))
alphaExpr (Handle e1 id e2) = let id' = incrVar id
                              in Handle e1 id' (subst e2 (id, (V id)))
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
                           (I _) -> expr
                           (B _) -> expr
                           --L{} -> expr
                           --Aridad 1
                           (Succ  e) -> Succ  (subst' e)
                           (Pred  e) -> Pred  (subst' e)
                           (Not   e) -> Not   (subst' e)
                           (Raise e) -> Raise (subst' e)
                           (Cont  _) -> expr
                           --(Alloc e) -> Alloc (subst' e)
                           --(Deref e) -> Deref (subst' e)
                           --Aridad 2
                           (Add      e1 e2)  -> Add      (subst' e1) (subst' e2)
                           (Mul      e1 e2)  -> Mul      (subst' e1) (subst' e2)
                           (And      e1 e2)  -> And      (subst' e1) (subst' e2)
                           (Or       e1 e2)  -> Or       (subst' e1) (subst' e2)
                           (Lt       e1 e2)  -> Lt       (subst' e1) (subst' e2)
                           (Gt       e1 e2)  -> Gt       (subst' e1) (subst' e2)
                           (Eq       e1 e2)  -> Eq       (subst' e1) (subst' e2)
                           --(Assig e1 e2)  -> Assig  (subst' e1) (subst' e2)
                           (Seq      e1 e2)  -> Seq      (subst' e1) (subst' e2)
                           (While    e1 e2)  -> While    (subst' e1) (subst' e2)
                           (App      e1 e2)  -> App      (subst' e1) (subst' e2)
                           (Continue e1 e2)  -> Continue (subst' e1) (subst' e2)
                           (Fn id e)
                             | id == x               -> expr
                             | id `elem` (frVars r)  -> subst' (alphaExpr expr)
                             | otherwise             -> Fn id (subst e (x, r))
                           (LetCC id e)
                             | id == x               -> expr
                             | id `elem` (frVars r)  -> subst' (alphaExpr expr)
                             | otherwise             -> LetCC id (subst e (x, r))
                           --Aridad 3
                           (Let id e1 e2)
                             | x == id               -> Let id (subst' e1) e2
                             | id `elem` frVars r    -> subst' (alphaExpr expr)
                             | otherwise             -> Let id (subst' e1) (subst' e2)
                           
                           (Handle e1 id e2)
                             | x == id               -> Handle (subst' e1) id e2
                             | id `elem` (frVars r)  -> subst' (alphaExpr expr)
                             | otherwise             -> Handle (subst' e1) id (subst' e2)
                           (If  e1 e2 e3)     -> If     (subst' e1) (subst' e2) (subst' e3)

                           

--Doble implicación. Supongo que la implementación de (==) es prácticamente lo igual... 
infixr 1 <-> --Asocia a la derecha ya que &&, || asocian a la derecha y su prioridad es menor
(<->) :: Bool -> Bool -> Bool
(<->) True  True  = True
(<->) False False = True
(<->) _     _     = False
-- Función que regresa true si dos expresiones son alfa equivalentes, falso en otro caso.
alphaEq :: Expr -> Expr -> Bool
alphaEq (Cont s1)   (Cont s2) = alphaEqCont (reverse s1) (reverse s2) [] 
  where
    --Se checarán si dos continuaciones son alfa equivalente. Para hacer esto primero se calculó la reversa para ir
    --"del exterior al "al interior". A parte de recibir carga con una lista de restricciones que guarda pares de
    --identificadores. Si el par es (x,y) esto especifica que x y y deben de ser equivalentes,
    --o sea, encontrarse en los mismos lugares de los marcos.
    --Se tomó la decisión de cargar con esta lista para no tener que redefinir sust para marcos. 
    alphaEqCont :: Stack -> Stack -> [(Identifier, Identifier)] ->  Bool
    alphaEqCont []     []     _ = True
    alphaEqCont []     (_:_)  _ = False
    alphaEqCont (_:_)  []     _ = False
    alphaEqCont (a:as) (b:bs) s = case (a,b) of                 
                                    (SuccF{} ,SuccF{})     -> alphaEqTail
                                    (PredF{} ,PredF{})     -> alphaEqTail
                                    (NotF{}  ,NotF{} )     -> alphaEqTail
                                    (RaiseF{},RaiseF{})    -> alphaEqTail
                                    (AllocF{},AllocF{})    -> alphaEqTail
                                    (DerefF{},DerefF{})    -> alphaEqTail
                                    (AddFR       a1 _ , AddFR       b1 _ ) -> (verify a1 b1) && (alphaEq a1 (adjust b1 s)) && alphaEqTail
                                    (AddFL       _  a2, AddFL       _  b2) -> (verify a2 b2) && (alphaEq a2 (adjust b2 s)) && alphaEqTail
                                    (MulFR       a1 _ , MulFR       b1 _ ) -> (verify a1 b1) && (alphaEq a1 (adjust b1 s)) && alphaEqTail
                                    (MulFL       _  a2, MulFL       _  b2) -> (verify a2 b2) && (alphaEq a2 (adjust b2 s)) && alphaEqTail
                                    (AndFR       a1 _ , AndFR       b1 _ ) -> (verify a1 b1) && (alphaEq a1 (adjust b1 s)) && alphaEqTail
                                    (AndFL       _  a2, AndFL       _  b2) -> (verify a2 b2) && (alphaEq a2 (adjust b2 s)) && alphaEqTail
                                    (OrFR        a1 _ , OrFR        b1 _ ) -> (verify a1 b1) && (alphaEq a1 (adjust b1 s)) && alphaEqTail
                                    (OrFL        _  a2, OrFL        _  b2) -> (verify a2 b2) && (alphaEq a2 (adjust b2 s)) && alphaEqTail
                                    (LtFR        a1 _ , LtFR        b1 _ ) -> (verify a1 b1) && (alphaEq a1 (adjust b1 s)) && alphaEqTail
                                    (LtFL        _  a2, LtFL        _  b2) -> (verify a2 b2) && (alphaEq a2 (adjust b2 s)) && alphaEqTail
                                    (GtFR        a1 _ , GtFR        b1 _ ) -> (verify a1 b1) && (alphaEq a1 (adjust b1 s)) && alphaEqTail
                                    (GtFL        _  a2, GtFL        _  b2) -> (verify a2 b2) && (alphaEq a2 (adjust b2 s)) && alphaEqTail
                                    (EqFR        a1 _ , EqFR        b1 _ ) -> (verify a1 b1) && (alphaEq a1 (adjust b1 s)) && alphaEqTail
                                    (EqFL        _  a2, EqFL        _  b2) -> (verify a2 b2) && (alphaEq a2 (adjust b2 s)) && alphaEqTail
                                    (AppFR       a1 _ , AppFR       b1 _ ) -> (verify a1 b1) && (alphaEq a1 (adjust b1 s)) && alphaEqTail
                                    (AppFL       _  a2, AppFL       _  b2) -> (verify a2 b2) && (alphaEq a2 (adjust b2 s)) && alphaEqTail
                                    (AssigFR     a1 _ , AssigFR     b1 _ ) -> (verify a1 b1) && (alphaEq a1 (adjust b1 s)) && alphaEqTail
                                    (AssigFL     _  a2, AssigFL     _  b2) -> (verify a2 b2) && (alphaEq a2 (adjust b2 s)) && alphaEqTail
                                    (ContinueFR  a1 _ , ContinueFR  b1 _ ) -> (verify a1 b1) && (alphaEq a1 (adjust b1 s)) && alphaEqTail
                                    (ContinueFL  _  a2, ContinueFL  _  b2) -> (verify a2 b2) && (alphaEq a2 (adjust b2 s)) && alphaEqTail
                                    (SeqF        _  a2, SeqF        _  b2) -> (verify a2 b2) && (alphaEq a2 (adjust b2 s)) && alphaEqTail
                                    (FnF         x  _ , FnF         y  _ )
                                      | x == y       -> alphaEqTail
                                      | otherwise    -> alphaEqCont as bs ((x,y):s) 
                                    (LetF      x _  a2, LetF     y _  b2)
                                      | x == y       -> alphaEqTail
                                      | otherwise    -> let s' = ((x,y):s)
                                                        in (verify a2 b2) && (alphaEq a2 (adjust b2 s')) && alphaEqCont as bs ((x,y):s') 
                                    (HandleF   _ x  a2, HandleF  _ y  b2)
                                      | x == y       -> alphaEqTail
                                      | otherwise    -> let s' = ((x,y):s)
                                                        in (verify a2 b2) && (alphaEq a2 (adjust b2 s')) && alphaEqCont as bs ((x,y):s') 
                                    (IfF       _ a2 a3, IfF      _ b2 b3)  -> (verify a2 b2) && (verify a3 b3) && (alphaEq a2 (adjust b2 s)) && (alphaEq a3 (adjust b3 s)) && alphaEqTail
                                    _                                      -> False
      where
        alphaEqTail  = alphaEqCont as bs s 
        --Verifica usando la lista de restricciones
        verify a b = foldr (\(x,y) acc -> ((not(x `elem` frVars b) && (y `notElem` frVars a) ||  (x `elem` frVars b) && (y `notElem` frVars a)) && acc)) True s           
        --Hace las sustituciones en la segunda pila
        adjust :: Expr -> [(Identifier, Identifier)]-> Expr
        adjust e []     = e
        adjust e ((x,y):ss) = adjust (subst e (y, V x)) ss
alphaEq (Fn  x a)    (Fn  y b)
  | x == y                                        = alphaEq a b
  | (x `elem` frVars b) && (y `notElem` frVars a) = False 
  | (y `elem` frVars a) && (x `notElem` frVars b) = False
  | otherwise                                     = alphaEq a $ subst b (y, V x)
alphaEq (LetCC  x a) (LetCC  y b)
  | x == y                                        = alphaEq a b
  | (x `elem` frVars b) && (y `notElem` frVars a) = False 
  | (y `elem` frVars a) && (x `notElem` frVars b) = False
  | otherwise                                     = alphaEq a $ subst b (y, V x)
alphaEq (Let x a u)  (Let y b v)
  | x == y                                        = (alphaEq a b) && alphaEq u v 
  | (x `elem` vFree) && (y `notElem` uFree)       = False 
  | (y `elem` uFree) && (x `notElem` vFree)       = False
  | otherwise                                     = (alphaEq a b) && alphaEq u v'
  where
    uFree = frVars u
    vFree = frVars v
    v'    = subst v  (y, V x)
alphaEq (Handle a x u)  (Handle b y v)
  | x == y                                        = (alphaEq a b) && alphaEq u v 
  | (x `elem` vFree) && (y `notElem` uFree)       = False 
  | (y `elem` uFree) && (x `notElem` vFree)       = False
  | otherwise                                     = (alphaEq a b) && alphaEq u v'
  where
    uFree = frVars u
    vFree = frVars v
    v'    = subst v  (y, V x)
alphaEq Void                Void           = True
alphaEq (I a)               (I b)          = a == b
alphaEq (B a)               (B b)          = a <-> b
alphaEq (V a)               (V b)          = a == b
alphaEq (L a)               (L b)          = a == b
alphaEq (Succ  a)           (Succ  b)      = alphaEq a b
alphaEq (Pred  a)           (Pred  b)      = alphaEq a b
alphaEq (Not   a)           (Not   b)      = alphaEq a b
alphaEq (Raise a)           (Raise b)      = alphaEq a b
alphaEq (Deref a)           (Deref b)      = alphaEq a b
alphaEq (Alloc a)           (Alloc b)      = alphaEq a b
alphaEq (Add      a1 a2)    (Add   b1 b2)  = alphaEq a1 b1 && alphaEq a2 b2
alphaEq (Mul      a1 a2)    (Mul   b1 b2)  = alphaEq a1 b1 && alphaEq a2 b2
alphaEq (And      a1 a2)    (And   b1 b2)  = alphaEq a1 b1 && alphaEq a2 b2
alphaEq (Or       a1 a2)    (Or    b1 b2)  = alphaEq a1 b1 && alphaEq a2 b2
alphaEq (Lt       a1 a2)    (Lt    b1 b2)  = alphaEq a1 b1 && alphaEq a2 b2
alphaEq (Gt       a1 a2)    (Gt    b1 b2)  = alphaEq a1 b1 && alphaEq a2 b2
alphaEq (Eq       a1 a2)    (Eq    b1 b2)  = alphaEq a1 b1 && alphaEq a2 b2
alphaEq (Seq      a1 a2)    (Seq   b1 b2)  = alphaEq a1 b1 && alphaEq a2 b2
alphaEq (App      a1 a2)    (App   b1 b2)  = alphaEq a1 b1 && alphaEq a2 b2
alphaEq (Assig    a1 a2)    (Assig b1 b2)  = alphaEq a1 b1 && alphaEq a2 b2
alphaEq (While    a1 a2)    (While b1 b2)  = alphaEq a1 b1 && alphaEq a2 b2
alphaEq (Continue a1 a2) (Continue b1 b2)  = alphaEq a1 b1 && alphaEq a2 b2
alphaEq (If    a1 a2 a3) (If    b1 b2 b3)  = alphaEq a1 b1 && alphaEq a2 b2 && alphaEq a3 b3
alphaEq _ _ = False


--Nota: Bajo esta implementación de alphaEq y subst hay que verificar que la interacción
--      entre Cont P y ambas funciones es correcta.
