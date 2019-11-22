module BAE.Dynamic where

import BAE.Sintax
import BAE.Memory

import Data.List (insertBy)

data Type = Integer | Boolean deriving (Show, Eq)

data State = E (Memory, Stack, Expr) | R (Memory, Stack, Expr) | P (Memory, Stack, Expr) 

showWithSymbol :: String -> Memory -> Stack -> Expr -> String
showWithSymbol sym m s e = "(" ++ (show m) ++ ", " ++ (show s) ++ ") " ++ sym ++ " " ++ show e 

instance Show State where
  show (E (m, s, e)) = showWithSymbol "\8827" m s e
  show (R (m, s, e)) = showWithSymbol "\8826" m s e
  show (P (m, s, e)) = showWithSymbol "\8828" m s e

--Determina si una expresión lambda está en forma normal. 
normal :: Expr -> Bool
normal e = case e of
             Fn id a -> normal a
             App a b -> case a of
                          Fn _ _     -> False
                          otherwise  -> normal a
             _       -> True

--Función que dice si una expresión es valor o no
isValue :: Expr -> Bool
isValue e@(Fn _ _) = True
isValue (Cont _)   = True
isValue (I  _)     = True
isValue (B  _)     = True
isValue (V  _)     = True
isValue (L  _)     = True
isValue (Void)     = True
isValue _          = False

--Función que dice si una expresión es una función
--isFn :: Expr -> Bool
--isFn (Fn _ _) = True
--isFn _        = False

--Devuelve la transición tal que eval1 e = e' syss e -> e'
eval1 :: State -> State
eval1 (E (m,s,e))
  | isValue e   = R (m,s,e)
  | otherwise   = case e of
                    (Fn id e')     -> E (m, (FnF id ()):s,e')
                    (Succ  e')     -> E (m, (SuccF  ()):s,e')
                    (Pred  e')     -> E (m, (PredF  ()):s,e')
                    (Not   e')     -> E (m, (NotF   ()):s,e')
                    (Raise e')     -> E (m, (RaiseF ()):s,e')
                    (Alloc e')     -> E (m, (AllocF ()):s,e')
                    (Deref e')     -> E (m, (DerefF ()):s,e')
                    (Add       e1 e2) -> E (m, (AddFL      () e2):s, e1)
                    (Mul       e1 e2) -> E (m, (MulFL      () e2):s, e1)
                    (And       e1 e2) -> E (m, (AndFL      () e2):s, e1)
                    (Or        e1 e2) -> E (m, (OrFL       () e2):s, e1)
                    (Lt        e1 e2) -> E (m, (LtFL       () e2):s, e1)
                    (Gt        e1 e2) -> E (m, (GtFL       () e2):s, e1)
                    (Eq        e1 e2) -> E (m, (EqFL       () e2):s, e1)
                    (App       e1 e2) -> E (m, (AppFL      () e2):s, e1)
                    (Assig     e1 e2) -> E (m, (AssigFL    () e2):s, e1)
                    (Continue  e1 e2) -> E (m, (ContinueFL () e2):s, e1)
                    (Seq       e1 e2) -> E (m, (SeqF       () e2):s, e1)
                    (If     e1 e2 e3) -> E (m, (IfF     () e2 e3):s, e1)
                    (Let    id e1 e2) -> E (m, (LetF    id () e2):s, e1)
                    (Handle e1 id e2) -> E (m, (HandleF () id e2):s, e1)
                    (While    e1 e2)  -> E (m, s, If e1 (Seq e2 e) Void)
                    (LetCC  id e   )  -> E (m, s, subst e (id, Cont s))
eval1 (R (m,[],e))        = P (m,[],Raise e)    
eval1 (R (m,st@(s:ss),e))
  --El siguiente caso sólo se da si se llama con argumentos incompatibles a eval1
  | (not . isValue) e = E (m,(RaiseF ()):st,e)
  | otherwise         = case s of
                            (RaiseF _)     -> P (m,ss,Raise e) 
                            (SuccF  _)     -> case e of
                                                (I n) -> R (m,ss,I (n+1))
                                                _     -> P (m,ss,Raise e)
                            (PredF  _)     -> case e of
                                                (I n)
                                                  | n > 0     -> R (m,ss, I(n-1))
                                                  | otherwise -> P (m,ss,Raise e)
                                                _      -> P (m,ss,Raise e)
                            (NotF   _)     -> case e of
                                                (B b) -> R (m,ss,B (not b))
                                                _     -> P (m,ss,Raise e  )
                            (AllocF _)     -> let (L l) = newAddress m
                                              in  R (insertBy (\x y -> compare (fst x) (fst y)) (l, e) m,ss, L l)
                            (DerefF _)     -> case e of
                                                (L l) -> let v = access l m
                                                         in case v of
                                                              Nothing -> P (m,ss,Raise e)
                                                              Just v' -> R (m,ss,v')
                                                _     -> P (m,st,Raise e)
                            (AddFL () e2)  -> E (m,(AddFR e ()):ss, e2)
                            (AddFR e1 ())  -> case e1 of
                                                (I n1) -> case e of
                                                            (I n2) -> R (m,ss,I (n1 + n2))
                                                            _      -> P (m,ss,Raise e)
                                                _      -> P (m,ss,Raise e1)
                            (MulFL () e2)  -> E (m,(MulFR e ()):ss, e2)
                            (MulFR e1 ())  -> case e1 of
                                                (I n1) -> case e of
                                                            (I n2) -> R (m,ss,I (n1 * n2))
                                                            _      -> P (m,ss,Raise e)
                                                _      -> P (m,ss,Raise e1)
                            (AndFL () e2)  -> E (m,(AndFR e ()):ss, e2)
                            (AndFR e1 ())  -> case e1 of
                                                (B b1) -> case e of
                                                            (B b2) -> R (m,ss,B (b1 && b2))
                                                            _      -> P (m,ss,Raise e)
                                                _      -> P (m,ss,Raise e1)
                            (OrFL () e2)   -> E (m,(OrFR e ()):ss, e2)
                            (OrFR e1 ())   -> case e1 of
                                                (B b1) -> case e of
                                                            (B b2) -> R (m,ss,B (b1 || b2))
                                                            _      -> P (m,ss,Raise e)
                                                _      -> P (m,ss,Raise e1)
                            (LtFL () e2)   -> E (m,(LtFR e ()):ss, e2)
                            (LtFR e1 ())   -> case e1 of
                                                (I n1) -> case e of
                                                            (I n2) -> R (m,ss,B (n1 < n2))
                                                            _      -> P (m,ss,Raise e)
                                                _      -> P (m,ss,Raise e1)
                            (GtFL () e2)  -> E (m,(GtFR e ()):ss, e2)
                            (GtFR e1 ())  -> case e1 of
                                               (I n1) -> case e of
                                                           (I n2) -> R (m,ss,B (n1 > n2))
                                                           _      -> P (m,ss,Raise e)
                                               _      -> P (m,ss,Raise e1)
                            (EqFL () e2)  -> E (m,(EqFR e ()):ss, e2)
                            (EqFR e1 ())  -> case e1 of
                                               (I n1) -> case e of
                                                           (I n2) -> R (m,ss,B (n1 == n2))
                                                           _      -> P (m,ss,Raise e)
                                               _      -> P (m,ss,Raise e1)
                            (AppFL () e2) -> case e of
                                               (Fn id f) -> E (m,ss,subst f (id,e2))
                                               _         -> P (m,ss,Raise e)
                            --(AppFR e1 ()) -> case e1 of
                            --                   (Fn id f) -> E (m,ss,subst f (id, e))
                            --                   _         -> P (m,ss,Raise e1)
                            (SeqF  _ e2)  -> case e of
                                                Void  -> E (m,ss,e2)
                                                _     -> P (m,ss,Raise e)
                            (AssigFL () e2) -> E (m,(AssigFR e ()):ss, e2)
                            (AssigFR e1 ()) -> case e1 of
                                                 (L l) -> let u = update (l,e) m
                                                          in case u of
                                                               Nothing -> P (m,ss,Raise e1)
                                                               Just m' -> R (m',ss, Void)
                                                 _     -> P (m,ss,Raise e1)
                            (ContinueFL () e2) -> E (m,(ContinueFR e ()):ss, e2)
                            (ContinueFR e1 ()) -> case e1 of
                                                    (Cont st') -> R (m, st', e )
                                                    _          -> P (m, st,  e1)
                            (FnF id ())        -> R (m,ss,Fn id e)
                            (IfF () e1 e2)     -> case e of
                                                    (B True ) -> E (m, ss, e1)
                                                    (B False) -> E (m, ss, e2)
                                                    _         -> P (m, ss, Raise e)
                            (LetF id () e2)    -> E (m,ss, subst e2 (id, e))
                            (HandleF () _ _)   -> R (m,ss,e)
eval1 (P (m,(s:ss),e@(Raise e')))  = case s of
                                       (HandleF () id e2) -> E (m, ss, subst e2 (id, e'))
                                       _                  -> P (m, ss,e)
eval1 _ =  P ([],[],Void) -- <- Esto nunca debería de suceder


-- Devuelve la transición tal que evals e = e’ syss e →∗ e' y e' está bloqueado
evals :: State -> State
evals state = case state of
                (E (m,s,e))
                  | isCorrupted m -> state
                  | otherwise     -> evals $ eval1 state
                (R (m,s,e))
                  | isCorrupted m -> state
                  | otherwise     -> case s of
                                       [] -> state
                                       _  -> evals $ eval1 state
                (P (m,s,e))
                  | isCorrupted m -> state
                  | otherwise     -> case s of
                                       [] -> state
                                       _  -> evals $ eval1 state
          
-- Devuelve la evaluación de un programa tal que evale e = e’ syss e →∗ e' y e'
-- es un valor. En caso de que e' no esa un valor deberá mostrar un mensaje
-- de error particular del operador que lo causó
evale :: Expr -> Expr
evale e = case evals (E ([],[],e)) of
            (R (_,[],e'))
              | isValue e' -> e'
              | otherwise  -> error "Error." --error $ "The resulting expression " ++ (show e') ++ " is not a value."
            (P (_,[],Raise(e'))) -> error "Error." --error $ "Exception " ++ (show e') ++ " raised."
            _ -> error "Error." 
              

evalN :: State -> Int -> State
evalN s 0 = s
evalN s i = evalN (eval1 s) (i - 1)
  
