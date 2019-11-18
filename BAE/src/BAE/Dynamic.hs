module BAE.Dynamic where

import BAE.Sintax
import  BAE.Memory

import Data.List (insertBy)

data Type = Integer | Boolean deriving (Show, Eq)

data State = E (Memory, Stack, Expr) | R (Memory, Stack, Expr) | P (Memory, Stack, Expr) 

instance Show State where
  show E (m, s, e) = showWithSymbol "\8826"
  show R (m, s, e) = showWithSymbol "\8827"
  show P (m, s, e) = showWithSymbol "\8828"
    where
      showWithSymbol :: String -> String
      showWIthSymbik sym = "(" ++ (show m) ++ "," ++ (show s) ++ ")" ++ sym ++ show e 

--Determina si una expresión lambda está en forma normal. 
normal :: Expr -> Bool
normal e = case e of
             Fn id a  -> normal a
             App a b -> case a of
                          Fn _ _     -> False
                          otherwise  -> normal a && normal b
             _       -> True

--Función que dice si una expresión es valor o no
isValue :: Expr -> Bool
isValue (I  _) = True
isValue (B  _) = True
isValue (V  _) = True
isValue (L  _) = True
isValue (Fn _) = True
isValue _      = False

--Función que dice si una expresión es una función
isFn :: Expr -> Bool
isFn (Fn _ _) = True
isFn _        = False

--Devuelve la transición tal que eval1 e = e' syss e -> e'
eval1 :: State -> State
eval1 E state@(m,s,e)
  | isValue e && (not . isFn) = R state
  | otherwise   = case e of
                    (Succ  e)    -> E (m, SuccF  (), e)
                    (Pred  e)    -> E (m, PredF  (), e)
                    (Not   e)    -> E (m, NotF   (), e)
                    (Raise e)    -> E (m, RaiseF (), e)
                    (Alloc e)    -> E (m, AllocF (), e)
                    (Deref e)    -> E (m, DerefF (), e)
                    (Fn id e)    -> E (m, FnF id (), e)
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
                    (Let    id e1 e2) -> E (m, (LetF    id () e1):s, e1)
                    (Handle e1 id e2) -> E (m, (HandleF () id e2):s, e1)
                    (While    e1 e2)  -> E (m, s, If e1 (Seq e2 expr) Void)
                    (LetCC  id e   )  -> E (m, s, subst e (id, Cont s))
eval1 R (m,[],e) = R (m,[],Raise e)    
eval1 R (m,st@(s:ss),e)
  | isValue e =  R (m,ss,e)
  | otherwise = case s of
                  (RaiseF  _)
                    | isValue e -> P (m,ss,Raise e)
                    | otherwise -> P (m,[],Raise e) --Como sólo sabemos manejar excepciones con valores podemos colapsar la pila                  
                  (SuccF  _)     -> case e' of
                                      (I n) -> R (m,st,I (n+1) )
                                      _     -> E (m,st,Raise e )
                  (PredF  _)     -> case e' of
                                      (I n)
                                        | n > 0     -> R (m,ss, I(n-1) )
                                        | otherwise -> E (m,st,Raise e )
                                      _      -> E (m,st,Raise e)
                  (NotF   _)     -> case e' of
                                      (B b) -> R (m,ss,B (not b))
                                      _     -> E (m,st,Raise e  )
                  (AllocF _)
                    | isValue e  ->  let (L l) = newAddress m
                                     in  R (insertBy (\x y -> compare (fst x) (fst y)) (l, e) m, s, L l)
                    | otherwise  ->  E (m,st,Raise e)
                  (DerefF _)     -> case e of
                                      (L l) -> let v = access n m
                                               in case v of
                                                    Nothing -> E (m,st,Raise e)
                                                    Just v' -> R (m,ss,v')
                                      _     -> E (m,st,Raise e)
                  (SeqF  _ e2)   -> case e of
                                      Void  -> E (m,ss,e2)
                                      _     -> E (m,st,Raise e)
                  (AddFL () e2)
                    | isValue e -> E (m,(AddFR e ()):ss, e2)
                    | otherwise -> E (m,st,Raise e)
                  (AddFR e1 ())
                    | isValue e -> case (e,e2) of
                                     (I n1,I n2) -> R (m,ss,I (n1 + n2))
                                     (I _ ,_   ) -> E (m,st,Raise e2)
                                     _           -> E (m,st,Raise e)
                         (Assig e1 e2)  -> case e1 of
                                        (L n)     -> case e2 of
                                                       (I _)    -> validateUpdate (n, e2) m
                                                       (B _)    -> validateUpdate (n, e2) m
                                                       (V _)    -> validateUpdate (n, e2) m
                                                       (Fn _ _) -> validateUpdate (n, e2) m
                                                       _        -> let (m', e2') = eval1 (m, e2)
                                                                   in  (m', Assig e1 e2')
                                          where
                                            validateUpdate ::  Cell -> Memory -> (Memory, Expr)
                                            validateUpdate c m = let u = update c m
                                                                 in case u of
                                                                      Nothing -> error $ "Invalid assignation: the address" ++ show n ++ "  hasn't been allocated in the memory: " ++ show m
                                                                      Just m' -> (m', Void) 
                                        _         -> let (m', e1') = eval1 (m, e1)
                                                     in  (m', Assig e1' e2

                    (Let id e1 e2) -> case e1 of
                                        (I _)     -> (m, subst e2 (id, e1))
                                        (B _)     -> (m, subst e2 (id, e1))
                                        (L _)     -> (m, subst e2 (id, e1))
                                        (V _)     -> (m, subst e2 (id, e1))
                                        (Fn _ _)  -> (m, subst e2 (id, e1))
                                        _         -> let (m', e1') = eval1 (m, e1)
                                                     in  (m', Let id e1' e2)
                    (App e1 e2) -> case e1 of
                                     (Fn id e1')
                                       | normal e2   -> (m, subst e1' (id, e2))
                                       | otherwise   -> let (m', e2') = eval1 (m, e2)
                                                        in  (m', App e1 e2')
                                     _
                                       | normal e1   -> let (m', e2') = eval1 (m, e2)
                                                        in  (m', App e1 e2')
                                       | otherwise   -> let (m', e1') = eval1 (m, e1)
                                                        in  (m', App e1' e2)
                    (Add () e2)    -> case e1 of
                                        (I i1) -> case e2 of
                                                    (I i2) -> (m, I (i1 + i2))
                                                    _      -> let (m', e2') = eval1 (m, e2)
                                                              in  (m', Add e1 e2')
                                        _      -> let (m', e1') = eval1 (m, e1)
                                                  in  (m', Add e1' e2)
                    (Mul e1 e2)    -> case e1 of
                                        (I i1) -> case e2 of
                                                    (I i2) -> (m, I (i1 * i2))
                                                    _      -> let (m', e2') = eval1 (m, e2)
                                                              in  (m', Mul e1 e2')
                                        _      -> let (m', e1') = eval1 (m, e1)
                                                  in  (m', Mul e1' e2)
                    (Lt e1 e2)    -> case e1 of
                                        (I i1) -> case e2 of
                                                    (I i2) -> (m, B (i1 < i2))
                                                    _      -> let (m', e2') = eval1 (m, e2)
                                                              in  (m', Lt e1 e2')
                                        _      -> let (m', e1') = eval1 (m, e1)
                                                  in  (m', Lt e1' e2)
                    (Gt e1 e2)    -> case e1 of
                                        (I i1) -> case e2 of
                                                    (I i2) -> (m, B (i1 > i2))
                                                    _      -> let (m', e2') = eval1 (m, e2)
                                                              in  (m', Gt e1 e2')
                                        _      -> let (m', e1') = eval1 (m, e1)
                                                  in  (m', Gt e1' e2)
                    (Eq e1 e2)    -> case e1 of
                                        (I i1) -> case e2 of
                                                    (I i2) -> (m, B (i1 == i2))
                                                    _      -> let (m', e2') = eval1 (m, e2)
                                                              in  (m', Eq e1 e2')
                                        _      -> let (m', e1') = eval1 (m, e1)
                                                  in  (m', Eq e1' e2)
                    (And e1 e2)    -> case e1 of
                                        (B False) -> (m, B False)
                                        (B True ) -> case e2 of
                                                       (B False) -> (m, B False)
                                                       (B True)  -> (m, B True)
                                                       _         -> let (m', e2') = eval1 (m, e2)
                                                                    in  (m', And e1 e2')
                                        _         -> let (m', e1') = eval1 (m, e1)
                                                                    in  (m', And e1' e2)
                    (Or e1 e2)    -> case e1 of
                                        (B True ) -> (m, B True)
                                        (B False) -> case e2 of
                                                       (B False) -> (m, B False)
                                                       (B True)  -> (m, B True)
                                                       _         -> let (m', e2') = eval1 (m, e2)
                                                                    in  (m', Or e1 e2')
                                        _         -> let (m', e1') = eval1 (m, e1)
                                                                    in  (m', Or e1' e2)
                    (If e1 e2 e3)  -> case e1 of
                                        (B True)  -> (m, e2)
                                        (B False) -> (m, e3)
                                        _         -> let (m', e1') = eval1 (m, e1)
                                                     in  (m', If e1' e2 e3)



-- Devuelve la transición tal que evals e = e’ syss e →∗ e' y e' está bloqueado
evals :: (Memory, Expr) -> (Memory, Expr)
evals x@(m,e)
  | isCorrupted m = x
  | isItBlocked e = x
  | otherwise     = (evals . eval1) x
    where
      --Dice si una expresión está bloqueada o no
      isItBlocked :: Expr -> Bool
      isItBlocked e = case e of
                             (Alloc e)      -> case e of
                                                 (I _)    -> False
                                                 (B _)    -> False
                                                 (V _)    -> False
                                                 (L _)    -> False
                                                 (Fn _ _) -> False
                                                 _        -> isItBlocked e
                             (Deref e)      -> case e of
                                                 (L l)  -> verifyAllocation l m
                                                 _      -> isItBlocked e
                             (Succ e')      -> isItBlocked1Int e'
                             (Pred e')      -> isItBlocked1Int e'
                             (Not  e')      -> case e' of
                                                 B{} -> False
                                                 _   -> isItBlocked e'
                             (While _ _)    -> False                    
                             (Add e1 e2)    -> isItBlocked2Ints e1 e2           
                             (Mul e1 e2)    -> isItBlocked2Ints e1 e2
                             (Lt  e1 e2)    -> isItBlocked2Ints e1 e2
                             (Gt  e1 e2)    -> isItBlocked2Ints e1 e2
                             (Eq  e1 e2)    -> isItBlocked2Ints e1 e2
                             (And e1 e2)    -> isItBlocked2Bools e1 e2
                             (Or  e1 e2)    -> isItBlocked2Bools e1 e2
                             (Seq e1 e2)    -> case e1 of
                                                 B{} -> True
                                                 I{} -> True
                                                 L{} -> True
                                                 V{} -> True
                                                 _   -> False
                             (Assig e1 e2)  -> case e1 of
                                                 (L l)
                                                   |otherwise -> case e2 of
                                                                   B{}      -> verifyAllocation l m
                                                                   I{}      -> verifyAllocation l m
                                                                   V{}      -> verifyAllocation l m
                                                                   L{}      -> verifyAllocation l m
                                                                   (Fn _ _) -> verifyAllocation l m
                                                                   _        -> isItBlocked e2
                                                 _   -> isItBlocked e1
                             (If e1 _ _)    -> case e1 of
                                                 B{} -> False
                                                 _   -> isItBlocked e1
                             (Let _ e1 _)   -> case e1 of
                                                 B{}  -> False
                                                 I{}  -> False
                                                 Fn{} -> False
                                                 L{}  -> False
                                                 _    -> isItBlocked e1
                             (Fn id e')     -> isItBlocked e'                    
                             (App e1 e2)
                               | normal e1 && normal e2 -> case e1 of
                                                             (Fn id e1') -> False
                                                             _           -> True      
                               | otherwise              -> False
                             _              -> True
        where
          --Checa si una dirección de memoria fue alojada
          verifyAllocation :: Address -> Memory -> Bool
          verifyAllocation l m = let v = lookup l m
                                 in case v of
                                      Nothing -> True
                                      _       -> False
          --Para expresiones que reciben un argumento entero
          isItBlocked1Int :: Expr -> Bool
          isItBlocked1Int I{} = False
          isItBlocked1Int B{} = True
          isItBlocked1Int e@_ = isItBlocked e
          --Para expresiones que reciben dos argunmentos enteros
          isItBlocked2Ints :: Expr -> Expr -> Bool
          isItBlocked2Ints e1 e2 = case e1 of
                                     B{} -> True
                                     I{} -> case e2 of
                                              B{} -> True
                                              I{} -> False
                                              _   -> isItBlocked e2
                                     _   -> isItBlocked e1
          --Para expresiones que reciben tres argumentos enteros
          isItBlocked2Bools :: Expr -> Expr -> Bool
          isItBlocked2Bools e1 e2 = case e1 of
                                      I{} -> True
                                      B{} -> case e2 of
                                               I{} -> True
                                               B{} -> False
                                               _    -> isItBlocked e2
                                      _   -> isItBlocked e1

          
-- Devuelve la evaluación de un programa tal que evale e = e’ syss e →∗ e' y e'
-- es un valor. En caso de que e' no esa un valor deberá mostrar un mensaje
-- de error particular del operador que lo causó
evale :: Expr -> Expr
evale e = let (m, e') = evals ([], e)         
          in if isCorrupted m
             then error "Corrupted memory."
             else case e' of
                    B{}      -> e'
                    I{}      -> e'
                    V{}      -> e'
                    L{}      -> e'
                    (Fn _ _) -> e'
                    _      -> let e'' = itsBlockedIn e'
                              in case e'' of
                                   Void      -> error "[Void] is not a value."
                                   Succ{}    -> error "[Succ] expects an Integer."
                                   Pred{}    -> error "[Pred] expects an Integer."
                                   Add{}     -> error "[Add] expects two Integer."
                                   Mul{}     -> error "[Mul] expects two Integer."
                                   Lt{}      -> error "[Lt] expects two Integer."
                                   Gt{}      -> error "[Gt] expects two Integer."
                                   Eq{}      -> error "[Eq] expects two Integer."
                                   Not{}     -> error "[Not] expects a Boolean."
                                   Or{}      -> error "[Or] expects two Boolean."
                                   And{}     -> error "[And] expects two Booleas."
                                   If{}      -> error "[If] expects its first argument to be Boolean."
                                   App{}     -> error "[App] expects a function as first argument."
                                   Fn{}      -> error "[Fn] is expected to be the first argument of an [App] Expr."
                                   Seq{}     -> error "[Seq] expects its first argument to be void."
                                   (Deref f) -> case f of
                                                  L{} -> error $ "[Deref] couldn't find memory location " ++ show f ++ " in the memory " ++ show m ++ "."
                                                  _   -> error   "[Deref] expects a memory location."
                                   (Assig e1 _) -> case e1 of
                                                     (L l) -> error $ "[Assig] couldn't find memory location " ++ show e1 ++ " in the memory " ++ show m ++ "."
                                                     _     -> error   "[Assig] expects a memory location as its first argument."
                    
                      where
                        --Dice exactamente cual es la subexpresión en la que se bloqueó la expresión 
                        itsBlockedIn :: Expr -> Expr
                        itsBlockedIn e = case e of
                                           (Succ e')      -> itsBlockedIn1Int  e e'
                                           (Pred e')      -> itsBlockedIn1Int  e e'
                                           (Not  e')      -> itsBlockedIn1Bool e e'
                                           (Add e1 e2)    -> itsBlockedIn2Ints  e e1 e2
                                           (Mul e1 e2)    -> itsBlockedIn2Ints  e e1 e2
                                           (Lt  e1 e2)    -> itsBlockedIn2Ints  e e1 e2  
                                           (Gt  e1 e2)    -> itsBlockedIn2Ints  e e1 e2
                                           (Eq  e1 e2)    -> itsBlockedIn2Ints  e e1 e2
                                           (And e1 e2)    -> itsBlockedIn2Bools e e1 e2
                                           (Or  e1 e2)    -> itsBlockedIn2Bools e e1 e2
                                           (If  I{}  _ _) -> e
                                           (If  L{}  _ _) -> e
                                           (If  e1   _ _) -> itsBlockedIn e1
                                           (Let _ e1 _)   -> itsBlockedIn e1
                                           (Alloc e')     -> itsBlockedIn e'
                                           (Deref I{})    -> e
                                           (Deref B{})    -> e
                                           (Deref L{})    -> e
                                           (Deref e')     -> itsBlockedIn e'
                                           (Seq L{}  _)   -> e
                                           (Seq B{}  _)   -> e
                                           (Seq I{}  _)   -> e
                                           (Seq Void e2)  -> itsBlockedIn e2
                                           (Seq e1   _)   -> itsBlockedIn e1
                                           (Assig  B{} _) -> e
                                           (Assig  I{} _) -> e
                                           (Assig  V{} _) -> e
                                           (Assig L{} e2) -> case e2 of
                                                               B{}      -> e
                                                               I{}      -> e
                                                               V{}      -> e
                                                               L{}      -> e
                                                               (Fn _ _) -> e
                                                               _        -> itsBlockedIn e2
                                           (Assig e1   _) -> itsBlockedIn e1
                                           _              -> e
                        --Para expresiones que reciben un argumento entero
                        itsBlockedIn1Int :: Expr -> Expr -> Expr
                        itsBlockedIn1Int e e' = case e' of
                                                  B{}  -> e
                                                  L{}  -> e
                                                  _    -> itsBlockedIn e'
                        --Para expresiones qu reciben un argumento entero
                        itsBlockedIn1Bool :: Expr -> Expr -> Expr
                        itsBlockedIn1Bool e e' = case e' of
                                                   I{}  -> e
                                                   L{}  -> e
                                                   _    -> itsBlockedIn e'
                        --Para expresiones que reciben dos argumentos enteros
                        itsBlockedIn2Ints :: Expr -> Expr -> Expr ->Expr
                        itsBlockedIn2Ints e e1 e2 = case e1 of                           
                                                      B{}  -> e
                                                      L{}  -> e
                                                      I{}  -> case e2 of
                                                                B{}  -> e
                                                                L{}  -> e
                                                                _    -> itsBlockedIn e2
                                                      _    -> itsBlockedIn e1
                        --Para expresiones que reciben tres argumentos enteros
                        itsBlockedIn2Bools :: Expr -> Expr -> Expr -> Expr
                        itsBlockedIn2Bools e e1 e2 = case e1 of
                                                       I{}  -> e
                                                       L{}  -> e
                                                       B{}  -> case e2 of
                                                                 I{}  -> e
                                                                 L{}  -> e
                                                                 _    -> itsBlockedIn e2
                                                       _    -> itsBlockedIn e1



  
