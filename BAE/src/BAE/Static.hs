--Práctica 4
--Semántica estática del lenguaje funcional (verificación de tipos)

module BAE.Static where

import Data.Maybe
import Data.List 
import BAE.Type as T 
import BAE.Sintax as S
import BAE.Unifier

type Ctxt = [(S.Identifier, T.Type)]
type Constraint = [(T.Type, T.Type)]
  
--Función que aplica una sustitución (de variables de tipo) a un contexto dado.
subst :: Ctxt -> T.Substitution -> Ctxt
subst s    []   = s
subst s    r    = subst' s r
  where
    subst' :: Ctxt -> T.Substitution -> Ctxt
    subst' []   s'     = []
    subst' (s':ss') r' = (subst'' s' r'):(subst' ss' r')
      where
        subst'' :: (S.Identifier, T.Type) -> T.Substitution -> (S.Identifier, T.Type)
        subst'' (id, t) r'' = (id, T.subst t r'')
--Nota: Al esta función debe de ser llamada de forma restringida (qualified) 

--Función que busca el tipo de una variable en un contexto dado. Devuelve el tipo en caso de ser encontrado y Nothing en otro caso.
find :: S.Identifier -> Ctxt -> Maybe T.Type
find id ctxt = lookup id ctxt


--Dado un conunto de variables de tipo, obtiene una variable de tipo fresa, es decir, que no aparece en este conjunto.
fresh :: [T.Type] -> T.Type
fresh [] = T 0
fresh l = fresh' l 0 
  where
    fresh' :: [T.Type] -> Int -> T.Type
    fresh' l' i
      | (T i) `elem` l' = fresh' l' (i + 1)
      | otherwise         = T i


--Dada una expresión, infiere su tipo implementando las reglas descritas anteriormente. Devolviendo el contexto y el conjunto de restricciones donde es válido. Utiliza el conjunto de variables de tipo para crear variables de tipo frescas durante la ejecucioń.
infer' :: ([T.Type], Expr) -> ([T.Type], Ctxt, T.Type, Constraint)
infer' (varsT, expr) = case expr of
                         (V x)    -> let t = fresh varsT
                                     in  (varsT ++ [t], [(x, t)], t, [])
                         (Fn x e) -> let (varsT', ctxt, t, rest) = infer' (varsT, e)
                                         s                       = BAE.Static.find x ctxt
                                     in case s of
                                          Nothing -> let s' = fresh varsT'
                                                     in (varsT' ++ [s'], ctxt, s' :-> t, rest)
                                          Just s  -> (varsT', delete (x, s) ctxt, s :-> t, rest)
                         (App e1 e2) -> let (varsT1, ctxt1, t1, rest1) = infer' (varsT , e1)
                                            (varsT2, ctxt2, t2, rest2) = infer' (varsT1, e2)
                                            s = findS [ctxt1, ctxt2]
                                            z = fresh varsT2
                                        in (varsT2 ++ [z], ctxt1 ++ ctxt2, z, (t1, t2 :-> z): rest1 ++ rest2 ++ s)
                         (I i)    -> (varsT, [], Integer, [])
                         (B b)    -> (varsT, [], Boolean, [])                                
                         (Succ e) -> unaryInt  e varsT
                         (Pred e) -> unaryInt  e varsT
                         (Not  e) -> unaryBool e varsT
                         (Add e1 e2) -> binaryInt  e1 e2 varsT
                         (Mul e1 e2) -> binaryInt  e1 e2 varsT
                         (Or  e1 e2) -> binaryBool e1 e2 varsT
                         (And e1 e2) -> binaryBool e1 e2 varsT
                         (Lt  e1 e2) -> binaryRel  e1 e2 varsT
                         (Gt  e1 e2) -> binaryRel  e1 e2 varsT
                         (Eq  e1 e2) -> binaryRel  e1 e2 varsT
                         (If  e1 e2 e3) -> let (varsT1, ctxt1, t1, rest1) = infer' (varsT , e1)
                                               (varsT2, ctxt2, t2, rest2) = infer' (varsT1, e2)
                                               (varsT3, ctxt3, t3, rest3) = infer' (varsT2, e3)
                                               s = findS [ctxt1, ctxt2, ctxt3]
                                           in (varsT3, ctxt1 ++ ctxt2 ++ ctxt3, t2,(t1, Boolean):(t2, t3): rest1 ++ rest2 ++ rest3 ++ s)
                         (Let x e1 e2)  -> let (varsT1, ctxt1, t1, rest1) = infer' (varsT , e1)
                                               (varsT2, ctxt2, t2, rest2) = infer' (varsT1, e2)
                                               s                       = findS [ctxt1, ctxt2]
                                               r                       = BAE.Static.find x ctxt2
                                           in case r of
                                                Nothing -> (varsT2 , ctxt1 ++ ctxt2, t2,rest1 ++ rest2 ++ s )
                                                Just r  -> (varsT2, ctxt1 ++ delete (x, r) ctxt2, t2,s ++ rest1 ++ rest2 ++ [(t1,r)])
                                               
  where
    unaryInt :: Expr -> [T.Type] -> ([T.Type], Ctxt, T.Type, Constraint)
    unaryInt  e' varstT = unary e' varsT Integer
    unaryBool :: Expr -> [T.Type] -> ([T.Type], Ctxt, T.Type, Constraint)
    unaryBool e' varstT = unary e' varsT Boolean
    unary :: Expr -> [T.Type] -> T.Type -> ([T.Type], Ctxt, T.Type, Constraint)
    unary e' varsT fType = let (varsT', ctxt, t, rest) = infer' (varsT, e')
                           in  (varsT', ctxt, fType, rest ++[(t,fType)] )  
    binaryInt :: Expr -> Expr ->[T.Type] -> ([T.Type], Ctxt, T.Type, Constraint)
    binaryInt  e1 e2 varsT = binary e1 e2 varsT Integer Integer Integer
    binaryBool :: Expr -> Expr ->[T.Type] -> ([T.Type], Ctxt, T.Type, Constraint)
    binaryBool e1 e2 varsT = binary e1 e2 varsT Boolean Boolean Boolean
    binaryRel  :: Expr -> Expr ->[T.Type] -> ([T.Type], Ctxt, T.Type, Constraint)
    binaryRel  e1 e2 varsT = binary e1 e2 varsT Boolean Integer Integer
    binary :: Expr -> Expr ->[T.Type] -> T.Type -> T.Type -> T.Type ->  ([T.Type], Ctxt, T.Type, Constraint)
    binary e1 e2 varsT fType e1Type e2Type = let (varsT1, ctxt1, t1, rest1) = infer' (varsT , e1)
                                                 (varsT2, ctxt2, t2, rest2) = infer' (varsT1 , e2)
                                                 s = findS [ctxt1, ctxt2]
                                             in (varsT2, ctxt1 ++ ctxt2, fType,rest1 ++ rest2 ++ [(t1, e1Type),(t2,e2Type)] ++ s)
    findS :: [Ctxt] -> Constraint
    findS ctxt = foldl (\acc (c1, c2) -> acc ++ foldl (\acc x -> let y = BAE.Static.find (fst x) c2
                                                                 in if y  == Nothing
                                                                    then acc
                                                                    else (snd x, fromJust y):acc )
                                         [] c1)
                 [] (pairs ctxt)
    pairs :: [a] -> [(a,a)]
    pairs []     = []
    pairs (x:xs) = map (\y -> (x,y)) xs ++ pairs xs

--Dada una expresión, infiere su tipo devolviendo el contexto donde es válido
infer :: Expr -> (Ctxt, Type)
infer expr = let (varsT, ctxt, t, rest) = infer' ([], expr)
                 ctxtµ =  µ rest
             in case ctxtµ of
                  s:[]  -> (BAE.Static.subst ctxt s, t)
                  _     -> error "La ecuación de restricciones no pudo ser resuelta"



