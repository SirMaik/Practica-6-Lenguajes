{-|
Module      : Unifier
Description : Provides the implementation of Martelli Montanari Unification Algorithm for Types.
Maintainer  : pablog@ciencias.unam.mx
-}
module BAE.Unifier (µ) where

import BAE.Type

-- |The 'µ' function returns the most general unifier of a set of type equations.
µ :: [(Type, Type)] -> [Substitution]
µ [] = [[]]
µ ((t1, t2) : ts) = [comp s1 s2 | s1 <- unify t1 t2, s2 <- µ [(subst (fst t) s1, subst (snd t) s1) | t <- ts]]

-- |The 'unify' function unify a pair of types.
unify :: Type -> Type -> [Substitution]
unify (T x) (T y) = if x == y then [[]] else [[(x, T y)]]
unify (T x) t = if elem x (vars t) then
                  error ("Unification Fails. (" ++ show (T x) ++ " = " ++ show t ++ ")")
                else return [(x, t)]
unify t (T x) = unify (T x) t
unify (t1 :-> t2) (t3 :-> t4) = [comp s1 s2 | s1 <- (unify t1 t3), s2 <- (unify (subst t2 s1) (subst t4 s1))]
unify t s = if t == s then [[]] else error ("Unification Fails. (" ++ show t ++ " = " ++ show s ++ ")")
