--Práctica 4
--Semántica estática del lenguaje funcional (verificación de tipos)

module BAE.Type where

--Para ayudarnos
import Data.List

type Identifier = Int

infix :->

data Type = T Identifier
          | Integer | Boolean
          | Type :-> Type deriving (Eq)

type Substitution = [(Identifier, Type)]

--Instancia de la clase Show para Type
instance Show Type where
  show (t1 :-> t2@(_ :-> _)) = show t1 ++ " -> (" ++ show t2 ++ ")"
  show (t1 :-> t2)           = show t1 ++ " -> " ++ show t2  
  show (T id)                = 'T' : show id
  show Integer               = "Integer"
  show Boolean               = "Boolean"

-- Función que devuelve el conjunto de variables de tipo.
vars :: Type -> [Identifier]
vars (T id)      = [id]
vars (t1 :-> t2) = vars t1 `union` vars t2
vars _           = []

-- Función que aplica la sustitución a un tipo dado.
subst :: Type -> Substitution -> Type
subst t@(T id)    s = subst' t s
  where
    subst' :: Type -> Substitution -> Type
    subst' t' []            = t'
    subst' t'@(T id') (s':ss)
      | id' == fst s' = snd s'
      | otherwise     = subst' t' ss 
subst (t1 :-> t2) s = (subst t1 s) :-> (subst t2 s)
subst t           s = t


-- Función que realiza la composición de dos sustituciones.
comp :: Substitution -> Substitution -> Substitution
comp s    []   = s
comp s    r    = unionBy (\x y -> fst x == fst y) (simpl (comp' s r)) r
  where
    comp' :: Substitution -> Substitution -> Substitution
    comp' []   s'     = []
    comp' (s':ss') r' = (comp'' s' r'):(comp' ss' r')
      where
        comp'' :: (Identifier, Type) -> Substitution -> (Identifier, Type)
        comp'' (id, t) r'' = (id, subst t r'')
--Nota: unionBy le da preferencia a la lista de la izquierda (s).


-- Función que elimina sustituciones redundantes (T0 := T0) en una sustitución.
simpl :: Substitution -> Substitution
simpl []     = []
simpl (s:ss) = case s of
                 (id1, T id2)
                   | id1 == id2 -> simpl ss
                   | otherwise  -> s:(simpl ss)
                 _ -> s:(simpl ss) 
