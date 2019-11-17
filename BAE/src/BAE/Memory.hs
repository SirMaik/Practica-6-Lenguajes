module BAE.Memory where

import BAE.Sintax

--Para ayudar con ciertas complejidades
import Data.Set (Set, member, notMember, insert, empty, findMax, findMin, size)

-- Definimos los tipos
type Address = Int

type Value = Expr

type Cell = (Address, Value)

type Memory = [Cell]


-- Dada una memoria, genera una nueva dirección de memoria que no esté
-- contenida en esta
-- Nota: La memoria estará corrompida si hay direcciones repetidas o alguna dirección es negativa (esta última condición no está especificada en la práctica pero es lógica. 
newAddress :: Memory -> Expr 
newAddress [] = L 0
newAddress m  = let addresses = obtainAddrs m
                    (corrupted, addrSet) = isCorrupted2 addresses empty
                in if corrupted 
                   then error "Corrupted memory."
                   else findMinAvailable addrSet 
  where
    obtainAddrs  :: Memory -> [Address]
    obtainAddrs =  foldr (\(a,b) as -> a:as) []
    isCorrupted2 :: [Address] -> (Set Address) ->  (Bool, Set Address)
    isCorrupted2 [] s     = (findMin s < 0, s)
    isCorrupted2 (a:as) s
      | member a s = (True, s)
      | otherwise  = isCorrupted2 as (insert a s)
    findMinAvailable :: (Set Address) ->  Expr
    findMinAvailable s 
      | notMember 0 s      = L 0
      | max == size s - 1  = L (max + 1) 
      | otherwise          = L (findHole 1 s)
      where
        max = findMax s
        findHole :: Address -> (Set Address) -> Address
        findHole a s
          | member a s = findHole (a + 1) s
          | otherwise  = a


--Función que dice si una memoria está corrompida o no
-- Nota: La memoria estará corrompida si hay direcciones repetidas o alguna dirección es negativa (esta última condición no está especificada en la práctica pero es lógica.
isCorrupted :: Memory -> Bool
isCorrupted [] = False
isCorrupted l  = isCorrupted' l empty
  where
    isCorrupted' :: Memory -> (Set Address) -> Bool
    isCorrupted' []     s = findMin s < 0
    isCorrupted' ((a,_):ms) s
      | member a s = True
      | otherwise  = isCorrupted' ms (insert a s)


-- Dada una dirección de memoria, devuelve el valor contenido en la celda con
-- tal dirección, en caso de no encontrarla debe devolver Nothing
access :: Address -> Memory -> Maybe Value
access a m
  | isCorrupted m = error "Corrupted memory."
  | otherwise     = lookup a m 



-- Dada una celda de memoria, actualiza el valor de esta misma en la memoria.
-- En caso de no existir debe devolver Nothing
update :: Cell -> Memory -> Maybe Memory
update c@(a,v) m
  | isCorrupted m = error "Corrupted memory."
  | otherwise     = case v of
                      (V _)    -> update' c m
                      (I _)    -> update' c m
                      (B _)    -> update' c m
                      (L _)    -> update' c m
                      (Fn _ _) -> update' c m
                      _        -> error "Memory can only store values."
    where
      update' :: Cell -> Memory -> Maybe Memory
      update' c m = let u' = update'' c m
                    in case u' of
                         (False, _ ) -> Nothing
                         (True , m') -> Just m'
      update'' :: Cell -> Memory -> (Bool, Memory)
      update'' _ [] = (False, [])
      update''  c@(ac, vc) (cm@(am,vm):ms)
        | ac == am  = (True, c:ms)
        | otherwise = let (b', m') = update'' c ms
                      in  (b', cm:m')
                      


 
