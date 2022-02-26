-- $ ghci main.hs 

-- :r (atualiza ghci)

-- Recursão
{-
fatorial :: Integer -> Integer
fatorial n = if n == 0
                then 1
                else fatorial (n - 1) * n
-}
-- OU
fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial n = fatorial (n - 1) * n

-- Potência de 2
potDois :: Integer -> Integer
potDois n 
    | n == 0 = 1
    | n > 0 = 2 * potDois(n - 1)

-- Multiplicação
mult :: Integer -> Integer -> Integer 
mult n m
    | n == 0 = 0
    | n > 0 = m + mult (n -1) m

-- Comprimento lista

-- Concatenar lista
(++++) :: [Int] -> [Int] -> [Int]
[] ++++ concatList =  concatList
(h:t) ++++ concatList = h : (t ++++ concatList)

-- Reverter lista
reverse'' :: [Int] -> [Int]
reverse'' [] = []
reverse'' (h:t) = reverse'' t ++ [h]

-- Produto no intervalo (m, n)
prodIntervalo :: Integer -> Integer -> Integer 
prodIntervalo m n
    | m == n = m 
    | m < n = m * prodIntervalo (m + 1) n

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)