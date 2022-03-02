salario valorBase = (valorBase - descontos) + acrecimos
  where 
    descontos = valorBase * (7 / 100)
    acrecimos = valorBase * (10 / 100)

conceito nota_1 nota_2 nota_3 = 
    if mediaPonderada >= 8.00 && mediaPonderada <= 10.00
      then 'A'
    else if mediaPonderada >= 7.00 && mediaPonderada < 8.00 
      then 'B' 
    else if mediaPonderada >= 6.00 && mediaPonderada < 7.00
      then 'C'
    else if mediaPonderada >= 5.00 && mediaPonderada < 6.00
      then 'D'
    else
      'E'
    where 
      mediaPonderada = (nota_1 * 2 + nota_2 * 3 + nota_3 * 5) / (2 + 3 +5)

precoRetrato :: Integer -> String -> Double
precoRetrato pessoas diaSemana = 
    if pessoas == 1
      then 100 * fatorAdicional
    else if pessoas == 2
      then 130 * fatorAdicional
    else if pessoas == 3
      then 150 * fatorAdicional
    else if pessoas == 4
      then 165 * fatorAdicional
    else if pessoas == 5
      then 175 * fatorAdicional
    else if pessoas == 6
      then 180 * fatorAdicional
    else if pessoas >= 7
      then 185 * fatorAdicional
    else
      0
  where
    fatorAdicional = (\dia -> if dia == "sabado" || dia == "domingo" then 1 + (20 / 100) else 1) diaSemana

produtoNumero :: Double -> Double -> Double -> Double 
produtoNumero num1 num2 num3 = num1  * num2 * num3

pesoIdeal :: Double -> Char -> Double
pesoIdeal h 'f' = 62.1 * h - 44.7
pesoIdeal h 'm' = 72.7 * h - 58

situacao notaUm notaDois notaTres = 
    if media >= 7
      then ("aprovado", media)
    else if media >= 3.00 && media < 7.00 
      then ("exame especial", media)
    else 
      ("reprovado", media)
    where 
      media = (notaUm + notaDois + notaTres) / 3

palindromo :: [Char] -> Bool
--palindromo palavra = if palavra == (reverse'' palavra) then True else False
palindromo palavra = if palavra == (reversePalavra) then True else False
  where 
    reversePalavra = (foldl (\acc x -> x : acc) []) palavra

-- Reverter lista
reverse'' :: [Char] -> [Char]
reverse'' [] = []
reverse'' (h:t) = reverse'' t ++ [h]

verificaEmprestimo :: Double -> Double -> Bool
verificaEmprestimo sal prest = if prest <= max then True else False
  where
    max = sal * 0.30

classeEleitoral :: Integer -> [Char] 
classeEleitoral n
    | n < 16 = "não eleitor"
    | n >= 18 && n < 65 = "eleitor obrigatório"
    | n >= 65 = "eleitor facultativo"
    | n >= 16 && n < 18 = "eleitor facultativo"

fat x 
  | x == 1 = x
  | x > 1 = x * fat (x - 1)

fatD x 
  | x == 0 = 1
  | x >= 1 = x * fatD (x - 2)

expoente x n
  | n == 0 = 1
  | n >= 1 = x * expoente x (n - 1)

reverseList :: [Int] -> [Int]
reverseList [] = []
reverseList (h:t) = reverseList t ++ [h]

lastList :: [Int] -> Int
lastList (h:t) = h

ultimo l = lastList (reverseList l)

firsts :: [Int] -> [Int]
firsts [] = []
firsts (h:t) = t

primeiros :: [Int] -> [Int]
primeiros lista = reverseList (firsts (reverseList lista))

somaLista :: [Int] -> Int
somaLista lista = foldl (+) 0 lista

somaLista2 :: [Int] -> [Int] -> [Int]
somaLista2 l1 l2 = zipWith (+) l1 l2

data TesteAux = TesteAux Integer String
  deriving Show

data Produto = 
  Perecivel Integer String Integer Bool TesteAux
  | NaoPecivel Integer String String Integer TesteAux
  deriving Show

descricaoProduto :: Produto -> String
descricaoProduto produto = case produto of 
  Perecivel cod desc ano comestivel (TesteAux n _) -> desc
  NaoPecivel cod desc marca ano     (TesteAux n _) -> desc

descricaoProduto' :: Produto -> Integer
descricaoProduto' (Perecivel cod _ _ _ (TesteAux n _)) = cod + n
descricaoProduto' (NaoPecivel cod _ _ _ (TesteAux n _)) = cod + n

--21
and' :: Bool -> Bool -> Bool
and' _ False = False
and' p1 True = p1

--22
lt'' :: [Integer] -> Integer
lt'' [] = 0
lt'' (k:(j:t)) = j + k
lt'' (h:_) = h

cl' :: [Integer] -> Integer
cl' l = foldl (\x l -> x + (l ^ 0)  ) 0 l

ft' l = filter (\(a, b) -> if a == "joao" then True else False) l

fil'' ("a",b) = True
fil'' (a, b) = if b == "d" then True else False 
