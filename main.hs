-- Questão 1:
data Veiculo = Passeio String String Int Int Data [[Char]]-- Nome Marca Potencia(CV) Portas Passageiros Fabrição(Data) Opcionais Exe: (Passeio "Onix" "Chevrolet" 110 4 (Data 12 12 2022) ["Ar", "Direçao", "Vidro Eletrico"])
         | Trilha String String Int Int Int Int Data [[Char]]-- Nome Marca Potencia(CV) Tração(4 ou 2 rodas) Portas Passageiros Fabrição(Data) Opcionais Exe: (Trilha "Ranger" "Ford" 250 4 4 5 (Data 12 12 2022) ["Ar", "Direçao", "Vidro Eletrico"])
         | Carga String String Int Int Int Int Data [[Char]]-- Nome Marca Potencia(CV) Carga(Kg) Portas Passageiros Fabrição(Data) Opcionais - Exe: (Passeio "Onix" "Chevrolet" 110 4 (Data 12 12 2022) ["Ar", "Direçao", "Vidro Eletrico"])
           deriving Show

-- Questão 2:
marcaNomeVeiculo :: Veiculo -> String
marcaNomeVeiculo (Passeio marca nome _ _ _ _) = marca ++ " " ++ nome
marcaNomeVeiculo (Trilha marca nome _ _ _ _ _ _) = marca ++ " " ++ nome
marcaNomeVeiculo (Carga marca nome _ _ _ _ _ _) = marca ++ " " ++ nome

potenciaVeiculo :: Veiculo -> Int 
potenciaVeiculo (Passeio _ _ potencia _ _ _) = potencia
potenciaVeiculo (Trilha _ _ potencia _ _ _ _ _) = potencia
potenciaVeiculo (Carga _ _ potencia _ _ _ _ _) = potencia

opcionaisVeiculo :: Veiculo -> [[Char]] 
opcionaisVeiculo (Passeio _ _ _ _ _ opcionais) = opcionais
opcionaisVeiculo (Trilha _ _ _ _ _ _ _ opcionais) = opcionais
opcionaisVeiculo (Carga _ _ _ _ _ _ _ opcionais) = opcionais

-- Questão 3:
veiculoPossuiArCondicionado :: Veiculo -> [Char] 
veiculoPossuiArCondicionado (Passeio _ _ _ _ _ opcionais) = encontraItem opcionais
veiculoPossuiArCondicionado (Trilha _ _ _ _ _ _ _ opcionais) = encontraItem opcionais
veiculoPossuiArCondicionado (Carga _ _ _ _ _ _ _ opcionais) = encontraItem opcionais

encontraItem :: [[Char]] -> [Char]
encontraItem [] = "Nao"
encontraItem (h:t) = 
    if h == "Ar" then "Sim" 
    else encontraItem t

-- Questão 4:
veiculoPossuiArCondicionado' :: Veiculo -> [Char] 
veiculoPossuiArCondicionado' (Passeio _ _ _ _ _ opcionais) = encontraItem' opcionais
veiculoPossuiArCondicionado' (Trilha _ _ _ _ _ _ _ opcionais) = encontraItem' opcionais
veiculoPossuiArCondicionado' (Carga _ _ _ _ _ _ _ opcionais) = encontraItem' opcionais

encontraItem' :: [[Char]] -> [Char]
encontraItem' opcoes = if not (null lista) then "Sim" else "Nao"
    where 
        lista = (filter (\ item -> item == "Ar") opcoes)

-- Questão 5:
data Data = Data Int Int Int -- Dia Mês Ano
    deriving Show

-- Questão 6:
anoData :: Data -> Int 
anoData (Data _ _ ano) = ano

-- Questão 7:
adicionaDia :: Data -> Data 
adicionaDia (Data dia mes ano) = 
    if diaMaisUm > 30 then
        if (mes + 1) > 12 then (Data 1 1 (ano + 1))
        else (Data 1 (mes + 1) ano)
    else
        (Data (dia+1) mes ano)
    where 
        diaMaisUm = dia + 1