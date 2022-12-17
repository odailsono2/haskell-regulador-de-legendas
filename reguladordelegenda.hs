{-# LANGUAGE OverloadedStrings #-}
import System.IO
import System.Environment

--para compilar inclua na mesma pasta a legenda original e o este arquivo
--digite o comando no terminal:
--runhaskell .\provafinal.hs shift Stretch "Arquivo_Original" "Arquivo_Destino.srt"
--a leganda será deslocado em shift segundo e esticada em Stretch de fator

{--trech para testes de conversão
main =  do
    --args <- getargs
    --arq <- openfile (args!!2) readmode
    arq <- openFile "Legenda.srt" ReadMode
    contents <- hGetContents arq
    let
        --d = read((args!!0))::Float
        --f = read((args!!1))::Float
        funcao =  regulador 1440 1 contents
    --putStr $ funcao!!(read(args!!0)::Int) --(lines (contents) )!!10
    --writeFile (args!!3) funcao
    writeFile "Interestelar3.srt" funcao
    hClose arq
--}

main =  do
    args <- getArgs
    arq <- openFile (args!!2) ReadMode
    --arq <- openFile "Legenda1.srt" ReadMode
    contents <- hGetContents arq
    let
        d = read((args!!0))::Float
        f = read((args!!1))::Float
        funcao =  regulador d f contents
    --putStr $ funcao!!(read(args!!0)::Int) --(lines (contents) )!!10
    writeFile (args!!3) funcao
    hClose arq

-----------formato Legenda
--1
--00:00:00,500 --> 00:00:01,000
--Legenda: Luiz Otávio Miranda
--
--2
--00:00:01,430 --> 00:00:03,874
--Meu nome é Liv Moore e eu meio que morri
--
--_____________________________________________

--verifica tamanho da strings
len::[a]->Int
len []     = 0
len (x:xs) = 1 + len xs

-- verifica se a lista de strings é a linha do tempo da legenda

regulador:: Float -> Float -> [Char] -> [Char]
regulador d f xss = unlines $ (time_line d f (lines xss))

time_line :: Float -> Float -> [[Char]] -> [[Char]]
time_line _ _ [a]      = [a]
time_line d f (xs:xss) = if separaHMS xs then [regula d f (ws!!0) ++ " --> " ++ regula d f (ws!!1)] ++ (time_line d f xss)
                                     else [xs] ++ (time_line d f xss)
                                         where
                                             ws = time_st_end xs
                                             --f  = f1
                                             --d  = d1 -- 1800

time_st_end xs = take 12 xs : drop 17 xs : []

regula d f xs = convToHMS $ shiftTime d $ stretchTime f $ convHM2SEC xs

--regula desloc fator xs = map (faux desloc fator) xs

--verifica se a linha é do tempo
    {--
separaHMS xs = if (any (==':') xs) then
                              if (any (==':') y) then True
                                 else False
                            else False
                                where
                                    y = dropWhile (/=':') xs

--                            xs!!2 == ':' && xs!!5==':' = True
--  | otherwise                  = False
--}
separaHMS (_:_:':':_:_:':':_:_:',':_:_:_:xs)   = True
separaHMS [ ]                    = False
separaHMS _                      = False


--
shiftTime :: Float->Float->Float
shiftTime t d = t + d

stretchTime :: Float->Float->Float
stretchTime t d = t*d

convToHMS :: Float -> [Char]
convToHMS x  = (showHora horas) ++ ":" ++ (showMin minutos) ++":"++ (segToChar segundos) ++"," ++ (incluiZero milesimos)
    where
        horas = convS2H x
        minutos = convS2M (x - fromIntegral (horas*3600) )  {--::Int--}
        segundos =  ( (sepInt x) `mod` 3600) `mod` 60
        milesimos = fSegToChar x


sepInt :: Float->Int
sepInt x = read(partInt x)::Int

sepDec :: Float->Float
sepDec x = read( "0."++(partDec x) )::Float

--converte fomato 00:00, serve para horas e minutos
showHora :: (Show a , Ord a, Num a) => a -> [Char]
showHora x = if x < 10
                then "0" ++ show (x)
                else show (x)

showMin :: (Show a , Ord a, Num a) => a -> [Char]
showMin = showHora

--retorna segundos em formato Char "00,000"
segToChar :: (Show a, Ord a, Num a)=>a->[Char]
segToChar x = if x < 10 then "0" ++ seg
                        else seg
                            where
                                seg    = partInt x


--dotseg = incluiZero $ partDec x --incluiZero $ partDec x

fSegToChar :: Float -> Int
fSegToChar x = read(y)::Int
    where y = takeWhile (/='.') $ show(1000*(snd $ properFraction x))

partInt :: (Show a,Num a)=>a->[Char]
partInt x = takeWhile (/='.') $ show(x)


partDec :: (Show a,Num a)=>a->[Char]
partDec x = if any (=='.') num
               then tail $ dropWhile (/='.') $ num
               else "000"
                   where
                       num = show (x)

--inlcui zeros no digitos decimais do segundo para manter o formato 0,000
incluiZero :: (Num a, Ord a, Show a)=>a->[Char]
incluiZero x = if (len $ show(x) )  < 3 then
                                        if x == 0 then "000"
                                                  else
                                                  if x < 10 then "00" ++ show x
                                                            else "0" ++ show x
                                        else show x


--calcula a parte inteira das horas de um total de segundos
--ex.: 8800.33 segundos -> 2 horas
convS2H :: Float -> Int
convS2H x =  y `div` 3600
    where y = sepInt x

convS2M :: Float -> Int
convS2M x = y `div` 60
    where y = sepInt x

--tempo total em seg x 1000 para abranger 3 casas decimais após a vigula
convHM2SEC :: [Char]->Float
convHM2SEC xs = fromIntegral(x)/1000
    where x = (fracSeg xs) + 1000*(( (toHora xs)*3600 + (toMinut xs)*60 + (toSegInt xs) ))

--  | otherwise = error "HM2SEC: Não está no formato HH:MM:SS,SSS"

--retorna separadamente horas em formato float
toHora :: [Char]->Int
toHora xs = read(takeWhile (/=':') xs)::Int
--toHora xs = read(take 2 xs)::Int
--retorna minutos em formato float
toMinut :: [Char]->Int
toMinut xs = read(takeWhile (/=':') $ drop 1 $ dropWhile (/=':') (xs) )::Int
--toMinut xs = read(drop 3 $ take 5 xs)::Int

--retorna segundos em formato float
toSegInt :: [Char]->Int
toSegInt xs = read(takeWhile (/=',') $ drop 1 $ dropWhile (/=':') $ drop 1 $ dropWhile (/=':') (xs))::Int
--toSegInt xs = read(takeWhile (/=',') $ tail $ dropWhile (/=':') $ tail $ dropWhile (/=':') (xs) )::Int
--toSegInt xs = read((take 2 $ drop 6 xs ))::Int

fracSeg :: [Char]->Int
fracSeg xs = read(takeWhile (/=' ') $ drop 1 $ dropWhile  (/=',') $ dropWhile (/=':') $ drop 1 $ dropWhile (/=':') (xs) )::Int
--fracSeg xs = read (takeWhile (/=' ') $ tail $ dropWhile (/=',') $ dropWhile (/=':') ( tail (dropWhile (/=':') (xs) )))::Int
--fracSeg xs =  read(drop 9 xs)::Int

-- str2int "10" = 10 ---converte em inteiro
str2int :: [Char]->Float
str2int x
  | isNumber x = read(x) :: Float
  | otherwise  = error "NaN"

--isDigit "1" = True, isDigit 'a' =False
isDigit :: Char->Bool
isDigit x
    |x >= '0' && x <= '9' = True
    |otherwise = False


--isNumber "100090293"= True, isNumber "swsws" = False
isNumber :: [Char]->Bool
isNumber []     = True
isNumber (x:xs) = and $ (isDigit x):[isNumber xs]

    {--
-- quebra_char "olá\nvc"= ["olá","vc"]
quebra_char :: String->[String]
quebra_char frases = lines frases

-- ant_split 4 "alfazema" = "alfa"
ant_split :: Int->[Char]->[Char]
ant_split d xs = take d xs

-- dep_split 2 "reprovacao" = "provacao"
dep_split :: Int->[Char]->[Char]
dep_split d xs = drop d xs

-- puttextAT 3 " nao" "Eu tenho dinheiro" = "Eu nao tenho dinheiro"
puttextAT :: Int->[Char]->[Char]->[Char]
puttextAT d ys xs = (ant_split d xs) ++ ys ++ (dep_split d xs)

--}
