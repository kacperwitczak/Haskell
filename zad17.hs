main = do
  print(generateSequence 14 2 2)
  print(generateSequence 13 2 2)
  print(generateSequence 12 2 2)
  print(generateSequence 11 2 2)
  print(generateSequence 10 2 2)
  print(generateSequence 10000 2 3)
  print(generateSequence 1000 2 3)


----------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------- zad 17
generateInteger :: [Int] -> Integer
generateInteger list = foldl (\x y -> x * 10 + fromIntegral y) 0 list
--zamiana listy intow na jedna wartosc integer [1,2] -> 12

checkOccurrences :: Int -> [Int] -> Bool
checkOccurrences p digits = 
  let exceeded = filter (\d -> length (filter (\x -> x == d) digits) > p) digits --podwojny for, filtruje powtorzenia powyzej p razy
  -- jezeli length>p dla jakiegos d to tablica bedzie niepusta
  in null exceeded --pusta = true     niepusta = false


generateSequence :: Int -> Int -> Int -> Integer
generateSequence n m p = generateInteger (head (generateQueue [[x] | x <- [0..m]] [[]])) --m = 2 [[0],[1],[2]]
  where
    generateQueue :: [[Int]] -> [[Int]] -> [[Int]] --definiowanie funkcji
    generateQueue [] result = result --jesli kolejka jest pusta to zwracamy wynik
    generateQueue (x:xs) result --result to tablica wynikowa, x-pierwszy element kolejki(liczba w postaci tablicy) xs-tablica reszty liczb
      | length(result) > n = take 1 result --jezeli w tablicy wynikowej mamy ponad n elementow to konczymy wykonywanie
      | head x == 0 = generateQueue xs ([0]:result) --przypadek szczegolny aby nie generowac 01, 010; dodanie 0 na 1. miejscu
      | checkOccurrences p x = generateQueue (xs ++ [x ++ [y] | y <- [0..m]]) (x:result)
      --jezeli x jest legalne to dodajemy na poczatek tablicy wynikowej
      --do kolejki dodajemy kolejne mozliwosci np. dla x = [[1]] do kolejki dodamy [[10], [11], [12]]
      | otherwise = generateQueue xs result
      --jezeli x jest nielegalne to jest pomijane i tablica wynikowa jest niezmieniona

{-Dla danych liczb całkowitych: n, m, p podać n-tą liczbę z ciągu rosnącego liczb od 0 do x gdzie x to
maksymalna liczba zawierająca, każdą cyfrę od 0 do m, p razy. Każda liczba z ciągu zawiera cyfry
od 0 do m maksymalnie p razy. Przykładowo dla m = 4 i p = 2, x będzie równe 4433221100.
Przykładowo dla n = 14, m = 2 i p = 2 wynikiem będzie 112.
(1) 0
(2) 1
(3) 2
(4) 10
(5) 11
(6) 12
(7) 20
(8) 21
(9) 22
(10) 100
(11) 101
(12) 102
(13) 110
(14) 112-}