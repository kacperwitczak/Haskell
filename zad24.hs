main = do
    print (largestAmicablePair 300)
    print (largestAmicablePair 1500)
    print (largestAmicablePair 3000)


----------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------- zad 24
sumDivisors :: Int -> Int
sumDivisors n = sum [i | i <- [1..(n `div` 2)], n `mod` i == 0]
--sumowanie wszystkich dzielnikow liczby zadanej (w liscie)

sumDivisorsList :: Int -> [Int]
sumDivisorsList n = [sumDivisors x | x <- [1..n]]
--tworzenie listy sum przy uzyciu powyzszej funkcji


largestAmicablePair :: Int -> (Int, Int)
largestAmicablePair n =
    let reversedPairedSums = reverse ( zip [1..n] (sumDivisorsList n)) 
        --stworzenie listy par liczb z ich sumami dzielnikow i odwrocenie ich
        pairs = [(a, b) | (a, divSumA) <- reversedPairedSums, (b, divSumB) <- reversedPairedSums, a /= b, a == divSumB, b == divSumA]
        --stworzenie wszystkich mozliwych par liczb zaprzyjaznionych
    in case pairs of
        [] -> (0, 0)
        _ -> head pairs --zwroc pierwszy element listy

{-Dla danej liczby całkowitej n znaleźć największą parę liczb zaprzyjaźnionych mniejszych od n.-}