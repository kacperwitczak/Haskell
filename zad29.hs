
main = do
    print $ pruferCode [(1, 2)] -- []
    print $ pruferCode [(1, 2), (1, 3)] -- [1]
    print $ pruferCode [(1, 2), (2, 3)] -- [2]
    print $ pruferCode [(1, 3), (2, 3)] -- [3]
    print $ pruferCode [(1, 4), (2, 4), (3, 4)] -- [4, 4]
    print $ pruferCode [(1,4),(2,4),(3,4),(4,5),(5,6)] -- [4,4,4,5]
    print $ pruferCode [(5,6),(4,5),(3,4),(2,4),(1,4)] -- [4,4,4,5]
    print $ pruferCode [(2,7),(6,7),(4,7),(5,7),(3,5),(1,5),(1,8)] --[7,5,7,7,5,1]

----------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------- zad 29
type Edge = (Int, Int)
type Tree = [Edge] --tworzenie struktur

pruferCode :: Tree -> [Int]
pruferCode treeMain = pruferCode_helper (length treeMain + 1) treeMain
  where
    pruferCode_helper :: Int -> Tree -> [Int]
    pruferCode_helper verticesCount tree
      | null tree = [] --base case
      | length tree == 1 = [] --base case
      | otherwise = neighbor : pruferCode_helper verticesCount newTree
      where
        leaves = [vert | vert <- [1..verticesCount], isLeaf vert tree] -- znajdz wszystkie liscie w aktualnym drzewie
        minLeaf = minimum leaves --znajdz lisc o najmniejszym id
        neighbor = findNeighbor minLeaf tree --znalezienie sasiada usuwanego liscia
        newTree = filter (\(a, b) -> (a, b) /= (minLeaf, neighbor) && (a, b) /= (neighbor, minLeaf)) tree
        --budowa nowego drzewa bez krawedzi laczacej usuwany lisc



    isLeaf :: Int -> Tree -> Bool
    isLeaf vert tree = length (filter (\(a, b) -> a == vert || b == vert) tree) == 1
    --sprawdzenie czy wierzcholek vert jest lisciem

    findNeighbor :: Int -> Tree -> Int
    findNeighbor leaf tree =
      let [(a, b)] = filter (\(x, y) -> x == leaf || y == leaf) tree
      in if a == leaf then b else a
    --znajdujemy sasiedni wierzcholek liscia

{- Kod Prüfer’a5 pozwala przekształcić dowolne drzewo na unikalną sekwencję liczb.
Zaimplementować funkcję kodującą drzewo podane w postaci ciągu krawędzi (lista par liczb)
kodem Prüfera (lista liczb):
 prufer_code [(1, 2)]
> []
 prufer_code [(1, 2), (1, 3)]
> [1]
 prufer_code [(1, 2), (2, 3)]
> [2]
 prufer_code [(1, 3), (2, 3)]
> [3]
 prufer_code [(1, 4), (2, 4), (3, 4)]
> [4, 4] -}
