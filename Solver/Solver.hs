module Solver where

--  z podanej listy elementow usuwa wszystkie powtorzenia 
removeDups [] = []
removeDups [x] = [x]
removeDups (x:y:ys)   | elem x (y:ys) = removeDups (y:ys)
                       | otherwise = x : removeDups (y:ys)


-- z podanej listy list elementow typu (0,0,'a') zwraca litere przypisana do wspolrzednych podawanych jako parametr
getCharFromCoords1 :: Int -> Int -> Int -> [[(Int,Int,Char)]] -> [Char]
getCharFromCoords1 _ _ _ [] = []
getCharFromCoords1 outer inner len xs  | ((outer <0) || (inner <0) || (inner >= len) || (outer >= len) )  = "."    --jesli indeks poza plansza to zwraca '.'
getCharFromCoords1 outer inner len (x:xs) = ([c | (a,b,c)<- x,(a==outer && b == inner )]) ++ getCharFromCoords1 outer inner len xs

-- funkcja wykorzystywana w celu wstawienia znaku '.' gdy funkcja getCharFromCoords1 nie zwroci nic
getCharFromCoords :: Int -> Int -> Int -> [[(Int,Int,Char)]] -> Char
getCharFromCoords _ _ _ [] = '.'
getCharFromCoords outer inner len xs | length (getCharFromCoords1 outer inner len xs ) == 0 = '.'    --jesli getCharFromCoords1 nic nie zwrocil, to tutaj zwraca '.'
                      | otherwise = head (getCharFromCoords1 outer inner len xs )


-- funkcja zaleznie czy jestesmy w krotszym (nieparzystym) czy dluzszym (parzystym) rzedzie zwraca 
-- gornych sasiadow podanego elementu
getUpperNeighbors :: Int -> Int -> Int -> [[(Int,Int,Char)]] -> [Char]
getUpperNeighbors _ _ _ [] = []
getUpperNeighbors a b len xs | (((a-1) `mod` 2) ==1) = ((getCharFromCoords (a-1) b len xs) : (getCharFromCoords (a-1) (b+1) len xs) : [])
                          | otherwise = ((getCharFromCoords (a-1) (b-1) len xs) : (getCharFromCoords (a-1) b len xs) : [])

-- funkcja zaleznie czy jestesmy w krotszym (nieparzystym) czy dluzszym (parzystym) rzedzie zwraca 
-- dolnych sasiadow podanego elementu
getLowerNeighbors :: Int -> Int -> Int -> [[(Int,Int,Char)]] -> [Char]
getLowerNeighbors _ _ _[] = []
getLowerNeighbors a b len xs | (((a+1) `mod` 2) ==1) = ((getCharFromCoords (a+1) b len xs) : (getCharFromCoords (a+1) (b+1) len xs) : [])
                            | otherwise = ((getCharFromCoords (a+1) (b-1) len xs) : (getCharFromCoords (a+1) b len xs) : [])

-- dla podanego elementu na planszy zwraca wszystkich jego najblizszych sasiadow
-- czyli: dwoch bocznych, gorych i dolnych
getNeighbors :: Int -> Int -> Int -> [[(Int,Int,Char)]] -> [Char]
getNeighbors _ _ _[] = []
getNeighbors a b len xs = do 
                            let neighsLeftRight = (getCharFromCoords a (b-1) len xs) : (getCharFromCoords a (b+1) len xs) : []
                            let neighsUp = (getUpperNeighbors a b len xs)
                            let neighsLow = (getLowerNeighbors a b len xs)
                            neighsLeftRight ++ neighsUp ++ neighsLow


-- funkcja do zwracania wszystkich sasiadow dla naszych gornych sasiadow
getAllNeighborsForUpperNeigh :: Int -> Int -> Int -> [[(Int,Int,Char)]] -> [Char]
getAllNeighborsForUpperNeigh _ _ _ [] = []
getAllNeighborsForUpperNeigh a b len xs | (((a-1) `mod` 2) ==1) = ((getNeighbors (a-1) b len xs) ++ (getNeighbors (a-1) (b+1) len xs))
                             | otherwise = ((getNeighbors (a-1) (b-1) len xs) ++ (getNeighbors (a-1) b len xs) )


-- funkcja do zwracania wszystkich sasiadow dla naszych dolnych sasiadow
getAllNeighborsForLoweNeigh :: Int -> Int -> Int -> [[(Int,Int,Char)]] -> [Char]
getAllNeighborsForLoweNeigh _ _ _[] = []
getAllNeighborsForLoweNeigh a b len xs | (((a+1) `mod` 2) ==1) = ((getNeighbors (a+1) b len xs) ++ (getNeighbors (a+1) (b+1) len xs) )
                               | otherwise = ((getNeighbors (a+1) (b-1) len xs) ++ (getNeighbors (a+1) b len xs) )



-- funkcja do znajdowania sasiadow dla zadanego punku a nastepnie wszystkich sasiadow
-- dla sasiadow zadanego punktu
getAllNeighbors :: Int -> Int -> Int -> [[(Int,Int,Char)]] -> [Char]
getAllNeighbors _ _ _[] = []
getAllNeighbors a b len xs = do 
                               let myNeighs = (getNeighbors a b len xs)
                               let neighsUpUp = (getAllNeighborsForUpperNeigh a b len xs)
                               let neighsLowLow = (getAllNeighborsForLoweNeigh a b len xs)
                               let neighsOfLeftRight = (getNeighbors a (b-1) len xs ) ++ (getNeighbors a (b+1) len xs )
                               myNeighs ++ neighsUpUp ++ neighsLowLow ++ neighsOfLeftRight


-- na podstawie naszych sasiadow oraz sasiadow sasiadow okresla jakich liter brakuje dla tych punktow i zwraca liste tych liter
findAllPossible :: Int -> Int -> Int -> [[(Int,Int,Char)]] -> [Char]
findAllPossible a b len xs = do
                                 let myValue = getCharFromCoords a b len xs 
                                 if (myValue /= '.') --bylem juz wpisany dobrze lub bylem w zadaniu
                                   then (myValue : []) --to jedynym kandydatem to wpisanie jestem ja sam
                                 else do 
                                   let allNeighs = getAllNeighbors a b len xs
                                   let allNeighsNoDups = removeDups allNeighs
                                   [x | x<- ['A'..'G'], (not (elem x allNeighsNoDups)) ]





-- w liscie list podmienia elementowi numer a jego element nr b
-- np: [[1,1,1],[2],[1,2,3,4]] 4 (0,0) - > [[4,1,1],[2],[1,2,3,4]]
updateList :: [[a]] -> a -> (Int, Int) -> [[a]]
updateList list elem (a,b) =
  take a list ++
  [take b (list !! a) ++ [elem] ++ drop (b + 1) (list !! a)] ++
  drop (a + 1) list


-- do elementu typu (0,0,'a')d wstawia okresloną litere w miejscu okreslonym przez wspolrzedne przekazywane jako argument funkcji
updateHC :: [[(Int, Int, Char)]] -> Char -> (Int, Int) -> [[(Int, Int, Char)]]
updateHC list letter (a,b) = (updateList list (a,b,letter) (a,b))


-- dla podanej listy elementow sprawdza gdzie jest sytuacja, ze dla wybranego punktu jest tylko jedna mozliwa litera do wstawienia
-- i nastepnie szuka kolejnego wolnego punktu i tak az do napotkania konca list (planszy)
setCertain :: Int -> [[(Int,Int,Char)]] -> (Int,Int) -> [[(Int,Int,Char)]]
setCertain len [] (_,_) = [] 
setCertain len xs (a,b) = do
                        if((a>(len-1)) && (b>(len-2)))
                           then xs
                        else do
                          let possibles = (findAllPossible a b len xs)
                          let possiblesLen = (length possibles)
                          if (possiblesLen ==1)
                             then do let newList = updateHC xs (possibles !! 0) (a,b)
                                     if (a == (len-1) && b==(len-2))
                                       then newList
                                     else  setCertain len newList (nextElement (a,b) len)
      
                          else do
                                     if (a == (len-1) && b==(len-2))
                                       then xs
                                     else  
                                        setCertain len xs (nextElement (a,b) len)


-- wykonuje funkcje setCertain az miedzy kolejnymi iteracjami nie zostana wykonane zadne zmiany
setAllCertain :: Int -> [[(Int,Int,Char)]] -> Int -> Int -> [[(Int,Int,Char)]]
setAllCertain len [] _ _ = [] 
setAllCertain len xs a b = do
                            let new = setCertain len xs (a,b)
                            if(xs == new)
                              then xs
                            else setAllCertain len new a b


-- dla podanego elementu zwraca jego kolejnego sasiada, jezeli jest to ostatni element rzedu to
-- zwraca pierwszy element kolejnego rzedu
-- gdy jest to ostatni element to ponownie zwraca ten element
nextElement :: (Int, Int) -> Int -> (Int, Int)
nextElement (a,b) len = do 
                        if (a == (len-1) && b==(len-2))
                            then (a,b)
                        else do
                          if (a `mod` 2 ==0 && ((b+1) == (len-1)) ) 
                              then  ((a+1),0)
                          else if (a `mod` 2 == 0  ) 
                                   then  (a,(b+1))
                          else if (a `mod` 2 ==1 && ((b+1) == (len)) ) 
                                   then ((a+1),0)
                          else
                                 (a,(b+1))


-- zwraca wspolrzednego najblizszego elementu nie wypelnionego
-- dla ostatniego elementu planszy zwraca ponownie ten sam element
nextEmpty :: (Int,Int) -> [[(Int,Int,Char)]] ->(Int,Int)
nextEmpty (a,b) list | (a==(length list)-1) && (b==(length list)-2) = (a,b)
                      |  (getCharFromCoords x y (length list) list ) == '.' = (x,y)
                      |  otherwise = nextEmpty (nextElement (a,b) (length list)) list
                         where x = fst (nextElement (a,b) (length list))
                               y = snd (nextElement (a,b) (length list))



-- glowna funkcja wykonujaca algorytm rozwiazywania polegajaca na wstawianiu kolejnych dozwolonych liter 
-- i sprawdzaniu czy takie rozwiazanie moze byc sluszne
solve :: (Int,Int) -> [[(Int,Int,Char)]] -> [Char] -> [[(Int,Int,Char)]]
--solve (a,b) list []     | (a==(length list)-1) && (b==(length list)-2) = [] --ostatni element i nie ma mozliwosci do wpisania
solve (a,b) list (x:[]) | (a==(length list)-1) && (b==(length list)-2) = updateHC list x (a,b)  --ostatni element i jest tylko 1 mozliwa letter do wpisania
solve (_,_) list [] = []
solve (a,b) list (x:xs) | solvedNext == [] = solve (a,b) list xs
                         | otherwise = solvedNext
            where solveNext (a,b) list = do let certain = setAllCertain (length list) list a b 
                                            solve (nextEmpty (a,b) list) list (findAllPossible c d (length list) list) --solve dla kolejnego pustego miejsca
                  solvedNext = solveNext (a,b) (updateHC list x (a,b)) --solveNext ale gdy do listy wstawimy pierwszy z mozliwych do pustego
                  c = fst (nextEmpty (a,b) list)
                  d = snd (nextEmpty (a,b) list)

-- funkcja wywolywana z glownego programu majaca za zadanie przekazac sterowanie do kolejnych funkcji pomocniczych
solveHC list = do     let certain = setAllCertain (length list) list 0 0
                      solve (0,0) certain (findAllPossible 0 0 (length certain) certain)
