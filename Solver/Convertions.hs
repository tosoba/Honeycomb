module Convertions where

-- funkcja odpowiedzialna za zamiane napisu na zbior znakow wraz ze wspolrzednymi 
-- np z:getSingleRowCoords ala 0 0-> [(0,0,'a'),(0,1,'l'),(0,2,'a')]
getSingleRowCoords :: String -> Int -> Int -> [(Int,Int,Char)]
getSingleRowCoords [] liczZew liczWew = []
getSingleRowCoords str liczZew liczWew = (liczZew,liczWew, (str !! 0)) : (getSingleRowCoords (tail str) liczZew (liczWew+1))
 
                          
-- z listy napisow kazdy kolejny napis przekazuje wraz z jego numerem z listy do funkcji getSingleRowCoords aby
-- zamienic na punkty opisujace wspolrzednego kazdego ze znakow
getCoordsList :: [String] -> Int -> [[(Int,Int,Char)]]
getCoordsList [] liczZew = []
getCoordsList (x:xs) liczZew =  (getSingleRowCoords x liczZew 0) : (getCoordsList xs (liczZew+1)) 

-- z listy w postaci zbioru wspolrzednych oraz przypisanych do nich znakow tworzy
-- liste napisow w celu ladnego wyswietlenia ostatecznego wyniku
coordsToStringList :: [[(a,a,b)]] -> [[b]]
coordsToStringList [] = []
coordsToStringList (c:cs) = (([x | (_,_,x) <- c]) : (coordsToStringList cs) )
