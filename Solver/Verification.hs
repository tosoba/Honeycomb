module Verification where

--sprawdza poprawnosc wczytywanego pliku,
-- wymusza aby kolejne rzedy byly formatu n-1, n, n-1 itd
-- jezeli jest poprawny to zwraca tru w innej sytuacji False
-- obie funkcje uruchamiane sa na zmiane w celu sprawdzania roznej dlugosci wierszy
checkShortRowLen :: [String] -> Int -> Bool
checkShortRowLen [] _ = True
checkShortRowLen (x:xs) len =      if (length x == (len -1))   
                                 then  
                                     checkLongRowLen xs len
                               else False
                     
                                                   
checkLongRowLen :: [String] -> Int -> Bool
checkLongRowLen [] _ = True
checkLongRowLen (x:xs) len =        if (length x == len)
                                 then
                                     checkShortRowLen xs len
                               else False