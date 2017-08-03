module Ui where

import Data.List

-- dla kazdego napisu wstawia miedzy litery spacje i ewentualnie spacje na poczatku dla krotszych napisow
honeycombRowToString :: String -> Bool -> String
honeycombRowToString row padRow = 
    (if padRow then " " else "") ++ intersperse ' ' row

-- wykonuje funkcje honeycombRowToString dla kazdego napisu z listy
honeycombToStringImpl :: [String] -> Bool -> String
honeycombToStringImpl [] _ = ""
honeycombToStringImpl (head:rest) padRow =
    (honeycombRowToString head padRow) ++ "\n" ++ (honeycombToStringImpl rest (not padRow))

-- wyswietla String (cala zagadka)
showHoneycomb :: [String] -> IO ()
showHoneycomb h = do
    putStrLn ""
    putStr (honeycombToStringImpl h True)
    putStrLn ""