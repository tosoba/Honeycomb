module Types where

--typ wykorzystywany do do wyswietlania i konwersji 
data Plaster = Plaster [String] deriving (Show, Read)

-- z typu plaster zwraca jedynie liste stringow
list :: Plaster -> [String]
list (Plaster list) = list
