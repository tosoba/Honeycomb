import System.IO
import System.IO.Error
import Control.Exception
import Convertions
import Types
import Solver
import Ui
import Verification


--funkcja ktora pobiera od uzytkownika nazwe pliku i przekazuje do rozwiazania zagadke
main :: IO()
main = do
  putStrLn "Podaj nazwe pliku lub wybierz <w> aby wyjsc:"
  line <- getLine
  case line of
    ['w'] -> return ()
    fileName -> do loadAndSolve fileName
                   main


-- funkcja odpowiedzialna za caly algorytm programu, poczatkowo sprawdza poprawnosc 
-- podanego pliku, nastepnie przekazuje zagadke do rozwiazania i ja wyswitla
loadAndSolve :: String -> IO ()
loadAndSolve fileName =
  catch (do  handle   <- openFile fileName ReadMode
             contents <- hGetContents handle
             let converted = read contents :: (Plaster)
             val <- try (print converted) :: IO (Either SomeException ())
             case val of
               Left _   -> putStrLn "Zly format pliku"
               Right () -> do let listString = list converted
                              if(checkShortRowLen listString (length listString) == False)
                                 then putStrLn "Zle uzupelnione wiersze w zagadce, nie w formacie n-1,n,n-1,n..."
                              else do
                              let listCoords = getCoordsList listString 0
                              let result = solveHC listCoords
                              let resultToShow = coordsToStringList result 
                              putStrLn "Zadanie do rozwiazania:"
                              showHoneycomb listString
                              putStrLn "Rozwiazana zagadka:"
                              showHoneycomb resultToShow
                              
             hClose handle
        )  errorHandler
  where
    errorHandler e =
      if (fileError e)
        then putStrLn ("Nie mozna otworzyc pliku " ++ fileName)
          else return ()

--bleady jakie moga byc napotkane podczas otwierania pliku
fileError :: IOError -> Bool
fileError e = isDoesNotExistError e || isAlreadyInUseError e || isPermissionError e || isEOFError e