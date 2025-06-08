module WriteOutput where

import FloydWarshall
import Types
import System.Directory (doesFileExist)
import Data.Maybe (fromJust, isJust, fromMaybe)
import Data.Graph (path, vertices)
import qualified GHC.TypeLits as Graph

-- Creates a proper String represantation of Graph.
-- Takes an input Graph and returns a list of Strings that represent one line to print.
-- For each Vertex prints its name and a list of neighbours, followed by the Weight of edge to it.
prepareForOutputGraph :: (Eq a, Show a) => Graph a -> [String]
prepareForOutputGraph = map covertToStr
    where
        -- Gets a single Graph entry [(v, [n, w])] and returns a String
        -- of format "Vertex v, Neighbours & Weights: n w; ...".
        -- If v has no outcoming edges, prints "Neighbours & Weights: None"
        covertToStr :: (Eq a, Show a) => (Vertex a, [(Vertex a, Weight)]) -> String
        covertToStr (vertexName, neighboursList) = result
            where
                edges' = foldr
                    (\(n, w) acc -> "; " ++ show n ++ " " ++
                        show (fromJust w) ++ acc) "\n" neighboursList
                result = "Vertex " ++ show vertexName ++ ", Neighbours & Weights:" ++ 
                    if not (null neighboursList) then tail edges' else " None\n"

-- Creates a proper String represantation of MatrixDistances.
-- Takes an input matrix and returns a list of Strings that represent one line to print.
-- All matrix entries are in square brackets and separated by a comma.
prepareForOutputDistsMatrix :: MatrixDistances -> [String]
prepareForOutputDistsMatrix = map ((:) '[' . tail . convertDistRowToStr)
    where
        -- For every entry e of a matrix it:
        --  - If e = Just w, appends w.
        --  - If e = Nothing, it appends "Inf".
        convertDistRowToStr :: MatrixDistancesRow -> String
        convertDistRowToStr = foldr (\r acc -> ", " ++
                                if isJust r then show (fromJust r) ++ acc
                                    else show "Inf" ++ acc) " ]\n"
                                    
-- Creates a proper String represantation of MatrixPaths.
-- Takes an input matrix and returns a list of Strings that represent one line to print.
prepareForOutputPathsMatrix :: (Eq a, Show a) => MatrixPaths a -> [Vertex a] -> [String]
prepareForOutputPathsMatrix matrix verticies = [ convertPathRowToStr (verticies !! i) (matrix !! i) | i <- [0..length verticies - 1]]
    where
        convertPathRowToStr ::(Eq a, Show a) => Vertex a -> MatrixPathsRow a -> String
        -- For origin Vertex o and Path p to Vertex d it creates a String of a format:
        -- "o -> d: o -> p -> d"
        convertPathRowToStr origin pathsRow = concat [ do
            let formPath = foldr (\p acc ->  " -> " ++ show p ++ acc) "\n"
            let path = pathsRow !! i
            show origin ++ " to " ++ show (verticies !! i) ++ ":" ++ if null path then " no path\n" 
                else drop 3 $ formPath path | i <- [0..length verticies - 1]]

writeToFile :: (Eq a, Show a) => String -> Graph a -> MatrixDistances -> MatrixPaths a -> [Vertex a] -> IO ()
writeToFile fileName g ds ps vs = do
    writeFile fileName "Input Graph\n"
    appendFile fileName (concat $ prepareForOutputGraph g)
    appendFile fileName "\nMatrix of Weights (|Vertices|x|Vertices|)\n"
    appendFile fileName (concat $ prepareForOutputDistsMatrix ds)
    appendFile fileName "\nShortest paths for all vertices\n"
    appendFile fileName (concat $ prepareForOutputPathsMatrix ps vs)

writeToConsole :: (Eq a, Show a) => Graph a -> MatrixDistances -> MatrixPaths a -> [Vertex a] -> IO ()
writeToConsole g ds ps vs = do
    putStrLn "Input Graph"
    mapM_ putStr (prepareForOutputGraph g)
    putStrLn "\nMatrix (|Vertices|x|Vertices|)"
    mapM_ (putStr . (:) '[' . tail) (prepareForOutputDistsMatrix ds)
    putStrLn "\nShortest paths for all vertices"
    mapM_ putStr (prepareForOutputPathsMatrix ps vs)