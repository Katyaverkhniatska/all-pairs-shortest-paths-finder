module ReadInput where

import Types
import FloydWarshall
import System.IO
import System.Directory (doesFileExist)
import Text.Read (readMaybe)
import Data.Graph (vertices)
import Data.List (groupBy)
import Data.Text (splitOn, groupBy)
import Data.Maybe (fromMaybe, mapMaybe)

-- Returns a pair of a graph and a list of its vertices
formGraph :: [String] -> [[String]] -> (Graph Char, [Vertex Char])
formGraph verticesStr edgesStr = (g, vertices)
    where
        vertices = map read verticesStr
        edges = map parseEdges edgesStr
        g = buildGraph vertices edges

        -- Iterates over the entire string of edges, returning the list
        -- of them in the correct format.
        parseEdges :: [String] -> [NeighbourGraph Char]
        parseEdges = mapMaybe parseEdge

        -- Parses a string of format "'X' Y" into a tuple ('X', Just Y).
        -- If there is no edges returns Nothing.
        parseEdge :: String -> Maybe (NeighbourGraph Char)
        parseEdge edge =
            case words edge of
                [] -> Nothing       -- no edges
                [to, weight] ->     -- valid edge
                    case (readMaybe to, readMaybe weight) of
                        (Just toV, Just w) -> Just (toV, Just w)
                        _ -> error ("Invalid format in edge: " ++ edge)
                _ -> error ("Invalid format of edge string: " ++ edge)

        buildGraph :: [Vertex Char] -> [[NeighbourGraph Char]] -> Graph Char
        buildGraph = zip

-- Helper function that splits a string into a list of substrings
-- originally separated by delimiter delim
splitWithDelimiter :: Char -> String -> [String]
splitWithDelimiter _ [] = [""]
splitWithDelimiter delim (c:cs)
    | c == delim = "" : rest
    | otherwise  = (c : head rest) : tail rest
        where rest = splitWithDelimiter delim cs

-- Reads the inout file, processes lines to get raw strings
-- for vertices and edges, then returns a pair of a graph and a vertices list
readFromInputFile :: String -> IO (Graph Char, [Vertex Char])
readFromInputFile fileName = do
    exists <- doesFileExist fileName
    if not exists
        then error ("Non-existing file " ++ fileName)
        else do
            contents <- readFile fileName
            let lines' = lines contents
            let vertices = map (takeWhile (/= ':')) lines'
            let edges = map (splitWithDelimiter ';' . tail . dropWhile (/= ':')) lines'
            return (formGraph vertices edges)

-- Helper function to read all lines from stdin
readAllLines :: IO [String]
readAllLines = do
  eof <- isEOF
  if eof then return []
  else do
    line <- getLine
    rest <- readAllLines
    return (line : rest)

-- Function that reads inout line from console, processes lines
-- to get raw strings, for vertices and edges, then returns a pair of
-- a graph and a vertices list
readFromConsole :: IO (Graph Char, [Vertex Char])
readFromConsole = do
    lines <- readAllLines
    let vertices = map (takeWhile (/= ':')) lines
    let edges = map (splitWithDelimiter ';' . tail . dropWhile (/= ':')) lines
    return (formGraph vertices edges)