module Main where

import ReadInput
import WriteOutput
import FloydWarshall
import System.Environment (getArgs)
import Data.Graph (vertices)
import Distribution.Simple.Build (writeAutogenFiles)

-- Function checks if the correct number and type of arguments is given.
checkArgs :: IO [String]
checkArgs = do
    args <- getArgs
    case length args of
        0 -> error "Missing input specification"
        1 -> if head args == "-f" then error "Missing input file name" else return args
        2 -> if head args == "-f" then error "Missing output specification" 
                else if (args !! 1) == "-f" then error "Missing output file name"
                    else return args
        3 -> if (args !! 2) == "-f" then error "Missing output file name"
                else if head args == "-c" || (args !! 1) == "-c" then error "File name specified with console input/output format"
                    else return args
        4 -> if head args == "-c" || (args !! 2) == "-c" then error "File name specified with console input/output format"
                else return args
        _ -> error "Too many arguments"

-- Main function of the program.
-- Reads the arguments and based on them reads and writes output.
main :: IO()
main = do
    args <- checkArgs
    (graph, vertices) <- case head args of
            "-f" -> readFromInputFile (args !! 1)
            "-c" -> readFromConsole
            _ -> error "Incorrect input format flag"
    let (finalWeightsMatrix, finalPathsMatrix) = solveFloydWarshall graph vertices
    case length args of
        2 -> do
            writeToConsole graph finalWeightsMatrix finalPathsMatrix vertices
        3 -> do
            -- args = [-f, filename, -c]
            if (args !! 2) == "-c" then do
                writeToConsole graph finalWeightsMatrix finalPathsMatrix vertices
            else do -- othw args = [-c, -f, filename]
                writeToFile (last args) graph finalWeightsMatrix finalPathsMatrix vertices
        4 -> do
            writeToFile (last args) graph finalWeightsMatrix finalPathsMatrix vertices