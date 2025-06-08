module FloydWarshall where

import Types

-- Returns True if sees a negative cycle in a Graph, otherwise false
hasNegativeCycle :: MatrixDistances -> Bool
-- Negative cycle <=> for some vertex the diagonal entry will be negative
hasNegativeCycle mat = any (`lessThan` Just 0) [ get mat i i | i <- [0..length mat - 1] ]

-- Given Weights and Paths matrices, the # of Vertices, indecies of a source Vertex and
-- an intermediate Vertex, returns the updated rows for both matrices.
processMatrixRow :: MatrixDistances -> MatrixPaths a -> Int -> Int -> Int -> ([Weight], [Path a])
processMatrixRow distMat pathsMat n src intermIdx = (newDists, newPaths)
    where
        pathsForSrc = pathsMat !! src
        intermPath = pathsMat !! intermIdx
        -- Loop over the rows and update each entry.
        newDists = [determineEntryWeight dest | dest <- [0..n - 1]]
        newPaths = [determineEntryPath dest | dest <- [0..n - 1]]

        -- The following two functions update MatrixDistances and MatrixPaths using the same logic.
        -- Compare the distances (Weights) from the souce to destination directly (matrix[src][dest])
        -- and through the indermediate Vertex (matrix[src][interm] + matrix[interm][dest]). If the
        -- path though the intermediate vertex is shorter - either has lower weight or the same but smaller
        -- number of vertices in the path (this is used only in determineEntryPath), then update matrix.

        determineEntryWeight dest =
            let intermDist = get distMat src intermIdx
                distToDest = get distMat intermIdx dest
                combined = addWeights intermDist distToDest
                old = get distMat src dest
            in if combined `lessThan` old
                    then combined
                    else old

        determineEntryPath dest =
            let intermDist = get distMat src intermIdx
                distToDest = get distMat intermIdx dest
                combined = addWeights intermDist distToDest
                old = get distMat src dest
                combinedPath = (pathsForSrc !! intermIdx) ++ tail (intermPath !! dest)
                oldPath = (pathsForSrc !! dest)
            in ( if (combined `lessThan` old) || (combined `finiteAndEqual` old && length combinedPath < length oldPath) 
                then combinedPath else oldPath )

-- Given Weights and Paths matrices, list of all Vertices and the index of intermediate Vertex,
-- update all the rows in both matrices. If the degative cycle is detected, throws an error.
updateMatrices :: MatrixDistances -> MatrixPaths a -> [Vertex a] -> Int -> (MatrixDistances, MatrixPaths a)
updateMatrices distMat pathsMat verticesOrd intermediateIdx = (distMat', pathsMat')
    where
        n = length distMat
        intermediate = verticesOrd !! intermediateIdx
        distAndPathRowPairs = [processMatrixRow distMat pathsMat n i intermediateIdx | i <- [0..n-1]]
        (distMatNew, pathsMat') = unzip distAndPathRowPairs
        distMat' = if hasNegativeCycle distMatNew
                    then error "Negative cycle detected."
                        else distMatNew

-- Main entry point for the Floyd Warshal algorithm.
-- Runs in O(|Vertices|^3).
-- Iterates though the matrices taking every Vertex v as intermediate one-by-one.
-- Each time for every pair of Vertices it is determined if there is a shorter path through v.
-- If so, Weight and Paths Matrices are updated.
solveFloydWarshall :: (Eq a) => Graph a -> [Vertex a] -> (MatrixDistances, MatrixPaths a)
solveFloydWarshall g verticesOrd = iterateOverMatrix updateMatrices (dist, paths) verticesOrd 0
    where
        dist = makeDistMatrix g verticesOrd
        paths = makePathsMatrix g verticesOrd
        iterateOverMatrix f (distMat, pathsMat) elems k
            | k == length distMat   = (distMat, pathsMat)
            | otherwise             = iterateOverMatrix f (f distMat pathsMat elems k) elems (k+1)