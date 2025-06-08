module Types where

import Control.Monad (join)

-- All the tyoes representing a Graph
type Vertex a = a
-- Just value represets a real weight, 
-- Nothing - no edge / infinite weight etc
type Weight = Maybe Integer
type Path a = [Vertex a]
type NeighbourGraph a = (Vertex a, Weight)
type Graph a = [(Vertex a, [NeighbourGraph a])]

-- Matrix of weights
type MatrixDistancesRow = [Weight]
type MatrixDistances = [MatrixDistancesRow]

-- Matrix of paths
-- matrix[i][j] = path i->j = [verticies]
type MatrixPathsRow a = [Path a]
type MatrixPaths a = [MatrixPathsRow a]

-- Helper function to add two Weights
addWeights :: Weight -> Weight -> Weight
addWeights (Just x) (Just y) = Just (x + y)
addWeights _ _ = Nothing

-- Helper function to compare two Weights as (<)
lessThan :: Weight -> Weight -> Bool
lessThan Nothing _ = False
lessThan _ Nothing = True
lessThan (Just x) (Just y) = x < y

-- Helper function to compare two Weights as (==)
-- and also checks if both values are 'finite'
finiteAndEqual :: Weight -> Weight -> Bool
finiteAndEqual (Just x) (Just y) = x == y
finiteAndEqual _ _ = False

-- Helper getter function for the matrices
get :: [[a]] -> Int -> Int -> a
get matrix i j = (matrix !! i) !! j

-- Given a Graph and list of its Vertices returns a matrix of Weights (MatrixDistances)
makeDistMatrix :: (Eq a) => Graph a -> [Vertex a] -> MatrixDistances
makeDistMatrix g vertices = map (\(v, ns) -> makeRowFromMap v ns vertices) g
    where
        -- Given a Vertex and its Neighbours returns a correctly formed row of the MatrixDistances
        makeRowFromMap :: (Eq a) => Vertex a -> [NeighbourGraph a] -> [Vertex a] -> MatrixDistancesRow
        makeRowFromMap v neighbours =
            -- The diagonal entries i.e. Weight of edge from v to itself is set to 0
            map (\x -> if x == v then Just 0 else join (lookup x neighbours))   -- join to 'flatter' Maybe

-- Given a Graph and list of its Vertices returns a matrix of Paths (MatrixPaths)
makePathsMatrix :: (Eq a) => Graph a -> [Vertex a] -> MatrixPaths a
makePathsMatrix g vertices = map (\(v, list) -> makePathRow v list vertices) g
    where
        -- Given a Vertex and its Neighbours returns a correctly formed row of the MatrixPathsRow
        makePathRow :: Eq a => a -> [NeighbourGraph a] -> [Vertex a] -> MatrixPathsRow a
        makePathRow v neighbours =
            -- From v to itself sets a path of a single Vertex v.
            -- If a valid edge from v to w exists sets a path [v, w].
            -- Else sets an empty path.
            map (\x -> if x == v then [v] else
                        case lookup x neighbours of
                            Just _ -> [v, x]
                            _ -> [])