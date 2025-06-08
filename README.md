# All-pairs shortest paths finder

This is a Haskell implementation of the **Floyd–Warshall algorithm** for computing the shortest paths between all pairs of vertices in a weighted directed graph. 

Supports:
- Negative weights (with cycle detection)
- Disconnected and partially connected graphs
- Reading input graph from file or console
- Writing output: graph, weights matrix and shortest paths, to file or console

---

## 📦 Features

- Implements all-pairs shortest paths via Floyd–Warshall
- Detects and reports negative cycles
- Parses custom-formatted graph input
- Supports nodes without outgoing edges
- Constructs and prints shortest paths between all pairs
- Constructs a matrix of weights of all shortest paths

---

## 🛠 Build Instructions

To compile manually using GHC:

```bash
ghc -isrc src/Main.hs -o main
```
---

## 💻 Running Intructions

To run the program using GHC (both input and ouput formats must be specified):
a) To read input from console:
```bash
./main -c ...
```
b) To read input from file:
```bash
./main -f <yourInputFilePath> ...
```
c) To write input to console:
```bash
./main ... -c
```
d) To write input to file:
```bash
./main ... -f <yourInputFilePath>
```

## 📄 Input file format

The input is expected to have each line in the exact same format:
```t
'VertexName1': 'VertexName2' WeightValue1;'VertexName3' WeightValue2; ...
```
Or, if the vertex has no outgoing edges:
```t
'VertexNameNoEdges':
```

No extra white spaces is expected.

## 📑 Output file format

You should expect your output to look like this:
For input:
```t
'a': 'b' 1
'b': 'c' 2
'c': 'a' 3
'd':
```
```t
Input Graph
Vertex 'a', Neighbours & Weights: 'b' 1
Vertex 'b', Neighbours & Weights: 'c' 2
Vertex 'c', Neighbours & Weights: 'a' 3
Vertex 'd', Neighbours & Weights: None

Matrix (|Vertices|x|Vertices|)
[ 0, 1, 3, "Inf" ]
[ 5, 0, 2, "Inf" ]
[ 3, 4, 0, "Inf" ]
[ "Inf", "Inf", "Inf", 0 ]

Shortest paths for all vertices
'a' to 'a': 'a'
'a' to 'b': 'a' -> 'b'
'a' to 'c': 'a' -> 'b' -> 'c'
'a' to 'd': no path
'b' to 'a': 'b' -> 'c' -> 'a'
'b' to 'b': 'b'
'b' to 'c': 'b' -> 'c'
'b' to 'd': no path
'c' to 'a': 'c' -> 'a'
'c' to 'b': 'c' -> 'a' -> 'b'
'c' to 'c': 'c'
'c' to 'd': no path
'd' to 'a': no path
'd' to 'b': no path
'd' to 'c': no path
'd' to 'd': 'd'
```

In all these cases the program will throw an **error**:
-- negative cycle is detected
-- the input has incorrect format
-- incorrent fags or their number
-- the inout file does not exist

## 📁 Input

By default it is expected to store input/output files in the ```resources``` folder, but of course it is not obligatory. There is prepared inputs to test different types of input, including a graph with negative cycle.
