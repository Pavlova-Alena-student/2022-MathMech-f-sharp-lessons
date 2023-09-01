namespace MathMechFSharpLessons.Tests

open Expecto
open MathMechFSharpLessons

module Task5Tests =
    open Task5
    open LinAlg
    open System.Collections.Generic // Queue

    let naiveBFS (graph: bool list list) vertex =
        let n = graph.Length
        let mutable q: Queue<int * int> = new Queue<int * int>()
        q.Enqueue(vertex, 0)
        let mutable was: bool list = FSharp.Collections.List.replicate n false
        let mutable ans: int option list = FSharp.Collections.List.replicate n None

        while q.Count > 0 do
            let currentVertex, depth = q.Dequeue()

            if was.[currentVertex] = false then
                was <- FSharp.Collections.List.updateAt currentVertex true was
                ans <- FSharp.Collections.List.updateAt currentVertex (Some(depth)) ans

                for i in 0 .. n - 1 do
                    if graph.[currentVertex].[i] = true
                       && was.[i] = false then
                        q.Enqueue(i, depth + 1)

        ans

    // 1-2; 1-4; 4-5
    let smallGraphList: bool list list =
        [ [ 0; 1; 0; 1; 0 ]
          [ 1; 0; 0; 0; 0 ]
          [ 0; 0; 0; 0; 0 ]
          [ 1; 0; 0; 0; 1 ]
          [ 0; 0; 0; 1; 0 ] ]
        |> List.map (List.map (fun el -> if el = 0 then false else true))

    let smallGraph: Graph = { adjacency = Matrix(smallGraphList) }

    let midGraphList: bool list list =
        [ [ 0; 1; 1; 0; 0; 0; 0; 0; 0; 0; 0 ]
          [ 1; 0; 0; 1; 0; 0; 0; 0; 0; 1; 0 ]
          [ 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
          [ 0; 1; 0; 0; 1; 0; 0; 0; 0; 0; 1 ]
          [ 0; 0; 0; 1; 0; 1; 0; 0; 0; 0; 0 ]
          [ 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 1 ]
          [ 0; 0; 0; 0; 0; 0; 0; 1; 1; 0; 0 ]
          [ 0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 0 ]
          [ 0; 0; 0; 0; 0; 0; 1; 1; 0; 1; 0 ]
          [ 0; 1; 0; 0; 0; 0; 0; 0; 1; 0; 0 ]
          [ 0; 0; 0; 1; 0; 1; 0; 0; 0; 0; 0 ] ]
        |> List.map (List.map (fun el -> if el = 0 then false else true))

    let midGraph: Graph = { adjacency = Matrix(midGraphList) }

    [<Tests>]
    let tests =
        testList
            "BFS-related tests"
            [ testList "Tests for BFS-related functions" [] // There are none :/
              testList
                  "BFS for shortest path problem"
                  [ testCase "Example of solving shortest path problem"
                    <| fun _ ->
                        let subject = (BFS smallGraph 0).list ()
                        let naive = (naiveBFS smallGraphList 0)

                        let result =
                            [ Some(0)
                              Some(1)
                              None
                              Some(1)
                              Some(2) ]

                        Expect.equal subject result "Failed to find shortest paths in small graph"
                        Expect.equal naive result "Failed to find shortest paths in small graph (naive method)"
                    testCase "Example of solving shortest path problem 2"
                    <| fun _ ->
                        let subject = (BFS midGraph 0).list ()
                        let naive = (naiveBFS midGraphList 0)

                        let result =
                            [ Some(0)
                              Some(1)
                              Some(1)
                              Some(2)
                              Some(3)
                              Some(4)
                              Some(4)
                              Some(4)
                              Some(3)
                              Some(2)
                              Some(3) ]

                        Expect.equal subject result "Failed to find shortest paths in medium graph"
                        Expect.equal naive result "Failed to find shortest paths in medium graph (naive method)"
                    testProperty "Solving shortest path problem for random bool graph"
                    <| fun (edges: list<int * int>) ->
                        let size = List.fold (fun acc (i, j) -> max acc <| max i j) 0 edges

                        let graph: Graph =
                            { adjacency =
                                Matrix(edges |> List.map (fun (i, j) -> (i, j, true)), (size + 1, size + 1), false) }

                        let subject = (BFS graph 0).list ()
                        let result = (naiveBFS (graph.adjacency.list ()) 0)
                        Expect.equal subject result "Failed to find shortest paths (prop)" ] ]
