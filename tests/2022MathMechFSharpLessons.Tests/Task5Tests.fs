namespace MathMechFSharpLessons.Tests

open Expecto
open MathMechFSharpLessons

module Task5Tests =
    open Task5
    open LinAlg

    // 1-2; 1-4; 4-5
    let smallGraph: Graph =
        { adjacency =
            Matrix(
                [ [ 0; 1; 0; 1; 0 ]
                  [ 1; 0; 0; 0; 0 ]
                  [ 0; 0; 0; 0; 0 ]
                  [ 1; 0; 0; 0; 1 ]
                  [ 0; 0; 0; 1; 0 ] ]
            ) }

    let midGraph: Graph =
        { adjacency =
            Matrix(
                [ [ 0; 1; 1; 0; 0; 0; 0; 0; 0; 0; 0 ]
                  [ 1; 0; 0; 1; 0; 0; 0; 0; 0; 3; 0 ]
                  [ 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
                  [ 0; 1; 0; 0; 1; 0; 0; 0; 0; 0; 1 ]
                  [ 0; 0; 0; 1; 0; 1; 0; 0; 0; 0; 0 ]
                  [ 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 1 ]
                  [ 0; 0; 0; 0; 0; 0; 0; 1; 1; 0; 0 ]
                  [ 0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 0 ]
                  [ 0; 0; 0; 0; 0; 0; 1; 1; 0; 1; 0 ]
                  [ 0; 3; 0; 0; 0; 0; 0; 0; 1; 0; 0 ]
                  [ 0; 0; 0; 1; 0; 1; 0; 0; 0; 0; 0 ] ]
            ) }

    [<Tests>]
    let tests =
        testList
            "BFS-related tests"
            [ testList "Tests for BFS-related functions" []
              testList
                  "BFS for connected components problem"
                  [ testCase "Example of solving connected to one vertex component problem"
                    <| fun _ ->
                        let front: GraphVertexes =
                            BinProperties(Vector([ true; false; false; false; false ]))

                        let subject =
                            match BFS front smallGraph with
                            | BinProperties (prop) -> prop.list ()
                            | _ -> []

                        let result = [ true; true; false; true; true ]
                        Expect.equal subject result "Failed to find a connected component"
                    testCase "Example of solving connected components problem"
                    <| fun _ ->
                        let front: GraphVertexes = IntProperties(Vector([ 1; 2; 3; 4; 5 ]))

                        let subject =
                            match BFS front smallGraph with
                            | IntProperties (prop) -> prop.list ()
                            | _ -> []

                        let result = [ 1; 1; 3; 1; 1 ]
                        Expect.equal subject result "Failed to find connected components" ]
              testList "BFS for shortest path problem" []
              testList "BFS for shortest circle problem" [] ]
