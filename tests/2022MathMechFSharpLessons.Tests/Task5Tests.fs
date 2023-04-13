namespace MathMechFSharpLessons.Tests

open Expecto
open MathMechFSharpLessons

module Task5Tests =
    open Task5
    open LinAlg

    [<Tests>]
    let tests =
        testList
            "BFS-related tests"
            [ testList "Tests for BFS-related functions" []
              testList
                  "BFS for connected components problem"
                  [ testCase "Example of solving connected components problem"
                    <| fun _ ->
                        // 1-2; 1-4; 4-5
                        let g: Graph =
                            { adjacency =
                                Matrix(
                                    [ [ 0; 1; 0; 1; 0 ]
                                      [ 1; 0; 0; 0; 0 ]
                                      [ 0; 0; 0; 0; 0 ]
                                      [ 1; 0; 0; 0; 1 ]
                                      [ 0; 0; 0; 1; 0 ] ]
                                ) }

                        let front: GraphVertexes = { properties = Vector([ 1; 0; 0; 0; 0 ]) }
                        let subject = BFS front g
                        let result = [ 1; 1; 0; 1; 1 ]
                        Expect.equal (subject.properties.list ()) result "Failed to find connected component" ]
              testList "BFS for shortest path problem" []
              testList "BFS for shortest circle problem" [] ]
