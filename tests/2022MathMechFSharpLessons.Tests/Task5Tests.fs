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
                  "BFS for shortest path problem"
                  [ testCase "Example of solving shortest path problem"
                    <| fun _ ->
                        let subject = (BFS smallGraph 0).list ()

                        let result =
                            [ Some(0)
                              Some(1)
                              None
                              Some(1)
                              Some(2) ]

                        Expect.equal subject result "Failed to find shortest pathes"
                    testCase "Example of solving shortest path problem 2"
                    <| fun _ ->
                        let subject = (BFS midGraph 0).list ()

                        let result =
                            [ Some(0)
                              Some(1)
                              Some(1)
                              Some(2)
                              Some(3)
                              Some(4)
                              Some(6)
                              Some(6)
                              Some(5)
                              Some(4)
                              Some(3) ]

                        Expect.equal subject result "Failed to find shortest pathes 2" ] ]
