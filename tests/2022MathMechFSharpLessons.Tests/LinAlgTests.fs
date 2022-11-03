namespace MathMechFSharpLessons.Tests

open Expecto
open MathMechFSharpLessons

module LinAlgTests =
    open LinAlg

    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "Transpose test"
              <| fun _ ->
                  let a: Matrix<int> = Matrix([ [ 1; 2; 3 ]; [ 4; 5; 6 ] ])
                  let b: Matrix<int> = Matrix([ [ 1; 4 ]; [ 2; 5 ]; [ 3; 6 ] ])
                  let subject = transpose a
                  Expect.equal subject b "mistakes were made" ]
