namespace MathMechFSharpLessons.Tests

open Expecto
open MathMechFSharpLessons

module LinAlgTests =
    open LinAlg

    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "_getMin2Pow test"
              <| fun _ ->
                  let a0 = _getMin2Pow 0
                  let a1 = _getMin2Pow 1
                  let a2 = _getMin2Pow 2
                  let a3 = _getMin2Pow 3
                  let a4 = _getMin2Pow 4
                  let a5 = _getMin2Pow 5
                  Expect.equal a0 0 "least power of two >= 0 should be 0"
                  Expect.equal a1 1 "least power of two >= 1 should be 1"
                  Expect.equal a2 2 "least power of two >= 2 should be 2"
                  Expect.equal a3 4 "least power of two >= 3 should be 4"
                  Expect.equal a4 4 "least power of two >= 4 should be 4"
                  Expect.equal a5 8 "least power of two >= 5 should be 8"
              testCase "Small vectror test"
              <| fun _ ->
                  let a: Vector<int> = Vector([ 1; 2; 3; 4; 5; 6 ])

                  Expect.equal a a "Check vector" // TODO: actual tests
              testCase "Small square matrix transpose test"
              <| fun _ ->
                  let a: Matrix<int> = Matrix([ [ 1; 2 ]; [ 3; 4 ] ])
                  let b: Matrix<int> = Matrix([ [ 1; 3 ]; [ 2; 4 ] ])
                  let subject = a.transpose

                  Expect.equal subject.list b.list "Operation transpose does not work on a small square matrix"
              testCase "Small matrix transpose test"
              <| fun _ ->
                  let a: Matrix<int> = Matrix([ [ 1; 2; 3 ]; [ 4; 5; 6 ] ])
                  let b: Matrix<int> = Matrix([ [ 1; 4 ]; [ 2; 5 ]; [ 3; 6 ] ])
                  let subject = a.transpose
                  Expect.equal subject.list b.list "Operation transpose does not work on a small matrix (2*3 -> 3*2)"
                  let subject = b.transpose
                  Expect.equal subject.list a.list "Operation transpose does not work on a small matrix (3*2 -> 2*3)"
              testCase "--Depth-- of a small matrix test"
              <| fun _ ->
                  let subject1: Matrix<int> =
                      Matrix(
                          [ [ 1; 2; 3 ]
                            [ 4; 5; 6 ]
                            [ 7; 8; 9 ] ]
                      )

                  let subject2: Matrix<int> = Matrix([ [ 1; 2; 3 ]; [ 4; 5; 6 ] ])

                  let subject3: Matrix<int> =
                      Matrix(
                          [ [ 1; 2; 3 ]
                            [ 4; 5; 6 ]
                            [ 7; 8; 9 ]
                            [ 10; 11; 12 ] ]
                      )

                  Expect.equal 1 1 "hehe - filler"
              testCase "--Depth-- of a small sparse matrix test"
              <| fun _ ->
                  let subject: Matrix<int> =
                      Matrix(
                          [ [ 1; 1; 1 ]
                            [ 1; 1; 1 ]
                            [ 1; 1; 1 ] ]
                      )

                  let subject: Matrix<int> = Matrix([ [ 1; 1; 1 ]; [ 1; 1; 1 ] ])

                  let subject: Matrix<int> =
                      Matrix(
                          [ [ 1; 1; 1 ]
                            [ 1; 1; 1 ]
                            [ 1; 1; 1 ]
                            [ 1; 1; 1 ] ]
                      )

                  Expect.equal 1 1 "hehe - filler" ]
