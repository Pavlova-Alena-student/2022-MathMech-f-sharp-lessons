namespace MathMechFSharpLessons.Tests

open Expecto
open MathMechFSharpLessons

module LinAlgTests =
    open LinAlg

    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "Small vector tests"
              <| fun _ ->
                  let lst = [ 1; 2; 3; 4; 5; 6 ] // TODO: as property param
                  let a: Vector<int> = Vector(lst)
                  Expect.equal a.length 6 "Length of a vector is not the same as the original list"
                  Expect.equal (a.list ()) lst "Vector didn't convert to the original list"
              testCase "Small square matrix transpose test"
              <| fun _ ->
                  let a: Matrix<int> = Matrix([ [ 1; 2 ]; [ 3; 4 ] ])
                  let b: Matrix<int> = Matrix([ [ 1; 3 ]; [ 2; 4 ] ])
                  let subject = a.transpose ()

                  Expect.equal
                      (subject.list ())
                      (b.list ())
                      "Operation transpose does not work on a small square matrix"
              testCase "Small matrix transpose test"
              <| fun _ ->
                  let a: Matrix<int> = Matrix([ [ 1; 2; 3 ]; [ 4; 5; 6 ] ])
                  let b: Matrix<int> = Matrix([ [ 1; 4 ]; [ 2; 5 ]; [ 3; 6 ] ])
                  let subject = a.transpose ()

                  Expect.equal
                      (subject.list ())
                      (b.list ())
                      "Operation transpose does not work on a small matrix (2*3 -> 3*2)"

                  let subject = b.transpose ()

                  Expect.equal
                      (subject.list ())
                      (a.list ())
                      "Operation transpose does not work on a small matrix (3*2 -> 2*3)"
              testCase "Small matrix test"
              <| fun _ ->
                  let lst1 =
                      [ [ 1; 2; 3 ]
                        [ 4; 5; 6 ]
                        [ 7; 8; 9 ] ]

                  let subject1: Matrix<int> = Matrix(lst1)
                  Expect.equal subject1.height 3 "Length (height) of a matrix is not the same as the original list"
                  Expect.equal subject1.width 3 "Length (width) of a matrix is not the same as the original list"
                  Expect.equal (subject1.list ()) lst1 "Matrix didn't convert to the original list"

                  let lst2 = [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ]
                  let subject2: Matrix<int> = Matrix(lst2)
                  Expect.equal subject2.height 2 "Length (height) of a matrix is not the same as the original list"
                  Expect.equal subject2.width 3 "Length (width) of a matrix is not the same as the original list"
                  Expect.equal (subject2.list ()) lst2 "Matrix didn't convert to the original list"

                  let lst3 =
                      [ [ 1; 2; 3 ]
                        [ 4; 5; 6 ]
                        [ 7; 8; 9 ]
                        [ 10; 11; 12 ] ]

                  let subject3: Matrix<int> = Matrix(lst3)
                  Expect.equal subject3.height 4 "Length (height) of a matrix is not the same as the original list"
                  Expect.equal subject3.width 3 "Length (width) of a matrix is not the same as the original list"
                  Expect.equal (subject3.list ()) lst3 "Matrix didn't convert to the original list"
              testCase "Small sparse matrix test"
              <| fun _ ->
                  let lst1 =
                      [ [ 1; 1; 1 ]
                        [ 1; 1; 1 ]
                        [ 1; 1; 1 ] ]

                  let subject1: Matrix<int> = Matrix(lst1)
                  Expect.equal subject1.height 3 "Length (height) of a matrix is not the same as the original list"
                  Expect.equal subject1.width 3 "Length (width) of a matrix is not the same as the original list"
                  Expect.equal (subject1.list ()) lst1 "Matrix didn't convert to the original list"

                  let lst2 = [ [ 1; 1; 1 ]; [ 1; 1; 1 ] ]
                  let subject2: Matrix<int> = Matrix(lst2)
                  Expect.equal subject2.height 2 "Length (height) of a matrix is not the same as the original list"
                  Expect.equal subject2.width 3 "Length (width) of a matrix is not the same as the original list"
                  Expect.equal (subject2.list ()) lst2 "Matrix didn't convert to the original list"

                  let lst3 =
                      [ [ 1; 1; 1 ]
                        [ 1; 1; 1 ]
                        [ 1; 1; 1 ]
                        [ 1; 1; 1 ] ]

                  let subject3: Matrix<int> = Matrix(lst3)
                  Expect.equal subject3.height 4 "Length (height) of a matrix is not the same as the original list"
                  Expect.equal subject3.width 3 "Length (width) of a matrix is not the same as the original list"
                  Expect.equal (subject3.list ()) lst3 "Matrix didn't convert to the original list"
              testCase "Multiplication of matrixes to vectors"
              <| fun _ ->
                  let intMat1: Matrix<int> = Matrix([ [ 1; 2; 3 ]; [ 4; 5; 6 ] ])
                  let intVec1: Vector<int> = Vector([ 1; 2; 3 ])
                  let res1 = [ 14; 32 ]

                  Expect.equal
                      ((intMat1.mult (+) (*) intVec1).list ())
                      res1
                      "Standart multiplication of a matrix(int) to a vector(int) failed"

                  let res2 = [ "149"; "41018" ]

                  Expect.equal
                      ((intMat1.mult (+) (fun a b -> string (a * b)) intVec1)
                          .list ())
                      res2
                      "Multiplication of a matrix(int) to a vector(int) failed (+ as concat)"
              testCase "Multiplication of vecctors to matrixes"
              <| fun _ ->
                  let intVec1: Vector<int> = Vector([ 1; 2; 3 ])
                  let intMat1: Matrix<int> = Matrix([ [ 1; 2 ]; [ 4; 5 ]; [ 7; 8 ] ])
                  let res1 = [ 30; 36 ]

                  Expect.equal
                      ((intVec1.mult (+) (*) intMat1).list ())
                      res1
                      "Standart multiplication of a vector(int) to a matrix(int) failed"

                  let res2 = [ "1821"; "21024" ]

                  Expect.equal
                      ((intVec1.mult (+) (fun a b -> string (a * b)) intMat1)
                          .list ())
                      res2
                      "Multiplication of a vector(int) to a matrix(int) failed (+ as concat)" ]