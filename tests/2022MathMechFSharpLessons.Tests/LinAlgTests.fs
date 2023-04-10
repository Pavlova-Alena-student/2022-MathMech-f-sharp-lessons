namespace MathMechFSharpLessons.Tests

open Expecto
open FsCheck
open MathMechFSharpLessons

module LinAlgTests =
    open LinAlg

    [<Tests>]
    let tests =
        testList
            "Linear Algebra tests"
            [ testList
                  "Vector tests"
                  [ testCase "Small vector convertion tests"
                    <| fun _ ->
                        let lst = [ 1; 2; 3; 4; 5; 6 ]
                        let a: Vector<int> = Vector(lst)
                        Expect.equal a.length 6 "Length of a vector is not the same as the original list"
                        Expect.equal (a.list ()) lst "Vector didn't convert to the original list"
                    testProperty "Vector convertion tests (prop)"
                    <| fun lst ->
                        let a: Vector<int> = Vector(lst)
                        Expect.equal a.length (lst.Length) "Length of a vector is not the same as the original list"
                        Expect.equal (a.list ()) lst "Vector didn't convert to the original list" ]
              testList
                  "Matrix tests"
                  [ testCase "Small matrix test"
                    <| fun _ ->
                        let lst = [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ]
                        let subject: Matrix<int> = Matrix(lst)

                        Expect.equal subject.height 2 "Length (height) of a matrix is not the same as the original list"

                        Expect.equal subject.width 3 "Length (width) of a matrix is not the same as the original list"
                        Expect.equal (subject.list ()) lst "Matrix didn't convert to the original list"
                    testCase "Small sparse matrix test"
                    <| fun _ ->
                        let lst =
                            [ [ 1; 1; 1 ]
                              [ 1; 1; 1 ]
                              [ 1; 1; 1 ]
                              [ 1; 1; 1 ] ]

                        let subject: Matrix<int> = Matrix(lst)

                        Expect.equal subject.height 4 "Length (height) of a matrix is not the same as the original list"

                        Expect.equal subject.width 3 "Length (width) of a matrix is not the same as the original list"
                        Expect.equal (subject.list ()) lst "Matrix didn't convert to the original list"
                    testCase "Multiplication of vectors to matrixes"
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
                            "Multiplication of a vector(int) to a matrix(int) failed (+ as concat)"
                    testProperty "Multiplication of vectors to zero matrix (prop)"
                    <| fun (intVec: Vector<int>) ->
                        let n = intVec.length

                        if n <> 0 then // skip if vector has 0 length
                            let ZArray = List.replicate n <| List.replicate n 0
                            let ZMat: Matrix<int> = Matrix(ZArray)

                            Expect.allEqual
                                ((intVec.mult (+) (*) ZMat).list ())
                                0
                                "Standart multiplication of a vector(int) to zero matrix failed"
                    testProperty "Multiplication of vectors to Id matrix (prop)"
                    <| fun (intVec: Vector<int>) ->
                        let n = intVec.length

                        if n <> 0 then // skip if vector has 0 length
                            let EArray =
                                (List.replicate n <| List.replicate n 0)
                                |> List.mapi (fun i arr_i -> List.updateAt i 1 arr_i)

                            let EMat: Matrix<int> = Matrix(EArray)

                            Expect.equal
                                (intVec.mult (+) (*) EMat)
                                intVec
                                "Standart multiplication of a vector(int) to Id matrix failed"
                    testProperty "Multiplication of vectors to matrixes (prop)"
                    <| fun (lst: int list) ->
                        let intVec: Vector<int> = Vector<int>(lst)

                        let lstLst: int list list =
                            MatrixGenerator.matrixGen<int> (lst.Length) ((Arb.generate<int>.Sample (1, 1)).[0])

                        let intMat: Matrix<int> = Matrix<int>(lstLst)

                        let naiveMult: int list =
                            List.map
                                (fun col -> List.fold2 (fun acc lst_i col_i -> acc + (lst_i * col_i)) 0 lst col)
                                (List.transpose lstLst)

                        Expect.equal
                            ((intVec.mult (+) (*) intMat).list ())
                            naiveMult
                            "Standart multiplication of a vector to matrix should be equal to naive implementation" ] ]
