namespace MathMechFSharpLessons.Tests

open Expecto
open MathMechFSharpLessons

module LinAlgTests =
    open LinAlg
    //open Printf
    //open System.IO

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
                        let lst1 =
                            [ [ 1; 2; 3 ]
                              [ 4; 5; 6 ]
                              [ 7; 8; 9 ] ]

                        let subject1: Matrix<int> = Matrix(lst1)

                        Expect.equal
                            subject1.height
                            3
                            "Length (height) of a matrix is not the same as the original list"

                        Expect.equal subject1.width 3 "Length (width) of a matrix is not the same as the original list"
                        Expect.equal (subject1.list ()) lst1 "Matrix didn't convert to the original list"

                        let lst2 = [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ]
                        let subject2: Matrix<int> = Matrix(lst2)

                        Expect.equal
                            subject2.height
                            2
                            "Length (height) of a matrix is not the same as the original list"

                        Expect.equal subject2.width 3 "Length (width) of a matrix is not the same as the original list"
                        Expect.equal (subject2.list ()) lst2 "Matrix didn't convert to the original list"

                        let lst3 =
                            [ [ 1; 2; 3 ]
                              [ 4; 5; 6 ]
                              [ 7; 8; 9 ]
                              [ 10; 11; 12 ] ]

                        let subject3: Matrix<int> = Matrix(lst3)

                        Expect.equal
                            subject3.height
                            4
                            "Length (height) of a matrix is not the same as the original list"

                        Expect.equal subject3.width 3 "Length (width) of a matrix is not the same as the original list"
                        Expect.equal (subject3.list ()) lst3 "Matrix didn't convert to the original list"
                    testCase "Small sparse matrix test"
                    <| fun _ ->
                        let lst1 =
                            [ [ 1; 1; 1 ]
                              [ 1; 1; 1 ]
                              [ 1; 1; 1 ] ]

                        let subject1: Matrix<int> = Matrix(lst1)

                        Expect.equal
                            subject1.height
                            3
                            "Length (height) of a matrix is not the same as the original list"

                        Expect.equal subject1.width 3 "Length (width) of a matrix is not the same as the original list"
                        Expect.equal (subject1.list ()) lst1 "Matrix didn't convert to the original list"

                        let lst2 = [ [ 1; 1; 1 ]; [ 1; 1; 1 ] ]
                        let subject2: Matrix<int> = Matrix(lst2)

                        Expect.equal
                            subject2.height
                            2
                            "Length (height) of a matrix is not the same as the original list"

                        Expect.equal subject2.width 3 "Length (width) of a matrix is not the same as the original list"
                        Expect.equal (subject2.list ()) lst2 "Matrix didn't convert to the original list"

                        let lst3 =
                            [ [ 1; 1; 1 ]
                              [ 1; 1; 1 ]
                              [ 1; 1; 1 ]
                              [ 1; 1; 1 ] ]

                        let subject3: Matrix<int> = Matrix(lst3)

                        Expect.equal
                            subject3.height
                            4
                            "Length (height) of a matrix is not the same as the original list"

                        Expect.equal subject3.width 3 "Length (width) of a matrix is not the same as the original list"
                        Expect.equal (subject3.list ()) lst3 "Matrix didn't convert to the original list"
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
                    testProperty "Multiplication of vectors to matrixes (prop)"
                    <| fun (intVec: Vector<int>) ->
                        let n = intVec.length

                        if n <> 0 then
                            let ZArray = List.replicate n <| List.replicate n 0
                            let ZMat: Matrix<int> = Matrix(ZArray)

                            let EArray =
                                ZArray
                                |> List.mapi (fun i arr_i ->
                                    arr_i.[i] = 1 |> ignore
                                    arr_i)

                            let EMat: Matrix<int> = Matrix(EArray)

                            Expect.allEqual
                                ((intVec.mult (+) (*) ZMat).list ())
                                0
                                "Standart multiplication of a vector(int) to zero matrix failed"

                            //let file = File.CreateText("out.txt")
                            //List.iter (fprintf file "%d") (intVec.list ())
                            //file.Close()

                            Expect.equal
                                (intVec.mult (+) (*) EMat)
                                intVec
                                "Standart multiplication of a vector(int) to Id matrix failed" ] ]
