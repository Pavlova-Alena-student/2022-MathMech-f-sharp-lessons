namespace MathMechFSharpLessons.Tests

open Expecto
open MathMechFSharpLessons

module TreeTests =
    open Tree

    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "Tree test - TODO"
              <| fun _ ->
                  // TODO
                  Expect.equal 1 1 "TODO"
              testCase "Tree fold - binfrarr"
              <| fun _ ->
                  let arra = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |]
                  let tree = BinaryFromArray arra
                  Expect.equal tree (Leaf(0)) "should fail"
              testCase "Tree fold - sum"
              <| fun _ ->
                  let arra = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |]
                  let tree = BinaryFromArray arra
                  let addition = Fold(fun ac value -> ac + value) 0 tree
                  Expect.equal addition 54 "should fail by 1"
              testCase "Tree fold - collect"
              <| fun _ ->
                  let arra = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |]
                  let tree = BinaryFromArray arra
                  let collection = Fold(fun ac value -> FuncList.Cons(value, ac)) FuncList.Empty tree
                  Expect.equal collection FuncList.Empty "should fail"
              testCase "Tree fold todo"
              <| fun _ -> Expect.equal 1 1 "TODO" ]
