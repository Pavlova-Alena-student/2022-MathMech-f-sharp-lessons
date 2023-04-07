namespace MathMechFSharpLessons.Tests

open Expecto
open MathMechFSharpLessons

module TreeTests =
    open FuncList
    open Tree

    [<Tests>]
    let tests =
        testList
            "TreeTests"
            [ testCase "Binary Tree from array"
              <| fun _ ->
                  let array = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |]
                  let tree = BinaryFromArray array
                  //           1
                  //     2          3
                  //  4     5      6   7
                  // 8 9  10
                  Expect.equal
                      tree
                      (Node(
                          1,
                          Cons(
                              Node(
                                  2,
                                  Cons(
                                      Node(4, Cons(Leaf(8), Cons(Leaf(9), Empty))),
                                      Cons(Node(5, Cons(Leaf(10), Empty)), Empty)
                                  )
                              ),
                              Cons(Node(3, Cons(Leaf(6), Cons(Leaf(7), Empty))), Empty)
                          )
                      ))
                      "Binary tree structure expected for array [1..10]"
              testCase "Tree fold - sum"
              <| fun _ ->
                  let array = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |]
                  let tree = BinaryFromArray array
                  let addition = Fold(fun ac value -> ac + value) 0 tree
                  Expect.equal addition 55 "Summation of numbers 1..10 didn't work in tree"
              testCase "Tree fold - collect ( = GetList)"
              <| fun _ ->
                  let array = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |]
                  let tree = BinaryFromArray array
                  let collection = Fold(fun ac value -> FuncList.Cons(value, ac)) FuncList.Empty tree
                  let lst = GetList tree // same

                  Expect.equal
                      collection
                      lst
                      "Getting list from tree should be the same as folding it? No, but they should consist of same elements. Correct this test, if it fails"

                  Expect.equal
                      (GetLength collection)
                      10
                      "Getting list from tree with 10 elements should consist of 10 elements"
              testCase "GetSortedList from tree test"
              <| fun _ ->
                  let array = [| 1; 2; 3; 4; 5; 6; 5; 5; 2; 1 |]
                  let tree = BinaryFromArray array
                  let subject = GetSortedList(<) tree

                  let sortedList =
                      Cons(1, Cons(1, Cons(2, Cons(2, Cons(3, Cons(4, Cons(5, Cons(5, Cons(5, Cons(6, Empty))))))))))

                  Expect.equal subject sortedList "GetSortedList from small tree returned unexpected result"
              testCase "Count different elements in tree test"
              <| fun _ ->
                  let array = [| 1; 2; 3; 4; 5; 6; 5; 5; 2; 1 |]
                  let tree = BinaryFromArray array
                  let subject = CountDifferent tree

                  Expect.equal
                      subject
                      6
                      "Counting different in tree, made from small array with 6 diffeent elements, should still be 6 elements"
              testProperty "Tree GetList test (prop)"
              <| fun (tree: Tree<int>) ->
                  let subject = GetList tree
                  Expect.isNotEmpty (string subject) "GetList should return at least something"
                  Expect.notEqual subject Empty "GetList should return at least 1 element"
              testProperty "GetSortedList from tree test (prop)"
              <| fun (tree: Tree<int>) ->
                  let subject = GetSortedList(<) tree
                  Expect.isTrue (FuncListTests.IsSorted(<) subject) "GetSortedList should return sorted list"
                  Expect.notEqual subject Empty "Tree can't be empty (0 elements)"
              testProperty "Count different elements in tree test (prop)"
              <| fun (tree: Tree<int>) ->
                  let lst = GetList tree
                  let subject = CountDifferent tree

                  Expect.isLessThanOrEqual
                      subject
                      (GetLength lst)
                      "The amount of different elements in tree, should be less or equal to the amount of all elements" ]
