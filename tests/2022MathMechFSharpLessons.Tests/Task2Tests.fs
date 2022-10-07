namespace _2022MathMechFSharpLessons.Tests

open Expecto
open _2022MathMechFSharpLessons

module Task2Tests =
    open Task2
    open OOPList
    open FuncList

    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "Functional length test"
              <| fun _ ->
                  let a: List<int> = Cons(4, Cons(3, Cons(9, Empty)))
                  let subject = GetLength a
                  Expect.equal subject 3 "Failed to get length of a functional list"
              testCase "Functional concatination test"
              <| fun _ ->
                  let a: List<int> = Cons(4, Cons(3, Cons(9, Empty)))
                  let b: List<int> = Cons(4, Cons(3, Cons(9, Empty)))
                  let subject = ConCat a b

                  Expect.equal
                      subject
                      (Cons(4, Cons(3, Cons(9, Cons(4, Cons(3, Cons(9, Empty)))))))
                      "Failed to concat two functional lists"
              testCase "Functional insertion sort test"
              <| fun _ ->
                  let a: List<int> = Cons(4, Cons(3, Cons(9, Empty)))
                  let subject = InsertionSort(>) a

                  Expect.equal subject (Cons(3, Cons(4, Cons(9, Empty)))) "Failed to sort a functional list (insertion)"
              testCase "Functional bubble sort test"
              <| fun _ ->
                  let a: List<int> = Cons(4, Cons(3, Cons(9, Empty)))
                  let subject = BubbleSort(>) a
                  Expect.equal subject (Cons(3, Cons(4, Cons(9, Empty)))) "Failed to sort a functional list (bubble)"
              testCase "Comparing functional sort test"
              <| fun _ ->
                  let a: List<int> = RandList 20
                  let b = a
                  let control = InsertionSort(>) a
                  let subject = BubbleSort(>) b
                  Expect.equal subject control "Sorting functional list with insertion <> with bubble"
              testCase "Comparing functional qsort test"
              <| fun _ ->
                  let a: List<int> = RandList 20
                  let b = a
                  let control = InsertionSort(<) a
                  let subject = QuickSort(<) b
                  Expect.equal subject control "Sorting functional list with insertion <> with qsort" ]
