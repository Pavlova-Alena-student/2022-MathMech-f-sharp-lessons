namespace _2022MathMechFSharpLessons.Tests

open Expecto
open _2022MathMechFSharpLessons

module OOPListTests =
    open Task2
    open OOPList

    (*testCase "OOP length test"
              <| fun _ ->
                  let a = NonEmptyList(4, NonEmptyList(3, NonEmptyList(9, EmptyList<int>())))
                  let subject = GetLength a
                  Expect.equal subject 3 "Failed to get length of a OOP list"*)

    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "OOP concatination test"
              <| fun _ ->
                  let a = NonEmptyList(4, NonEmptyList(3, NonEmptyList(9, EmptyList<int>())))
                  let b = NonEmptyList(4, NonEmptyList(3, NonEmptyList(9, EmptyList<int>())))
                  let subject: IList<int> = ConCat a b

                  Expect.isTrue
                      (Compare
                          subject
                          (NonEmptyList(
                              4,
                              NonEmptyList(
                                  3,
                                  NonEmptyList(9, NonEmptyList(4, NonEmptyList(3, NonEmptyList(9, EmptyList<int>()))))
                              )
                          )))
                      "Failed to concat two OOP lists" ]

(*
              testCase "OOP insertion sort test"
              <| fun _ ->
                  let a: List<int> = Cons(4, Cons(3, Cons(9, Empty)))
                  let subject = InsertionSort(>) a
                  Expect.equal subject (Cons(3, Cons(4, Cons(9, Empty)))) "Failed to sort a OOP list (insertion)"
              testCase "OOP bubble sort test"
              <| fun _ ->
                  let a: List<int> = Cons(4, Cons(3, Cons(9, Empty)))
                  let subject = BubbleSort(>) a
                  Expect.equal subject (Cons(3, Cons(4, Cons(9, Empty)))) "Failed to sort a OOP list (bubble)"
              testCase "Comparing OOP sort test"
              <| fun _ ->
                  let a: List<int> = RandList 20
                  let b = a
                  let control = InsertionSort(>) a
                  let subject = BubbleSort(>) b
                  Expect.equal subject control "Sorting OOP list with insertion <> with bubble"
              testCase "Comparing OOP qsort test"
              <| fun _ ->
                  let a: List<int> = RandList 20
                  let b = a
                  let control = InsertionSort(<) a
                  let subject = QuickSort(<) b
                  Expect.equal subject control "Sorting OOP list with insertion <> with qsort" ]
*)
