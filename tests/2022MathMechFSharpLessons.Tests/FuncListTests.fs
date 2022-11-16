namespace MathMechFSharpLessons.Tests

open Expecto
open Expecto.ExpectoFsCheck
open FsCheck
open MathMechFSharpLessons

module FuncListTests =
    open Task2
    open FuncList

    let rec IsSorted cmp lst =
        match lst with
        | Cons (a0, Cons (a1, axs)) ->
            ((cmp a0 a1) || (not <| cmp a1 a0))
            && (IsSorted cmp (Cons(a1, axs)))
        | _ -> true

    [<Tests>]
    let tests =
        testList
            "all tests"
            [ testCase "Functional length test"
              <| fun _ ->
                  let a: List<int> = Cons(4, Cons(3, Cons(9, Empty)))
                  let subject = GetLength a
                  Expect.equal subject 3 "Failed to get length of a functional list"
                  let subject = GetLength Empty
                  Expect.equal subject 0 "Length of empty list should be 0"

              testCase "Functional concatination test"
              <| fun _ ->
                  let a: List<int> = Cons(4, Cons(3, Cons(9, Empty)))
                  let b: List<int> = Cons(4, Cons(3, Cons(9, Empty)))
                  let subject = ConCat a b

                  Expect.equal
                      subject
                      (Cons(4, Cons(3, Cons(9, Cons(4, Cons(3, Cons(9, Empty)))))))
                      "Failed to concat two functional lists"

                  Expect.equal
                      (GetLength subject)
                      ((GetLength a) + (GetLength b))
                      "Length of concatinated functional list is not sum of lengthes of two functional lists"

              testCase "Small functional insertion sort test"
              <| fun _ ->
                  let a: List<int> = Cons(4, Cons(3, Cons(9, Empty)))
                  let subject = InsertionSort(<) a

                  Expect.equal subject (Cons(3, Cons(4, Cons(9, Empty)))) "Failed to sort a functional list (insertion)"

              testCase "Small functional bubble sort test"
              <| fun _ ->
                  let a: List<int> = Cons(4, Cons(3, Cons(9, Empty)))
                  let subject = BubbleSort(<) a

                  Expect.equal subject (Cons(3, Cons(4, Cons(9, Empty)))) "Failed to sort a functional list (bubble)"

              testCase "Comparing functional sort test on random list with size 30"
              <| fun _ ->
                  let a: List<int> = RandList 30
                  let b = a
                  let control = InsertionSort(>) a
                  let subject = BubbleSort(>) b
                  Expect.equal subject control "Sorting functional list with insertion <> with bubble"
                  Expect.isTrue (IsSorted(>) subject) "Bubble sort didn't sort!"
                  Expect.equal (GetLength subject) 30 "Bubble sort changed length!"

              testCase "Comparing functional qsort test on random list with size 30"
              <| fun _ ->
                  let a: List<int> = RandList 30
                  let b = a
                  let control = InsertionSort(<) a
                  let subject = QuickSort(<) b
                  Expect.equal subject control "Sorting functional list with insertion <> with qsort"
                  Expect.isTrue (IsSorted(<) subject) "Quick sort didn't sort!"
                  Expect.equal (GetLength subject) 30 "Quick sort changed length!"

              testCase "Empty list functional sort test"
              <| fun _ ->
                  let control = Empty
                  let a: List<int> = Empty
                  let subjectIS = InsertionSort(<) a
                  let a: List<int> = Empty
                  let subjectBS = BubbleSort(<) a
                  let a: List<int> = Empty
                  let subjectQS = QuickSort(<) a
                  Expect.equal subjectIS control "Sorting empty functional list with insertion failed"
                  Expect.equal subjectBS control "Sorting empty functional list with bubbles failed"
                  Expect.equal subjectQS control "Sorting empty functional list with quicksort failed"

              testCase "Equal list functional insertion sort test"
              <| fun _ ->
                  let a: List<int> = GenerateList(fun _ -> 3) 5
                  let subject = InsertionSort(<) a
                  let control = Cons(3, Cons(3, Cons(3, Cons(3, Cons(3, Empty)))))
                  Expect.equal subject control "Insertion sort can't sort list of equal elements"
                  Expect.equal (GetLength subject) 5 "Insertion sort changed length of a list!"

              testCase "Equal list functional bubble sort test"
              <| fun _ ->
                  let a: List<int> = GenerateList(fun _ -> 3) 5
                  let subject = BubbleSort(<) a
                  let control = Cons(3, Cons(3, Cons(3, Cons(3, Cons(3, Empty)))))
                  Expect.equal subject control "Bubble sort can't sort list of equal elements"
                  Expect.equal (GetLength subject) 5 "Bubble sort changed length of a list!"

              testCase "Equal list functional quick sort test"
              <| fun _ ->
                  let a: List<int> = GenerateList(fun _ -> 3) 5
                  let subject = QuickSort(<) a
                  let control = Cons(3, Cons(3, Cons(3, Cons(3, Cons(3, Empty)))))
                  Expect.equal subject control "Quick sort can't sort list of equal elements"
                  Expect.equal (GetLength subject) 5 "Quick sort changed length of a list!"

              testProperty "Comparing functional sort test (prop)"
              <| fun (a: List<int>) ->
                  let b = a
                  let control = InsertionSort(>) a
                  let subject = BubbleSort(>) b
                  Expect.equal subject control "Sorting functional list with insertion <> with bubble"
                  Expect.isTrue (IsSorted(>) subject) "Bubble sort didn't sort!"
                  Expect.equal (GetLength subject) (GetLength a) "Bubble sort changed length!"

              testProperty "Comparing functional qsort test (prop)"
              <| fun (a: List<int>) ->
                  let b = a
                  let control = InsertionSort(<) a
                  let subject = QuickSort(<) b
                  Expect.equal subject control "Sorting functional list with insertion <> with qsort"
                  Expect.isTrue (IsSorted(<) subject) "Quick sort didn't sort!"
                  Expect.equal (GetLength subject) (GetLength a) "Quick sort changed length!" ]
