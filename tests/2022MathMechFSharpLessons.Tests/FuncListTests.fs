namespace MathMechFSharpLessons.Tests

open Expecto
open FsCheck
open MathMechFSharpLessons

module FuncListTests =
    open Task2
    open FuncList

    let rec IsSorted cmp lst =
        match lst with
        | Cons (hd1, Cons (hd2, tl)) ->
            ((cmp hd1 hd2) || (not <| cmp hd2 hd1))
            && (IsSorted cmp (Cons(hd2, tl)))
        | _ -> true

    [<Tests>]
    let tests =
        testList
            "FuncList tests"
            [ testList
                  "GetLength tests for FuncList"
                  [ testCase "Functional length test (empty)"
                    <| fun _ ->
                        let subject = GetLength Empty
                        Expect.equal subject 0 "Length of empty list should be 0"

                    testCase "Functional length test"
                    <| fun _ ->
                        let a: List<int> = Cons(4, Cons(3, Cons(9, Empty)))
                        let subject = GetLength a
                        Expect.equal subject 3 "Failed to get length of a functional list" ]

              testList
                  "Concatination tests for FuncList"
                  [ testCase "Concatination test"
                    <| fun _ ->
                        let a: List<int> = Cons(4, Cons(3, Cons(9, Empty)))
                        let b: List<int> = Cons(4, Cons(3, Cons(9, Empty)))
                        let subject = Concat a b

                        Expect.equal
                            subject
                            (Cons(4, Cons(3, Cons(9, Cons(4, Cons(3, Cons(9, Empty)))))))
                            "Failed to concat two functional lists"

                        Expect.equal
                            (GetLength subject)
                            ((GetLength a) + (GetLength b))
                            "Length of concatinated functional list is not sum of lengthes of two functional lists"

                    testProperty "Concatination with empty test (property)"
                    <| fun (a: List<_>) ->
                        let subject = Concat a Empty
                        Expect.equal subject a "Concatination with empty list changed original list! (right)"
                        let subject = Concat Empty a
                        Expect.equal subject a "Concatination with empty list changed original list! (left)"

                    testProperty "Concatination test (property)"
                    <| fun (a: List<_>, b: List<_>) ->
                        let subject = Concat a b

                        Expect.equal
                            (GetLength subject)
                            ((GetLength a) + (GetLength b))
                            "Concatination length is different than sum of original lists!" ]

              testList
                  "Insertion sort tests for FuncList"
                  [ testCase "Small functional insertion sort test"
                    <| fun _ ->
                        let a: List<int> = Cons(4, Cons(3, Cons(9, Empty)))
                        let subject = InsertionSort(<) a

                        Expect.equal
                            subject
                            (Cons(3, Cons(4, Cons(9, Empty))))
                            "Failed to sort a functional list (insertion)"

                    testCase "Empty list insertion sort test"
                    <| fun _ ->
                        let a: List<int> = Empty
                        let subject = InsertionSort(<) a
                        Expect.equal subject Empty "Sorting empty functional list with insertion failed"

                    testProperty "Insertion sort test (property)"
                    <| fun (a: List<int>) ->
                        let subject = InsertionSort(>) a
                        Expect.isTrue (IsSorted(>) subject) "Bubble sort didn't sort!"
                        Expect.equal (GetLength subject) (GetLength a) "Bubble sort changed length!" ]

              testList
                  "Bubble sort tests for FuncList"
                  [ testCase "Small functional bubble sort test"
                    <| fun _ ->
                        let a: List<int> = Cons(4, Cons(3, Cons(9, Empty)))
                        let subject = BubbleSort(<) a

                        Expect.equal
                            subject
                            (Cons(3, Cons(4, Cons(9, Empty))))
                            "Failed to sort a functional list (bubble)"

                    testCase "Empty list bubble sort test"
                    <| fun _ ->
                        let a: List<int> = Empty
                        let subject = BubbleSort(<) a
                        Expect.equal subject Empty "Sorting empty functional list with bubbles failed"

                    testProperty "Bubble sort test (property)"
                    <| fun (a: List<int>) ->
                        let subject = BubbleSort(>) a
                        Expect.isTrue (IsSorted(>) subject) "Bubble sort didn't sort!"
                        Expect.equal (GetLength subject) (GetLength a) "Bubble sort changed length!"

                    testProperty "Comparing bubble sort test (property)"
                    <| fun (a: List<int>) ->
                        // here I hope that InsertionSort is pure, so I can reuse variable a
                        // should be something like `let b = copy a`
                        let control = InsertionSort(>) a
                        let subject = BubbleSort(>) a
                        Expect.equal subject control "Sorting functional list with insertion <> with bubble" ]

              testList
                  "Quick sort tests for FuncList"
                  [ testCase "Empty list functional qsort test"
                    <| fun _ ->
                        let a: List<int> = Empty
                        let subject = QuickSort(<) a
                        Expect.equal subject Empty "Sorting empty functional list with quicksort failed"

                    testProperty "Functional qsort test (prop)"
                    <| fun (a: List<int>) ->
                        let subject = QuickSort(<) a
                        Expect.isTrue (IsSorted(<) subject) "Quick sort didn't sort!"
                        Expect.equal (GetLength subject) (GetLength a) "Quick sort changed length!"

                    testProperty "Comparing functional qsort test (prop)"
                    <| fun (a: List<int>) ->
                        // here I hope that InsertionSort is pure, so I can reuse variable a
                        // should be something like `let b = copy a`
                        let control = InsertionSort(<) a
                        let subject = QuickSort(<) a
                        Expect.equal subject control "Sorting functional list with insertion <> with qsort" ]

              testList
                  "Fold tests for FuncList"
                  [ testCase "Empty list fold test"
                    <| fun _ ->
                        let a: List<int> = Empty
                        let subject = Fold(fun acc v -> Cons(v, acc)) Empty a
                        Expect.equal subject Empty "Folding empty list failed: not empty"
                        let subject = Fold(fun acc v -> v + acc) 0 a
                        Expect.equal subject 0 "Folding empty list failed: sum is not 0" ]

              testList
                  "SortedList module tests"
                  [ testCase "Fusion small test"
                    <| fun _ ->
                        let a = Cons(0, Cons(2, Cons(4, Cons(6, Empty))))
                        let b = Cons(0, Cons(3, Cons(6, Empty)))
                        let subject = SortedFuncList.FuseSorted(<) a b

                        Expect.equal
                            (GetLength subject)
                            (GetLength a + GetLength b)
                            "Small fusion failed: length is different"

                        Expect.equal
                            subject
                            (Cons(0, Cons(0, Cons(2, Cons(3, Cons(4, Cons(6, Cons(6, Empty))))))))
                            "Small fusion failed!"

                    testCase "Deletion of equal small test"
                    <| fun _ ->
                        let a = Cons(0, Cons(0, Cons(0, Empty)))
                        let subject = SortedFuncList.DeleteEqual a
                        Expect.equal subject (Cons(0, Empty)) "Couldn't delete equal elements in small list"

                    testProperty "Fusion test (prop)"
                    <| fun (a: List<int>, b: List<int>) ->
                        let srtd1 = QuickSort(<) a
                        let srtd2 = QuickSort(<) b
                        let subject = SortedFuncList.FuseSorted(<) srtd1 srtd2

                        Expect.equal
                            (GetLength subject)
                            (GetLength a + GetLength b)
                            "Fusion failed: length is different"

                        Expect.isTrue (IsSorted(<) subject) "Fusion of sorted lists returned unsorted!"

                    testProperty "Deletion of equal test (prop)"
                    <| fun (a: List<int>) ->
                        let srtd = QuickSort(<) a
                        let subject = SortedFuncList.DeleteEqual srtd

                        Expect.isTrue
                            (match subject, srtd with
                             | Empty, Empty -> true
                             | Cons (v1, _), Cons (v2, _) when v1 = v2 -> true
                             | _ -> false)
                            "First element of list disapperared after deleting equal elements!"

                        Expect.isLessThanOrEqual
                            (GetLength subject)
                            (GetLength a)
                            "Amount of elements increased after deleting some"

                        Expect.isTrue
                            (IsSorted(<) subject)
                            "Deletion of some elements in sorted list made this list unsorted!" ] ]
