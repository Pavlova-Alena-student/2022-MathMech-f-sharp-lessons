namespace MathMechFSharpLessons.Tests

open Expecto
open MathMechFSharpLessons

module OOPListTests =
    open Task2
    open OOPList

    type GreaterThan() =
        interface IComparer<int> with
            member this.Compare (a: int) (b: int) = a > b

    type LessThan() =
        interface IComparer<int> with
            member this.Compare (a: int) (b: int) = a < b

    let rec IsSorted cmp (lst: IList<'value>) =
        match lst with
        | :? NonEmptyList<'value> as lst ->
            match (lst.Tail) with
            | :? NonEmptyList<'value> as tl ->
                let a0 = lst.Head
                let a1 = tl.Head

                ((cmp a0 a1) || (not <| cmp a1 a0))
                && (IsSorted cmp tl)
            | :? EmptyList<'value> -> true
            | _ -> raise <| UnknownListTypeException()
        | :? EmptyList<'value> -> true
        | _ -> raise <| UnknownListTypeException()

    (*
    let properties =
    testList "OOPList properties" [
    testProperty "Sorted functional list is sorted" <| fun cmp sortF lst ->
      IsSorted cmp (sortF cmp lst)

    testProperty "Sorted functional list has same size as original" <| fun cmp sortF lst ->
      GetLength lst = GetLength (sortF cmp lst)
    ]
*)

    [<Tests>]
    let tests =
        testList
            "OOP sample tests"
            [ testCase "OOP length test"
              <| fun _ ->
                  let a = NonEmptyList(4, NonEmptyList(3, NonEmptyList(9, EmptyList<int>())))
                  let subject = GetLength a
                  Expect.equal subject 3 "Failed to get length of a OOP list"
              testCase "OOP small concatination test"
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
                      "Failed to concat two small OOP lists"
              testCase "OOP concatination test"
              <| fun _ ->
                  let a = GenerateList(fun _ -> 4) 4
                  let b = GenerateList(fun _ -> 3) 5
                  let subject1: IList<int> = ConCat a b
                  let subject2: IList<int> = ConCat b a

                  Expect.equal (GetLength subject1) 9 "Length of concatinated lists is invalid"
                  Expect.equal (GetLength subject2) 9 "Length of concatinated lists is invalid"

                  Expect.isFalse (Compare subject1 subject2) "Failed to concat: 3 <> 4"
                  Expect.equal (subject1 :?> NonEmptyList<int>).Head 4 "Failed to concat: wrong head"
                  Expect.equal (subject2 :?> NonEmptyList<int>).Head 3 "Failed to concat: wrong head"
              testCase "OOP bubble sort test"
              <| fun _ ->
                  let a = RandList 30
                  let cmp = GreaterThan()
                  let sorter = BubbleSort<int>() :> IListSortAlgorithm<int>
                  let subject = sorter.sort cmp a
                  Expect.isTrue (IsSorted(>) subject) "Failed to sort a OOP list (bubble)"
                  Expect.equal (GetLength subject) 30 "Failed to sort a OOP list: wrong length (bubble)"
              testCase "OOP qsort test"
              <| fun _ ->
                  let a = RandList 30
                  let cmp = LessThan()
                  let sorter = QuickSort<int>() :> IListSortAlgorithm<int>
                  let subject = sorter.sort cmp a
                  Expect.isTrue (IsSorted(<) subject) "Failed to sort a OOP list (quicksort)"
                  Expect.equal (GetLength subject) 30 "Failed to sort a OOP list: wrong length (quicksort)" ]