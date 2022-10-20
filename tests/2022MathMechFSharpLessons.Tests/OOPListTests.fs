namespace _2022MathMechFSharpLessons.Tests

open Expecto
open _2022MathMechFSharpLessons

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

    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "OOP length test"
              <| fun _ ->
                  let a = NonEmptyList(4, NonEmptyList(3, NonEmptyList(9, EmptyList<int>())))
                  let subject = GetLength a
                  Expect.equal subject 3 "Failed to get length of a OOP list"
              testCase "OOP concatination test"
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
                      "Failed to concat two OOP lists"
              testCase "OOP bubble sort test"
              <| fun _ ->
                  let a = RandList 30
                  let cmp = GreaterThan()
                  let sorter = BubbleSort<int>() :> IListSortAlgorithm<int>
                  let subject = sorter.sort cmp a
                  Expect.isTrue (IsSorted(>) subject) "Failed to sort a OOP list (bubble)"
              testCase "OOP qsort test"
              <| fun _ ->
                  let a = RandList 30
                  let cmp = LessThan()
                  let sorter = QuickSort<int>() :> IListSortAlgorithm<int>
                  let subject = sorter.sort cmp a
                  Expect.isTrue (IsSorted(<) subject) "Failed to sort a OOP list (quicksort)" ]
