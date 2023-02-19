namespace MathMechFSharpLessons.Tests

open Expecto
open FsCheck
open MathMechFSharpLessons

module OOPListTests =
    open Task2
    open OOPList
    open OOPListGenerator

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
                let head1 = lst.Head
                let head2 = tl.Head

                ((cmp head1 head2) || (not <| cmp head2 head1))
                && (IsSorted cmp tl)
            | :? EmptyList<'value> -> true
            | _ -> raise <| UnknownListTypeException()
        | :? EmptyList<'value> -> true
        | _ -> raise <| UnknownListTypeException()

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
                  let subject: IList<int> = Concat a b

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

              testPropertyWithConfig OOPListGenConfig "OOP concatination test (prop)"
              <| fun (a: IList<int>, b: IList<int>) ->
                  let subject = Concat a b

                  Expect.equal
                      (GetLength subject)
                      ((GetLength a) + (GetLength b))
                      "Length of concatinated lists is invalid"
              testPropertyWithConfig OOPListGenConfig "OOP bubble sort test (prop)"
              <| fun (a: IList<int>) ->
                  let cmp = LessThan()
                  let sorter = BubbleSort<int>() :> IListSortAlgorithm<int>
                  let subject = sorter.sort cmp a
                  Expect.isTrue (IsSorted(<) subject) "Failed to sort a OOP list (bubble)"
                  Expect.equal (GetLength subject) (GetLength a) "Failed to sort a OOP list: wrong length (bubble)"
              testPropertyWithConfig OOPListGenConfig "OOP qsort test (prop)"
              <| fun (a: IList<int>) ->
                  let cmp = LessThan()
                  let sorter = QuickSort<int>() :> IListSortAlgorithm<int>
                  let subject = sorter.sort cmp a
                  Expect.isTrue (IsSorted(<) subject) "Failed to sort a OOP list (qsort)"
                  Expect.equal (GetLength subject) (GetLength a) "Failed to sort a OOP list: wrong length (qsort)" ]
