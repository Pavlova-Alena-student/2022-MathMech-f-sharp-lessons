namespace MathMechFSharpLessons.Tests

open Expecto
open FsCheck
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

    // https://fscheck.github.io/FsCheck//TestData.html
    let treeGen<'elementType> =
        let rec treeGen' mSize =
            match mSize with
            | 0 -> Gen.constant (EmptyList<'elementType>() :> IList<'elementType>)
            | n when n > 0 ->
                let subtree = treeGen' (n - 1)

                Gen.oneof [ Gen.constant (EmptyList<'elementType>() :> IList<'elementType>)
                            Gen.map2
                                (fun hd tl -> NonEmptyList<'elementType>(hd, tl))
                                Arb.generate<'elementType>
                                subtree ]
            | _ -> invalidArg "mSize" "Only positive arguments are allowed"

        Gen.sized treeGen'

    type OOPListGenerator =
        static member IList() = Arb.fromGen treeGen

    Arb.register<OOPListGenerator> () |> ignore

    let config =
        { FsCheckConfig.defaultConfig with arbitrary = [ typeof<OOPListGenerator> ] }

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
                  Expect.equal (GetLength subject) 30 "Failed to sort a OOP list: wrong length (quicksort)"

              testProperty "OOP concatination test (prop)"
              <| fun (a: IList<int>, b: IList<int>) ->
                  let subject = ConCat a b

                  Expect.equal
                      (GetLength subject)
                      ((GetLength a) + (GetLength b))
                      "Length of concatinated lists is invalid"
              testProperty "OOP bubble sort test (prop)"
              <| fun (a: IList<int>) ->
                  let cmp = LessThan()
                  let sorter = BubbleSort<int>() :> IListSortAlgorithm<int>
                  let subject = sorter.sort cmp a
                  Expect.isTrue (IsSorted(<) subject) "Failed to sort a OOP list (bubble)"
                  Expect.equal (GetLength subject) (GetLength a) "Failed to sort a OOP list: wrong length (bubble)"
              testProperty "OOP qsort test (prop)"
              <| fun (a: IList<int>) ->
                  let cmp = LessThan()
                  let sorter = QuickSort<int>() :> IListSortAlgorithm<int>
                  let subject = sorter.sort cmp a
                  Expect.isTrue (IsSorted(<) subject) "Failed to sort a OOP list (bubble)"
                  Expect.equal (GetLength subject) (GetLength a) "Failed to sort a OOP list: wrong length (bubble)" ]
