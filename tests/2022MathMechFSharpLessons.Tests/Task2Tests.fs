namespace MathMechFSharpLessons.Tests

open Expecto
open MathMechFSharpLessons

module Task2Tests =
    open Task2
    open OOPList
    open FuncList
    open OOPListGenerator

    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "Functional-OOP-functional list test"
              <| fun _ ->
                  let a: List<int> = Cons(4, Cons(3, Cons(9, Empty)))
                  let subject = getFuncListFromOOPList <| getOOPListFromFuncList a
                  Expect.equal subject a "Failed func-oop-func list equivalence test"
              testProperty "Functional-OOP-functional list test (property)"
              <| fun (a: List<int>) ->
                  let subject = getFuncListFromOOPList <| getOOPListFromFuncList a
                  Expect.equal subject a "Failed func-oop-func list equivalence test (prop)"
              testCase "OOP-functional-OOP list test"
              <| fun _ ->
                  let a = NonEmptyList(4, NonEmptyList(3, NonEmptyList(9, EmptyList<int>())))
                  let subject = getOOPListFromFuncList <| getFuncListFromOOPList a
                  Expect.isTrue (Compare subject a) "Failed oop-func-oop list equivalence test"
              testPropertyWithConfig OOPListGenConfig "OOP-functional-OOP list test (property)"
              <| fun (a: IList<_>) ->
                  let subject = getOOPListFromFuncList <| getFuncListFromOOPList a
                  Expect.isTrue (Compare subject a) "Failed oop-func-oop list equivalence test (prop)" ]
