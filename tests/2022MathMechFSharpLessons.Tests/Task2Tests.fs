namespace _2022MathMechFSharpLessons.Tests

open Expecto
open _2022MathMechFSharpLessons

// TODO!!!
module Task2Tests =
    open Task2

    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "Length test"
              <| fun _ ->
                  let a: Task2.MyFuncList<int> = Cons(4, Cons(3, Cons(9, Empty)))
                  let subject = Task2.GetLength a
                  Expect.equal subject 3 "Failed to get length of a functional list"
              testCase "Concatination test"
              <| fun _ ->
                  let a: Task2.MyFuncList<int> = Cons(4, Cons(3, Cons(9, Empty)))
                  let b: Task2.MyFuncList<int> = Cons(4, Cons(3, Cons(9, Empty)))
                  let subject = Task2.MyFuncConCat a b

                  Expect.equal
                      subject
                      (Cons(4, Cons(3, Cons(9, Cons(4, Cons(3, Cons(9, Empty)))))))
                      "Failed to concat two functional lists"
              testCase "Bubble sort test"
              <| fun _ ->
                  let a: Task2.MyFuncList<int> = Cons(4, Cons(3, Cons(9, Empty)))
                  let subject = Task2.MyFuncBubbleSort(>) a
                  Expect.equal subject (Cons(3, Cons(4, Cons(9, Empty)))) "Failed to sort a functional list" ]
