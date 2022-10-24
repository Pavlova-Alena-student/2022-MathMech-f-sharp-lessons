namespace MathMechFSharpLessons.Tests

open Expecto
open MathMechFSharpLessons

module TreeTests =
    open Tree

    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "Tree with equal elements"
              <| fun _ ->
                  let tree = genRangedTree 3 10 (fun _ -> 777)

                  Expect.equal
                      (getListOfDifferent tree)
                      (FuncList.Cons(777, FuncList.Empty))
                      "Tree with every value 777 should have all elements equal to 777" ]
