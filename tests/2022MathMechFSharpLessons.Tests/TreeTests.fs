namespace MathMechFSharpLessons.Tests

open Expecto
open MathMechFSharpLessons

module TreeTests =
    open Tree

    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "Sample1"
              <| fun _ -> Expect.equal 0 0 "0<>0" ]
