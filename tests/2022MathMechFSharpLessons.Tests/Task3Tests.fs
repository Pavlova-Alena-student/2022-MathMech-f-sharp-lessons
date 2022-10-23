namespace MathMechFSharpLessons.Tests

open Expecto
open MathMechFSharpLessons

module Task3Tests =
    open Tree
    open Task3

    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "Sample2"
              <| fun _ -> Expect.equal 0 0 "0<>0" ]
