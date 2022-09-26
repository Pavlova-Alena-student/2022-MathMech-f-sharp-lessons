namespace _2022MathMechFSharpLessons.Tests

open Expecto
open _2022MathMechFSharpLessons

module Task1Tests =
    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "Regular power"
              <| fun _ ->
                  let subject = Task1.Power(3, 4)
                  Expect.equal subject 81 "Failed to get 3 to power of 4"
              testCase "Power of 0"
              <| fun _ ->
                  let subject = Task1.Power(0, 20)
                  Expect.equal subject 0 "Can't multiply 0 to 0 20 times"
              testCase "In power 0"
              <| fun _ ->
                  let subject = Task1.Power(10, 0)
                  Expect.equal subject 1 "Neutral multiplication faild: 10^0 <> 1"
              testCase "Exception on 0^0"
              <| fun _ -> Expect.throws (fun _ -> Task1.Power(0, 0) |> ignore) "Should fail to calculate on 0^0" ]
