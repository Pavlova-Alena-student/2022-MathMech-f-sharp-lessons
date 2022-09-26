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
              testCase "In power of 0"
              <| fun _ ->
                  let subject = Task1.Power(10, 0)
                  Expect.equal subject 1 "Neutral multiplication faild: 10^0 <> 1"
              testCase "Exception on 0^0"
              <| fun _ -> Expect.throws (fun _ -> Task1.Power(0, 0) |> ignore) "Should fail to calculate on 0^0"
              testCase "Exception on negative power"
              <| fun _ -> Expect.throws (fun _ -> Task1.Power(11, -7) |> ignore) "Should fail to calculate on 11^-7"

              testCase "Regular power (naive)"
              <| fun _ ->
                  let subject = Task1.NaivePower(3, 4)
                  Expect.equal subject 81 "Failed to get 3 to power of 4"
              testCase "Power of 0 (naive)"
              <| fun _ ->
                  let subject = Task1.NaivePower(0, 20)
                  Expect.equal subject 0 "Can't multiply 0 to 0 20 times"
              testCase "In power of 0 (naive)"
              <| fun _ ->
                  let subject = Task1.NaivePower(10, 0)
                  Expect.equal subject 1 "Neutral multiplication faild: 10^0 <> 1"
              testCase "Exception on 0^0 (naive)"
              <| fun _ -> Expect.throws (fun _ -> Task1.NaivePower(0, 0) |> ignore) "Should fail to calculate on 0^0"
              testCase "Exception on negative power (naive)"
              <| fun _ ->
                  Expect.throws (fun _ -> Task1.NaivePower(11, -7) |> ignore) "Should fail to calculate on 11^-7"

              testCase "Speed test on power functions"
              <| fun _ ->
                  Expect.isFasterThan
                      (fun _ -> Task1.Power(2, 30) |> ignore)
                      (fun _ -> Task1.NaivePower(2, 30) |> ignore)
                      "Fast power should be faster then naive implementation" ]
