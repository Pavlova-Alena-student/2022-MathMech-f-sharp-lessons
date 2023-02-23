namespace MathMechFSharpLessons.Tests

open Expecto
open MathMechFSharpLessons

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
                      "Fast power should be faster then naive implementation"

              testCase "Dispersion regular test"
              <| fun _ ->
                  let subject = Task1.Dispersion([ 10; 4; 2; 6; 8; 11; 4 ])
                  Expect.equal subject (11 - 2) "Something wrong with dispersion"
              testCase "Dispersion in equal elements test"
              <| fun _ ->
                  let subject = Task1.Dispersion([ for i in 1..30 -> 6 ])
                  Expect.equal subject 0 "Something wrong with dispersion in equal numbers"
              testCase "Exception on empty array (dispersion)"
              <| fun _ ->
                  Expect.throws
                      (fun _ -> Task1.Dispersion([]) |> ignore)
                      "Should fail to calculate dispersion on empty array"

              testCase "OddBetween regular test"
              <| fun _ ->
                  let subject = Task1.OddBetween(0, 15)

                  Expect.equal
                      subject
                      [| 1; 3; 5; 7; 9; 11; 13; 15 |]
                      "Something wrong with getting odd numbers between 0 and 15"
              testCase "Empty list with OddBetween"
              <| fun _ ->
                  let subject = Task1.OddBetween(0, -15)
                  Expect.equal subject [||] "List of odd numbers is not empty between 0 and -15!" ]
