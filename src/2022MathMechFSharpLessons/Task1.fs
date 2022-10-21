namespace MathMechFSharpLessons

module Task1 =
    // Causion! Fails with empty array
    let Dispersion (a: int list) =
        (List.reduce (fun acc item -> if acc < item then item else acc) a)
        - (List.reduce (fun acc item -> if acc > item then item else acc) a)

    let OddBetween (a, b) = [| a / 2 * 2 + 1 .. 2 .. b |]

    let rec NaivePower (a, b) =
        match b with
        | 0 ->
            if a <> 0 then
                1
            else
                raise <| new System.InvalidOperationException()
        | 1 -> a
        | b when b > 0 -> a * NaivePower(a, b - 1)
        | _ -> raise <| new System.InvalidOperationException()

    let rec Power (a, b) =
        match b with
        | 0 ->
            if a <> 0 then
                1
            else
                raise <| new System.InvalidOperationException()
        | 1 -> a
        | b when b > 0 ->
            let half = Power(a, b / 2)

            if (b % 2) = 0 then
                half * half
            else
                half * half * a
        | _ -> raise <| new System.InvalidOperationException()
