namespace _2022MathMechFSharpLessons

module Task1 =
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
