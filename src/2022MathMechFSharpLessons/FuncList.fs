namespace _2022MathMechFSharpLessons

open System

module FuncList =
    // Functional implementation of list
    type List<'value> =
        | Cons of head: 'value * tail: List<'value>
        | Empty

    // For testing: get random list
    let rec RandList length =
        let rng = new Random()

        if length = 0 then
            Empty
        else
            Cons(rng.Next() % 1000 - 500, RandList(length - 1))

    // For testing: insersion sort
    let rec InsertionSort cmp a =
        match a with
        | Empty -> Empty
        | Cons (a0, Empty) -> a
        | Cons (a0, axs) ->
            match (InsertionSort cmp axs) with
            | Cons (a1, axs) when cmp a0 a1 -> Cons(a1, InsertionSort cmp (Cons(a0, axs)))
            | a -> Cons(a0, a)

    let rec Map f lst =
        match lst with
        | Empty -> Empty
        | Cons (hd, tl) -> Cons(f hd, Map f tl)

    let rec ConCat a b =
        match a with
        | Empty -> b
        | Cons (hd, tl) -> Cons(hd, ConCat tl b)

    let rec GetLength a =
        match a with
        | Empty -> 0
        | Cons (_, a) -> 1 + GetLength a

    let BubbleSort cmp a =
        let rec RaiseBubble cmp a =
            match a with
            | Empty -> Empty
            | Cons (a0, Empty) -> a
            | Cons (a0, Cons (a1, axs)) when cmp a0 a1 -> RaiseBubble cmp (Cons(a1, Cons(a0, axs)))
            | Cons (a0, Cons (a1, axs)) -> Cons(a0, RaiseBubble cmp (Cons(a1, axs)))

        let len = GetLength a

        let rec recursiveLoop n f a =
            if n = 1 then
                f a
            else
                recursiveLoop (n - 1) f (f a)

        // Map ~= recursiveLoop
        recursiveLoop len (RaiseBubble cmp) a // TODO: no mutable + no loop

    // expects cmp to be asymmetric
    let QuickSort cmp a =
        let rec partition cmp pivot a =
            match a with
            | Cons (a0, axs) when cmp pivot a0 ->
                let f, s = partition cmp pivot axs
                Cons(a0, f), s
            | Cons (a0, axs) ->
                let f, s = partition cmp pivot axs
                f, Cons(a0, s)
            | _ -> Empty, Empty

        let rec qsort cmp a =
            match a with
            | Cons (a0, Empty) -> a
            | Cons (a0, Cons (a1, Empty)) ->
                if cmp a0 a1 then
                    Cons(a1, Cons(a0, Empty))
                else
                    Cons(a0, Cons(a1, Empty))
            | Cons (a0, _) ->
                let firstHalf, secondHalf =
                    match partition cmp a0 a with
                    | Empty, _ ->
                        if cmp (a0 + 1) a0 then
                            partition cmp (a0 + 1) a
                        else
                            partition cmp (a0 - 1) a
                    | _, Empty ->
                        if cmp a0 (a0 + 1) then
                            partition cmp (a0 + 1) a
                        else
                            partition cmp (a0 - 1) a
                    | f, s -> f, s

                ConCat(qsort cmp firstHalf) (qsort cmp secondHalf) // TODO: no concat...
            | _ -> Empty

        qsort cmp a
