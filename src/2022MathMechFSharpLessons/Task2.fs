namespace _2022MathMechFSharpLessons

open System

module Task2 =
    // Functional implementation of list
    type MyFuncList<'value> =
        | Cons of head: 'value * tail: MyFuncList<'value>
        | Empty

    // For testing: get random list
    let rec MyFuncRandList length =
        let rng = new Random()

        if length = 0 then
            Empty
        else
            Cons(rng.Next() % 100, MyFuncRandList(length - 1))

    // For testing: insersion sort
    let rec MyFuncInsertionSort cmp a =
        match a with
        | Empty -> Empty
        | Cons (a0, Empty) -> a
        | Cons (a0, axs) ->
            match (MyFuncInsertionSort cmp axs) with
            | Cons (a1, axs) when cmp a0 a1 -> Cons(a1, MyFuncInsertionSort cmp (Cons(a0, axs)))
            | a -> Cons(a0, a)

    let rec MyFuncConCat a b =
        match a with
        | Empty -> b
        | Cons (hd, tl) -> Cons(hd, MyFuncConCat tl b)

    let rec GetLength a =
        match a with
        | Empty -> 0
        | Cons (_, a) -> 1 + GetLength a

    let MyFuncBubbleSort cmp a =
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

        recursiveLoop len (RaiseBubble cmp) a // TODO: no mutable + no loop
        // fold ~= recursiveLoop

    // expects cmp to be asymmetric
    let MyFuncQuickSort cmp a =
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

                MyFuncConCat(qsort cmp firstHalf) (qsort cmp secondHalf) // TODO: no concat...
            | _ -> Empty

        qsort cmp a

    // OOP implementation of list
    type IList<'value> =
        interface
        end

    type MyOOPNonEmptyList<'value>(head: 'value, tail: IList<'value>) =
        interface IList<'value>
        member this.Head = head
        member this.Tail = tail

    type MyOOPEmptyList<'value>() =
        interface IList<'value>

// TODO: MyOOPBubbleSort

// TODO: MyOOPQuickSort

// TODO: MyOOPConCat
