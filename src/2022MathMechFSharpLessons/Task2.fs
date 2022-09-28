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
        let mutable ans = a // TODO: mutable is bad? Get rid of it, just in case

        for i in 1..len do
            ans <- RaiseBubble cmp ans

        ans

    // TODO: MyFuncQuickSort

    let rec MyFuncConCat a b =
        match a with
        | Empty -> b
        | Cons (hd, tl) -> Cons(hd, MyFuncConCat tl b)

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
