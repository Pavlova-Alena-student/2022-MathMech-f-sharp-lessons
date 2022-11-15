namespace MathMechFSharpLessons

open System

module FuncList =
    // Functional implementation of list
    type List<'value> =
        | Cons of head: 'value * tail: List<'value>
        | Empty

    /// Generates a list. Generator should take one argument - reversed no. of current element and return an element on that place
    let rec GenerateList rGenerator length =
        if length = 0 then
            Empty
        else
            Cons(rGenerator length, GenerateList rGenerator (length - 1))

    /// Generates a list of random integer numbers from -500 to 499
    let RandList length =
        let rng = new Random()
        GenerateList(fun _ -> rng.Next() % 1000 - 500) length

    // For testing: insersion sort
    // expects cmp to be asymmetric
    let rec InsertionSort cmp a =
        match a with
        | Empty -> Empty
        | Cons (a0, Empty) -> a
        | Cons (a0, axs) ->
            match (InsertionSort cmp axs) with
            | Cons (a1, axs) when not <| cmp a0 a1 -> Cons(a1, InsertionSort cmp (Cons(a0, axs)))
            | a -> Cons(a0, a)

    let rec ConCat a b =
        match a with
        | Empty -> b
        | Cons (hd, tl) -> Cons(hd, ConCat tl b)

    let rec GetLength a =
        match a with
        | Empty -> 0
        | Cons (_, a) -> 1 + GetLength a

    // expects cmp to be asymmetric
    let BubbleSort cmp a =
        let rec RaiseBubble cmp a =
            match a with
            | Empty -> Empty
            | Cons (a0, Empty) -> a
            | Cons (a0, Cons (a1, axs)) when not <| cmp a0 a1 -> Cons(a1, RaiseBubble cmp (Cons(a0, axs)))
            | Cons (a0, Cons (a1, axs)) -> Cons(a0, RaiseBubble cmp (Cons(a1, axs)))

        let len = GetLength a

        let rec recursiveLoop n f a =
            if n = 0 then
                a
            else
                recursiveLoop (n - 1) f (f a)

        recursiveLoop len (RaiseBubble cmp) a

    // expects cmp to be asymmetric
    let QuickSort cmp a =
        let rec partition cmp pivot a =
            match a with
            | Cons (a0, axs) when not <| cmp pivot a0 ->
                let f, s = partition cmp pivot axs
                Cons(a0, f), s
            | Cons (a0, axs) ->
                let f, s = partition cmp pivot axs
                f, Cons(a0, s)
            | _ -> Empty, Empty

        let rec qsort cmp a =
            match a with
            | Cons (a0, Empty) -> a
            | Cons (a0, axs) ->
                let firstHalf, secondHalf = partition cmp a0 axs

                match firstHalf, secondHalf with
                | Empty, nonempty -> Cons(a0, (qsort cmp nonempty))
                | nonempty, Empty -> ConCat(qsort cmp nonempty) (Cons(a0, Empty))
                | _ -> ConCat(qsort cmp firstHalf) (Cons(a0, (qsort cmp secondHalf)))
            | _ -> Empty

        qsort cmp a