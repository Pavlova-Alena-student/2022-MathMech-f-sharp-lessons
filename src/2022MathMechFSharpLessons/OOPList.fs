namespace MathMechFSharpLessons

open System

module OOPList =
    // OOP implementation of list
    type IList<'value> =
        interface
        end

    type NonEmptyList<'value>(head: 'value, tail: IList<'value>) =
        interface IList<'value>
        member this.Head = head
        member this.Tail = tail

    type EmptyList<'value>() =
        interface IList<'value>

    type UnknownListTypeException() =
        inherit Exception("Unknown list type")

    /// Generates a list. Generator should take one argument - reversed no. of current element and return an element on that place
    let rec GenerateList (rGenerator: int -> int) (length: int) : IList<int> =
        if length = 0 then
            EmptyList<int>()
        else
            NonEmptyList<int>(rGenerator length, GenerateList rGenerator (length - 1))

    /// Generates a list of random integer numbers from -500 to 499
    let RandList (length: int) =
        let rng = new Random()
        GenerateList(fun _ -> rng.Next() % 1000 - 500) length

    let rec GetLength (lst: IList<'value>) =
        match lst with
        | :? EmptyList<'value> -> 0
        | :? NonEmptyList<'value> as lst -> 1 + GetLength(lst.Tail)
        | _ -> raise <| UnknownListTypeException()

    let rec Compare<'value when 'value: equality> (f: IList<'value>) (s: IList<'value>) : bool =
        match f with
        | :? EmptyList<'value> -> (s :? EmptyList<'value>)
        | :? NonEmptyList<'value> as f ->
            (s :? NonEmptyList<'value>)
            && (f.Head = (s :?> NonEmptyList<'value>).Head)
            && (Compare f.Tail (s :?> NonEmptyList<'value>).Tail)
        | _ -> false

    let rec ConCat<'value> (a: IList<'value>) (b: IList<'value>) =
        match a with
        | :? EmptyList<'value> -> b
        | :? NonEmptyList<'value> as lst -> NonEmptyList<'value>(lst.Head, (ConCat lst.Tail b))
        | _ -> raise <| new UnknownListTypeException()

    // pages 193-194 from Kris Smit
    type IComparer<'value> =
        interface
            abstract member Compare: 'value -> 'value -> bool
        end

    type IListSortAlgorithm<'value> =
        interface
            abstract member sort: IComparer<'value> -> IList<'value> -> IList<'value>
        end

    type BubbleSort<'value>() =
        let rec RaiseBubble (cmp: IComparer<'value>) (lst: IList<'value>) : IList<'value> =
            match lst with
            | :? EmptyList<'value> -> EmptyList<'value>()
            | :? NonEmptyList<'value> as lst ->
                match lst.Tail with
                | :? EmptyList<'value> -> lst
                | :? NonEmptyList<'value> as tl ->
                    if not <| cmp.Compare lst.Head tl.Head then
                        NonEmptyList<'value>(tl.Head, RaiseBubble cmp (NonEmptyList<'value>(lst.Head, tl.Tail)))
                    else
                        NonEmptyList<'value>(lst.Head, RaiseBubble cmp tl)
                | _ -> raise <| new UnknownListTypeException()
            | _ -> raise <| new UnknownListTypeException()

        let rec recursiveLoop n f a =
            if n = 0 then
                a
            else
                recursiveLoop (n - 1) f (f a)

        interface IListSortAlgorithm<'value> with
            member this.sort (cmp: IComparer<'value>) (lst: IList<'value>) =
                recursiveLoop (GetLength lst) (RaiseBubble cmp) lst

    type QuickSort<'value>() =
        let rec partition
            (cmp: IComparer<'value>)
            (pivot: 'value)
            (lst: IList<'value>)
            : IList<'value> * IList<'value> =
            match lst with
            | :? NonEmptyList<'value> as lst when not <| cmp.Compare pivot lst.Head ->
                let f, s = partition cmp pivot lst.Tail
                NonEmptyList<'value>(lst.Head, f), s
            | :? NonEmptyList<'value> as lst ->
                let f, s = partition cmp pivot lst.Tail
                f, NonEmptyList<'value>(lst.Head, s)
            | :? EmptyList<'value> -> EmptyList<'value>(), EmptyList<'value>()
            | _ -> raise <| UnknownListTypeException()

        let rec qsort (cmp: IComparer<'value>) (lst: IList<'value>) : IList<'value> =
            match lst with
            | :? NonEmptyList<'value> as lst ->
                match lst.Tail with
                | :? EmptyList<'value> -> lst
                | :? NonEmptyList<'value> as tl ->
                    let pivot = lst.Head
                    let f, s = partition cmp pivot (lst.Tail)

                    if (f :? EmptyList<'value>) then
                        NonEmptyList<'value>(pivot, qsort cmp s)
                    elif (s :? EmptyList<'value>) then
                        ConCat(qsort cmp f)
                        <| NonEmptyList<'value>(pivot, EmptyList())
                    else
                        ConCat(qsort cmp f) (NonEmptyList<'value>(pivot, qsort cmp s))
                | _ -> raise <| UnknownListTypeException()
            | :? EmptyList<'value> -> EmptyList<'value>()
            | _ -> raise <| UnknownListTypeException()

        interface IListSortAlgorithm<'value> with
            member this.sort (cmp: IComparer<'value>) (lst: IList<'value>) = qsort cmp lst
