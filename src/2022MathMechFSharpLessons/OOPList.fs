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
        inherit Exception("Unknown list type (OOPList)")

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

    let rec Concat<'value> (a: IList<'value>) (b: IList<'value>) =
        match a with
        | :? EmptyList<'value> -> b
        | :? NonEmptyList<'value> as lst -> NonEmptyList<'value>(lst.Head, (Concat lst.Tail b))
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

        let rec recursiveLoop cnt func lst =
            if cnt = 0 then
                lst
            else
                recursiveLoop (cnt - 1) func (func lst)

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
                        Concat(qsort cmp f)
                        <| NonEmptyList<'value>(pivot, EmptyList())
                    else
                        Concat(qsort cmp f) (NonEmptyList<'value>(pivot, qsort cmp s))
                | _ -> raise <| UnknownListTypeException()
            | :? EmptyList<'value> -> EmptyList<'value>()
            | _ -> raise <| UnknownListTypeException()

        interface IListSortAlgorithm<'value> with
            member this.sort (cmp: IComparer<'value>) (lst: IList<'value>) = qsort cmp lst
