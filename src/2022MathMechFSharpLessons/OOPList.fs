namespace _2022MathMechFSharpLessons

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

    let rec BubbleSort<'value> (cmp: IComparer<'value>) (lst: IList<'value>) =
        let rec RaiseBubble (cmp: IComparer<'value>) (lst: IList<'value>) : IList<'value> =
            match lst with
            | :? EmptyList<'value> -> EmptyList<'value>()
            | :? NonEmptyList<'value> as lst ->
                match lst.Tail with
                | :? EmptyList<'value> -> EmptyList<'value>()
                | :? NonEmptyList<'value> as tl ->
                    if not <| cmp.Compare lst.Head tl.Head then
                        NonEmptyList<'value>(tl.Head, RaiseBubble cmp (NonEmptyList<'value>(lst.Head, tl.Tail)))
                    else
                        NonEmptyList<'value>(lst.Head, RaiseBubble cmp tl)
                | _ -> raise <| new UnknownListTypeException()
            | _ -> raise <| new UnknownListTypeException()

        let len = GetLength lst

        let rec recursiveLoop n f a =
            if n = 1 then
                f a
            else
                recursiveLoop (n - 1) f (f a)

        recursiveLoop len (RaiseBubble cmp) lst

    let rec QuickSort<'value> (cmp: IComparer<'value>) (lst: IList<'value>) : IList<'value> =
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
                    let f, s = partition cmp pivot lst

                    let f, s =
                        if (f :? EmptyList<'value>) then
                            if not <| cmp.Compare(pivot + 1) pivot then
                                partition cmp (pivot + 1) lst
                            else
                                partition cmp (pivot - 1) lst
                        elif (s :? EmptyList<'value>) then
                            if not <| cmp.Compare pivot (pivot + 1) then
                                partition cmp (pivot + 1) lst
                            else
                                partition cmp (pivot - 1) lst
                        else
                            f, s

                    if (f :? EmptyList<'value>) then s
                    elif (s :? EmptyList<'value>) then f
                    else ConCat f s
                | _ -> raise <| UnknownListTypeException()
            | :? EmptyList<'value> -> EmptyList<'value>()
            | _ -> raise <| UnknownListTypeException()

        qsort cmp lst
