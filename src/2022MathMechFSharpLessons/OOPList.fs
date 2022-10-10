namespace _2022MathMechFSharpLessons

open System

module OOPList =
    // OOP implementation of list
    [<CustomEquality>]
    type IList<'value when 'value: equality> =
        interface
        end

    type NonEmptyList<'value>(head: 'value, tail: IList<'value>) =
        interface IList<'value>
        member this.Head = head
        member this.Tail = tail

        override this.GetHashCode() : int =
            this.Head.GetHashCode() * 3
            + this.Tail.GetHashCode() * 103

        override this.Equals<'value>(other) =
            match other with
            | :? NonEmptyList<'value> as other ->
                (this.Head = other.Head)
                & (this.Tail = other.Tail)
            | _ -> false

    type EmptyList<'value>() =
        interface IList<'value>

    (*
        override this.GetHashCode() = 0

        override this.Equals(other) =
            match other with
            | :? EmptyList<'value> -> true
            | _ -> false
*)

    type UnknownListTypeException() =
        inherit Exception("Unknown list type")

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

    let rec BubbleSort<'value> (cmp: IComparer<'value>) (lst: IList<'value>) =
        let rec RaiseBubble (cmp: IComparer<'value>) (lst: IList<'value>) =
            match lst with
            | :? EmptyList<'value> -> 0
            | :? NonEmptyList<'value> as lst ->
                match lst.Tail with
                | :? EmptyList<'value> -> 0
                | :? NonEmptyList<'value> as tl ->
                    if cmp.Compare lst.Head tl.Head then
                        RaiseBubble cmp (NonEmptyList<'value>(tl.Head, NonEmptyList<'value>(lst.Head, tl.Tail)))
                        |> ignore

                        1
                    else
                        RaiseBubble cmp lst
                | _ -> raise <| new UnknownListTypeException()
            | _ -> raise <| new UnknownListTypeException()

        while RaiseBubble cmp lst = 1 do
            ignore 0


    let rec QuickSort<'value> (cmp: IComparer<'value>) (lst: IList<'value>) : IList<'value> =
        let rec partition
            (cmp: IComparer<'value>)
            (pivot: 'value)
            (lst: IList<'value>)
            : IList<'value> * IList<'value> =
            match lst with
            | :? NonEmptyList<'value> as lst when cmp.Compare pivot lst.Head ->
                let f, s = partition cmp pivot lst.Tail
                NonEmptyList<'value>(lst.Head, f), s
            | :? NonEmptyList<'value> as lst ->
                let f, s = partition cmp pivot lst.Tail
                f, NonEmptyList<'value>(lst.Head, s)
            | _ -> EmptyList<'value>(), EmptyList<'value>()

        let rec qsort (cmp: IComparer<'value>) (lst: IList<'value>) : IList<'value> =
            match lst with
            | :? NonEmptyList<'value> as lst ->
                match lst.Tail with
                | :? EmptyList<'value> -> lst
                | :? NonEmptyList<'value> as tl -> EmptyList<'value>()
            | _ -> EmptyList<'value>()

        qsort cmp lst

(* Hmm... TODO!
    [<AbstractClass>]
    type IListSortAlgorithm<'value> =
        interface
            abstract member op_LParenRParen: (IComparer<'value> * IList<'value>) -> bool
        end

    type BubbleSort<'value> =
        interface IListSortAlgorithm<'value> with
            member this.op_LParenRParen(cmp, lst) = true
*)
