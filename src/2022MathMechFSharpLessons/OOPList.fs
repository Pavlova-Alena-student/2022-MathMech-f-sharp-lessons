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

    let rec ConCat<'value> (a: IList<'value>) (b: IList<'value>) =
        match a with
        | :? EmptyList<'value> -> b
        | :? NonEmptyList<'value> as lst -> NonEmptyList<'value>(lst.Head, (ConCat lst.Tail b))
        | _ -> raise <| new UnknownListTypeException()

    // pages 193-194 from Kris Smit
    type IComparer<'value> =
        interface
            abstract member Compare: ('value -> 'value) -> int
        end

    // send help
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
                        1
                    else
                        RaiseBubble cmp lst
                | _ -> raise <| new UnknownListTypeException()
            | _ -> raise <| new UnknownListTypeException()

        RaiseBubble cmp lst // TODO: for

//[<AbstractClass>]
(*type IMyOOPListSortAlgorithm<'value> =
        interface
            abstract member op_LParenRParen: (IComparer<'value> * IList<'value>) -> bool
        end

    type MyOOPBubbleSort<'value> =
        interface IMyOOPListSortAlgorithm<'value> with
            member this.op_LParenRParen(cmp, lst) = true

    type MyOOPQuickSort<'value> =
        interface IMyOOPListSortAlgorithm<'value> with
            member this.op_LParenRParen(cmp, lst) = true

    type MyOOPConCat<'value> =
        member this.op_LParenRParen first second = first*)
