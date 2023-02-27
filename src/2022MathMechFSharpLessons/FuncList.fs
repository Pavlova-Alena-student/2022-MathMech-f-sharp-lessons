namespace MathMechFSharpLessons

open System

module FuncList =
    // Functional implementation of list
    type List<'value> =
        | Cons of head: 'value * tail: List<'value>
        | Empty

    // For testing: insersion sort
    // expects cmp to be asymmetric
    let rec InsertionSort cmp lst =
        match lst with
        | Empty -> Empty
        | Cons (hd, Empty) -> lst
        | Cons (hd, tl) ->
            match (InsertionSort cmp tl) with
            | Cons (hd2, tl2) when not <| cmp hd hd2 -> Cons(hd2, InsertionSort cmp (Cons(hd, tl2)))
            | lst2 -> Cons(hd, lst2)

    let rec Concat lst1 lst2 =
        match lst1 with
        | Empty -> lst2
        | Cons (hd, tl) -> Cons(hd, Concat tl lst2)

    let rec GetLength lst =
        match lst with
        | Empty -> 0
        | Cons (_, rest) -> 1 + GetLength rest

    // expects cmp to be asymmetric
    let BubbleSort cmp lst =
        let rec RaiseBubble cmp lst =
            match lst with
            | Empty -> Empty
            | Cons (_, Empty) -> lst
            | Cons (hd, Cons (hd2, tl)) when not <| cmp hd hd2 -> Cons(hd2, RaiseBubble cmp (Cons(hd, tl)))
            | Cons (hd, Cons (hd2, tl)) -> Cons(hd, RaiseBubble cmp (Cons(hd2, tl)))

        let len = GetLength lst

        let rec recursiveLoop cnt func lst =
            if cnt = 0 then
                lst
            else
                recursiveLoop (cnt - 1) func (func lst)

        recursiveLoop len (RaiseBubble cmp) lst

    // expects cmp to be asymmetric
    let QuickSort cmp lst =
        let rec partition cmp pivot lst =
            match lst with
            | Cons (hd, tl) when not <| cmp pivot hd ->
                let f, s = partition cmp pivot tl
                Cons(hd, f), s
            | Cons (hd, tl) ->
                let f, s = partition cmp pivot tl
                f, Cons(hd, s)
            | _ -> Empty, Empty

        let rec qsort cmp lst =
            match lst with
            | Cons (hd, Empty) -> lst
            | Cons (hd, tl) ->
                let firstHalf, secondHalf = partition cmp hd tl

                match firstHalf, secondHalf with
                | Empty, nonempty -> Cons(hd, (qsort cmp nonempty))
                | nonempty, Empty -> Concat(qsort cmp nonempty) (Cons(hd, Empty))
                | _ -> Concat(qsort cmp firstHalf) (Cons(hd, (qsort cmp secondHalf)))
            | _ -> Empty

        qsort cmp lst

    let rec Fold func acc lst =
        match lst with
        | Cons (hd, tl) -> Fold func (func acc hd) tl
        | Empty -> acc

module SortedFuncList =
    open FuncList

    let rec FuseSorted cmp l1 l2 =
        match l1 with
        | Empty -> l2
        | Cons (hd1, tl1) ->
            match l2 with
            | Empty -> l1
            | Cons (hd2, tl2) when cmp hd1 hd2 -> Cons(hd1, FuseSorted cmp tl1 l2)
            | Cons (hd2, tl2) -> Cons(hd2, FuseSorted cmp l1 tl2)

    let rec DeleteEqual sortedList =
        match sortedList with
        | Empty -> sortedList
        | Cons (_, Empty) -> sortedList
        | Cons (hd1, Cons (hd2, tl)) when hd1 = hd2 -> DeleteEqual(Cons(hd1, tl))
        | Cons (hd, tl) -> Cons(hd, DeleteEqual tl)

    let rec Insert cmp sortedList element =
        match sortedList with
        | Empty -> Cons(element, Empty)
        | Cons (hd, _) when cmp element hd -> Cons(element, sortedList)
        | Cons (hd, tl) -> Cons(hd, Insert cmp tl element)
