namespace MathMechFSharpLessons

open System

module Tree =
    open FuncList

    type Tree<'elementType> =
        | Leaf of value: 'elementType
        | Node of value: 'elementType * children: List<Tree<'elementType>>

    let BinaryFromArray (arr: 'elementType []) : Tree<'elementType> =
        let rec indexedFromArray (arr: 'elementType []) (currentPos: int) =
            if 2 * currentPos + 2 < arr.Length then
                Node(
                    arr[currentPos],
                    Cons(
                        (indexedFromArray arr <| 2 * currentPos + 1),
                        Cons((indexedFromArray arr <| 2 * currentPos + 2), Empty)
                    )
                )
            elif 2 * currentPos + 1 < arr.Length then
                Node(arr[currentPos], Cons((indexedFromArray arr <| 2 * currentPos + 1), Empty))
            else
                Leaf(arr[currentPos])

        if arr.Length > 0 then
            indexedFromArray arr 0
        else
            raise
            <| Exception("Can't generate tree without elements")

    let rec Fold func acc (tree: Tree<'elementType>) =
        match tree with
        | Leaf (value) -> func acc value
        | Node (value, children) ->
            FuncList.Fold(fun accumulated child -> (Fold func accumulated child)) (func acc value) children

    let rec GetList (tree: Tree<'elementType>) : FuncList.List<'elementType> =
        Fold(fun acc value -> FuncList.Cons(value, acc)) FuncList.Empty tree

    // elements might be equal
    let rec GetSortedList cmp (tree: Tree<'elementType>) : FuncList.List<'elementType> =
        match tree with
        | Leaf (value) -> FuncList.Cons(value, FuncList.Empty)
        | Node (value, children) ->
            FuncList.Fold
                (fun accumulated child -> (SortedFuncList.FuseSorted cmp accumulated (GetSortedList cmp child)))
                (FuncList.Cons(value, FuncList.Empty))
                children

    let CountDifferent (tree: Tree<'elementType>) : int =
        GetLength(
            SortedFuncList.DeleteEqual
            <| GetSortedList(<) tree
        )
