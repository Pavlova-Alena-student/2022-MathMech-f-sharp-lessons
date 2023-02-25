namespace MathMechFSharpLessons

open System

module Tree =
    type Tree<'elementType> =
        | Leaf of value: 'elementType
        | Node of value: 'elementType * children: Tree<'elementType> []

    let GenerateBinTreeFromArray (arr: 'elementType []) : Tree<'elementType> =
        let rec gen (arr: 'elementType []) (currentPos: int) =
            if 2 * currentPos + 2 < arr.Length then
                Node(
                    arr[currentPos],
                    [| (gen arr <| 2 * currentPos + 1)
                       (gen arr <| 2 * currentPos + 2) |]
                )
            elif 2 * currentPos + 1 < arr.Length then
                Node(arr[currentPos], [| (gen arr <| 2 * currentPos + 1) |])
            else
                Leaf(arr[currentPos])

        if arr.Length > 0 then
            gen arr 0
        else
            raise
            <| Exception("Can't generate tree without elements")

    let rec GetSortedList cmp (tree: Tree<'elementType>) : FuncList.List<'elementType> =
        match tree with
        | Leaf (value) -> FuncList.Cons(value, FuncList.Empty)
        | Node (value, children) ->
            Array.fold
                (fun accumulated child -> (FuncList.FuseSorted cmp accumulated (GetSortedList cmp child)))
                (FuncList.Cons(value, FuncList.Empty))
                children

    let GetListOfDifferent tree =
        FuncList.DeleteEqual(GetSortedList(<) tree)

    let GetList tree = GetListOfDifferent tree

    let CountDifferent (tree: Tree<'elementType>) : int =
        FuncList.GetLength(GetListOfDifferent tree)
