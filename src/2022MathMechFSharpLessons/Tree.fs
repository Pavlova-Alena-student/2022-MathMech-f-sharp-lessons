namespace MathMechFSharpLessons

open System

module Tree =
    type Tree<'elementType> =
        | Leaf of value: 'elementType
        | Node of value: 'elementType * children: Tree<'elementType> []

    let GenerateBinTree (arr: 'elementType []) : Tree<'elementType> =
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

    let rec genRangedTree eachDegree depth valueGenerator =
        if depth <= 1 then
            Leaf(valueGenerator depth)
        else
            Node(
                valueGenerator depth,
                Array.create eachDegree
                <| genRangedTree eachDegree (depth - 1) valueGenerator
            )

    let rec getSortedList cmp (tree: Tree<'elementType>) : FuncList.List<'elementType> =
        match tree with
        | Leaf (value) -> FuncList.Cons(value, FuncList.Empty)
        | Node (value, children) ->
            Array.fold
                (fun accumulated child -> (FuncList.fuseSorted cmp accumulated (getSortedList cmp child)))
                (FuncList.Cons(value, FuncList.Empty))
                children

    let getListOfDifferent tree =
        FuncList.deleteEqual (getSortedList (<) tree)

    let countDifferent (tree: Tree<'elementType>) : int =
        FuncList.GetLength(getListOfDifferent tree)
