namespace MathMechFSharpLessons

open System

module Tree =
    type Tree<'elementType> =
        | Leaf of value: 'elementType
        | Node of value: 'elementType * children: Tree<'elementType> []

    let rec getSortedList cmp (tree: Tree<'elementType>) : FuncList.List<'elementType> =
        match tree with
        | Leaf (value) -> FuncList.Cons(value, FuncList.Empty)
        | Node (value, children) ->
            Array.fold
                (fun accumulated child -> (FuncList.fuseSorted cmp accumulated (getSortedList cmp child)))
                (FuncList.Cons(value, FuncList.Empty))
                children

    let getList tree = getSortedList (<) tree

    let countDifferent (tree: Tree<'elementType>) : int = FuncList.GetLength(getList tree)
