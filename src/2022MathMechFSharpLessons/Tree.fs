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
    let rec GetSortedListAlpha cmp (tree: Tree<'elementType>) : FuncList.List<'elementType> =
        match tree with
        | Leaf (value) -> FuncList.Cons(value, FuncList.Empty)
        | Node (value, children) ->
            FuncList.Fold
                (fun accumulated child -> (SortedFuncList.FuseSorted cmp accumulated (GetSortedListAlpha cmp child)))
                (FuncList.Cons(value, FuncList.Empty))
                children

    module Heap =
        let rec Heapify cmp (tree: Tree<'elementType>) =
            match tree with
            | Leaf (value) -> Leaf(value)
            | Node (value, children) ->
                let heapifiedChildren, cmpMinVal =
                    FuncList.Fold
                        (fun (acc, accCmpMin) child ->
                            match Heapify cmp child with
                            | Leaf (childValue) when cmp childValue accCmpMin -> Cons(Leaf accCmpMin, acc), childValue
                            | Leaf (childValue) -> Cons(Leaf childValue, acc), accCmpMin
                            | Node (childValue, grandchildren) when cmp childValue accCmpMin ->
                                Cons((Heapify cmp (Node(accCmpMin, grandchildren))), acc), childValue
                            | Node (childValue, grandchildren) -> Cons(Node(childValue, grandchildren), acc), accCmpMin)
                        (Empty, value)
                        children

                Node(cmpMinVal, heapifiedChildren)

        let Fuse2Heaps cmp (heap1: Tree<'elementType>) (heap2: Tree<'elementType>) =
            match heap1, heap2 with
            | Leaf (val1), Leaf (val2) when cmp val1 val2 -> Node(val1, Cons(heap2, Empty))
            | Leaf (val1), Leaf (val2) -> Node(val2, Cons(heap1, Empty))
            | Node (val1, ch1), Leaf (val2) when cmp val1 val2 -> Node(val1, Cons(heap2, ch1))
            | Node (val1, ch1), Leaf (val2) -> Node(val2, Cons(Leaf(val1), ch1))
            | Leaf (val1), Node (val2, ch2) when cmp val1 val2 -> Node(val1, Cons(Leaf(val2), ch2))
            | Leaf (val1), Node (val2, ch2) -> Node(val2, Cons(heap1, ch2))
            | Node (val1, ch1), Node (val2, ch2) when cmp val1 val2 -> Node(val1, Cons(Leaf(val2), Concat ch1 ch2))
            | Node (val1, ch1), Node (val2, ch2) -> Node(val2, Cons(Leaf(val1), Concat ch1 ch2))

        let rec FuseHeaps cmp (heaps: List<Tree<'elementType>>) : Tree<'elementType> =
            match heaps with
            | Empty -> raise <| Exception("Can't fuse 0 heaps :(")
            | Cons (hd, Empty) -> hd
            | Cons (hd1, Cons (hd2, tl)) -> FuseHeaps cmp <| Cons(Fuse2Heaps cmp hd1 hd2, tl)

        let Pop cmp (heap: Tree<'elementType>) : Option<Tree<'elementType>> =
            match heap with
            | Leaf (_) -> None
            | Node (_, children) -> Some(FuseHeaps cmp children)

    // equal elements are erased, will rename this function if it will be final one
    let GetSortedListBeta cmp (tree: Tree<'elementType>) : FuncList.List<'elementType> =
        let rec getSortedWithout cmp (without: 'elementType) (heap: Tree<'elementType>) =
            match heap with
            | Leaf (value) when value = without -> Empty
            | Leaf (value) -> Cons(value, Empty)
            | Node (value, children) when value = without ->
                getSortedWithout cmp value (Option.get <| Heap.Pop cmp heap)
            | Node (value, children) -> Cons(value, getSortedWithout cmp value (Option.get <| Heap.Pop cmp heap))

        let sortedTree = Heap.Heapify cmp tree

        match sortedTree with
        | Leaf (value) -> Cons(value, Empty)
        | Node (value, children) -> Cons(value, getSortedWithout cmp (value) sortedTree)

    // counts different elements in tree, uses functions from module SortedFuncList
    let CountDifferentAlpha (tree: Tree<'elementType>) : int =
        GetLength(
            SortedFuncList.DeleteEqual
            <| GetSortedListAlpha(<) tree
        )

    // counts different elements in tree, uses functions from module Heap
    let CountDifferentBeta (tree: Tree<'elementType>) : int = GetLength(GetSortedListBeta(<) tree)
