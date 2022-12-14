namespace MathMechFSharpLessons

open System

module LinAlg =
    // Function to calculate 2**X, which is infinum of all 2**x >= n
    let _getMin2Pow (n: int) =
        let getRidOfZero (twoPow: int) (n: int) = n ||| (n >>> twoPow)

        (n - 1)
        |> getRidOfZero 1
        |> getRidOfZero 2
        |> getRidOfZero 4
        |> getRidOfZero 8
        |> getRidOfZero 16
        |> (+) 1

    // TODO:
    //  check preconditions: |arr| > 0, |arr.[i]| = |arr.[j]| > 0 for each i,j ONLY when flag debug is active (asserts)
    //  add postcondion-checks in tests
    //  make access to trees private (only wrappers shoud be visible from outside)

    //  This is not sparse at all, I still need to add relaxation XD

    type VectorBinTree<'elementType> =
        | VNode of leftSubtree: VectorBinTree<'elementType> * rightSubtree: VectorBinTree<'elementType> option
        | VLeaf of value: 'elementType option * compression: int // compression represents amout of elements compressed. It is always power of 2

    let rec vectorToList tree =
        match tree with
        | VLeaf (Some (value), compr) -> List.init compr (fun _ -> value)
        | VNode (l, None) -> vectorToList l
        | VNode (l, Some (r)) -> (vectorToList l) @ (vectorToList r)
        | _ -> []

    let rec listToVector<'elementType when 'elementType: equality>
        (normSize: int option)
        (lst: 'elementType list)
        : VectorBinTree<'elementType> =
        let normSize =
            if normSize.IsSome then
                normSize.Value
            else
                if lst.Length < 1 then
                    raise
                    <| new Exception("Can't create empty vector")

                _getMin2Pow <| lst.Length

        if normSize > 1 then
            let l = listToVector (Some(normSize / 2)) (lst.[.. (normSize / 2 - 1)])

            let r =
                if normSize / 2 < lst.Length then
                    listToVector (Some(normSize / 2)) lst.[(normSize / 2) ..]
                else
                    VLeaf(None, normSize / 2)

            match l, r with
            | VLeaf (v1, c1), VLeaf (v2, c2) when v1 = v2 && c1 = c2 -> VLeaf(v1, c1 * 2)
            | _, _ -> VNode(l, Some(r))
        else
            VLeaf(Some(lst.[0]), 1)

    type MatrixQuadTree<'elementType> =
        | MNode of
            leftTopSubtree: MatrixQuadTree<'elementType> *
            rightTopSubtree: MatrixQuadTree<'elementType> option *
            leftBottomSubtree: MatrixQuadTree<'elementType> option *
            rightBottomSubtree: MatrixQuadTree<'elementType> option
        | MLeaf of value: 'elementType option * compression: int // compression = side of a compressed square

    let rec matrixToList tree =
        match tree with
        | MLeaf (Some (value), compr) ->
            let inner = List.init compr (fun _ -> value)
            List.init compr (fun _ -> inner)
        | MLeaf (None, compr) -> []
        | MNode (lt, None, None, None) -> matrixToList lt
        | MNode (lt, Some (rt), None, None) ->
            let rightPart = matrixToList rt

            if rightPart = [] then
                matrixToList lt
            else
                List.map2 (fun a b -> a @ b) (matrixToList lt) rightPart
        | MNode (lt, None, Some (lb), None) -> (matrixToList lt) @ (matrixToList lb)
        | MNode (lt, Some (rt), Some (lb), Some (rb)) ->
            let rightPart = ((matrixToList rt) @ (matrixToList rb))

            if rightPart = [] then
                ((matrixToList lt) @ (matrixToList lb))
            else
                List.map2 (fun a b -> a @ b) ((matrixToList lt) @ (matrixToList lb)) rightPart
        | _ ->
            raise
            <| new Exception("Invalid tree state for a matrix")

    let rec listToMatrix<'elementType when 'elementType: equality>
        (normSizeH: int option)
        (normSizeW: int option)
        (lst: 'elementType list list)
        : MatrixQuadTree<'elementType> =
        let normSizeH =
            if normSizeH.IsSome then
                normSizeH.Value
            else
                if lst.Length < 1 then
                    raise
                    <| new Exception("Can't create empty matrix")

                _getMin2Pow <| lst.Length

        let normSizeW =
            if normSizeW.IsSome then
                normSizeW.Value
            else
                let lenW = lst.[0].Length

                if lenW < 1 then
                    raise
                    <| new Exception("Can't create empty matrix")

                if not
                   <| List.forall (fun line -> List.length line = lenW) lst then
                    raise
                    <| new Exception("Can't create matrix of inconsistent size")

                _getMin2Pow <| lenW

        if normSizeH > normSizeW then
            MNode(
                listToMatrix (Some(normSizeH / 2)) (Some(normSizeW)) lst.[.. (normSizeH / 2 - 1)],
                None,
                Some(listToMatrix (Some(normSizeH / 2)) (Some(normSizeW)) lst.[(normSizeH / 2) ..]),
                None
            )
        elif normSizeH < normSizeW then
            MNode(
                listToMatrix (Some(normSizeH)) (Some(normSizeW / 2))
                <| List.map (fun (line: 'elementType list) -> line.[.. (normSizeW / 2 - 1)]) lst,
                Some(
                    listToMatrix (Some(normSizeH)) (Some(normSizeW / 2))
                    <| List.map (fun (line: 'elementType list) -> line.[(normSizeW / 2) ..]) lst
                ),
                None,
                None
            )
        else // normSizeH = normSizeW
            let normSize = normSizeH

            if normSize > 1 then
                let lt =
                    listToMatrix (Some(normSize / 2)) (Some(normSize / 2))
                    <| List.map
                        (fun (line: 'elementType list) -> line.[.. (normSize / 2 - 1)])
                        (lst.[.. (normSize / 2 - 1)])

                let rt =
                    if normSize / 2 < lst.[0].Length then
                        listToMatrix (Some(normSize / 2)) (Some(normSize / 2))
                        <| List.map
                            (fun (line: 'elementType list) -> line.[(normSize / 2) ..])
                            (lst.[.. (normSize / 2 - 1)])
                    else
                        MLeaf(None, normSize / 2)

                let lb =
                    if normSize / 2 < lst.Length then
                        listToMatrix (Some(normSize / 2)) (Some(normSize / 2))
                        <| List.map
                            (fun (line: 'elementType list) -> line.[.. (normSize / 2 - 1)])
                            (lst.[(normSize / 2) ..])
                    else
                        MLeaf(None, normSize / 2)

                let rb =
                    if (normSize / 2 < lst.Length
                        && normSize / 2 < lst.[0].Length) then
                        listToMatrix (Some(normSize / 2)) (Some(normSize / 2))
                        <| List.map
                            (fun (line: 'elementType list) -> line.[(normSize / 2) ..])
                            (lst.[(normSize / 2) ..])
                    else
                        MLeaf(None, normSize / 2)

                match lt, rt, lb, rb with
                | MLeaf (v1, c1), MLeaf (v2, c2), MLeaf (v3, c3), MLeaf (v4, c4) when
                    (v1 = v2 && c1 = c2)
                    && (v2 = v3 && c2 = c3)
                    && (v3 = v4 && c3 = c4)
                    ->
                    MLeaf(v1, c1 * 2)
                | _, _, _, _ -> MNode(lt, Some(rt), Some(lb), Some(rb))
            else
                MLeaf(Some(lst.[0].[0]), 1)

    let rec transpose<'elementType> (tree: MatrixQuadTree<'elementType>) =
        match tree with
        | MNode (a00, Some (a01), Some (a10), Some (a11)) ->
            MNode(transpose a00, Some(transpose a10), Some(transpose a01), Some(transpose a11))
        | MNode (a00, None, Some (a10), None) -> MNode(transpose a00, Some(transpose a10), None, None)
        | MNode (a00, Some (a01), None, None) -> MNode(transpose a00, None, Some(transpose a01), None)
        | MNode (a00, None, None, None) -> MNode(transpose a00, None, None, None)
        | MNode _ ->
            raise
            <| new Exception("Invalid tree state for a matrix")
        | _ -> tree

    let rec multiplyMatrixOnVector<'elementType>
        (mat: MatrixQuadTree<'elementType>)
        (vec: VectorBinTree<'elementType>)
        : VectorBinTree<'elementType> =
        match mat, vec with
        | MNode (a00, None, None, None), VNode (v0, None) -> VNode(multiplyMatrixOnVector a00 v0, None)
        | MLeaf (Some (matVal), c1), VLeaf (Some (vecVal), c2) when c1 = c2 -> VLeaf(matVal * vecVal * c1, c1)
        | MNode (a00, Some (a01), None, None), VNode (v0, Some (v1)) ->
            VNode(
                (multiplyMatrixOnVector a00 v0)
                + (multiplyMatrixOnVector a01 v1),
                None
            )
        | MNode (a00, None, Some (a10), None), VNode (v0, None) ->
            VNode(multiplyMatrixOnVector a00 v0, multiplyMatrixOnVector a10 v0)
        | MNode (a00, Some (a01), Some (a10), Some (a11)), VNode (v0, Some (v1)) ->
            VNode(
                (multiplyMatrixOnVector a00 v0)
                + (multiplyMatrixOnVector a01 v1),
                (multiplyMatrixOnVector a10 v0)
                + (multiplyMatrixOnVector a11 v1)
            )
        | MLeaf (Some (matVal), c1), VNode (v0, Some (v1)) ->
            VNode(
                (multiplyMatrixOnVector (MLeaf(Some(matVal), c1 / 2)) v0)
                + (multiplyMatrixOnVector (MLeaf(Some(matVal), c1 / 2)) v1),
                (multiplyMatrixOnVector (MLeaf(Some(matVal), c1 / 2)) v0)
                + (multiplyMatrixOnVector (MLeaf(Some(matVal), c1 / 2)) v1)
            )
        | MNode (a00, Some (a01), None, None), VLeaf (Some (vecVal), c2) ->
            VNode(
                (multiplyMatrixOnVector a00 (VLeaf(Some(vecVal), c2 / 2)))
                + (multiplyMatrixOnVector a01 (VLeaf(Some(vecVal), c2 / 2))),
                None
            )
        | MNode (a00, Some (a01), Some (a10), Some (a11)), VLeaf (Some (vecVal), c2) ->
            VNode(
                (multiplyMatrixOnVector a00 (VLeaf(Some(vecVal), c2 / 2)))
                + (multiplyMatrixOnVector a01 (VLeaf(Some(vecVal), c2 / 2))),
                (multiplyMatrixOnVector a10 (VLeaf(Some(vecVal), c2 / 2)))
                + (multiplyMatrixOnVector a11 (VLeaf(Some(vecVal), c / 2)))
            )
        | _ ->
            raise
            <| new Exception("Failed to multiply matrix on vector")

    let multiplyVectorOnMatrix vec mat = 0

    // wrapper over VectorBinTree
    type Vector<'elementType when 'elementType: equality>(tree_: VectorBinTree<'elementType>, len: int) =
        struct
            member this.tree: VectorBinTree<'elementType> = tree_
            member this.length = len
            member this.list() = vectorToList this.tree

            member this.mult(other: Matrix<'elementType>) =
                if this.length != other.height then
                    raise
                    <| Exception("Can't multiply vector to matrix")

                multiplyVectorOnMatrix this.tree other.tree

            new(arr: 'elementType list) = new Vector<'elementType>(listToVector None arr, arr.Length)
        end

    // wrapper over MatrixQuadTree
    and Matrix<'elementType when 'elementType: equality>(tree_: MatrixQuadTree<'elementType>, tupleHW: int * int) =
        struct
            member this.tree: MatrixQuadTree<'elementType> = tree_
            member this.height = fst tupleHW
            member this.width = snd tupleHW
            member this.list() = matrixToList this.tree

            member this.mult(other: Vector<'elementType>) =
                if this.width != other.length then
                    raise
                    <| Exception("Can't multiply matrix to vector")

                multiplyMatrixOnVector this.tree other.tree

            member this.transpose() =
                Matrix(transpose this.tree, (this.height, this.width))

            new(arr: 'elementType list list) =
                new Matrix<'elementType>(
                    listToMatrix None None arr,
                    (arr.Length,
                     if arr.Length = 0 then
                         raise
                         <| Exception("Can't create matrix of zero size")

                     arr.[0].Length)
                )
        end
