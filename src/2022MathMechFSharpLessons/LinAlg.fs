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
        | Node of leftSubtree: VectorBinTree<'elementType> * rightSubtree: VectorBinTree<'elementType> option
        | CompLeaf of value: 'elementType option * compression: int // compression represents amout of elements compressed. It is always power of 2
        | DataLeaf of value: 'elementType option

    let rec vectorToList tree =
        match tree with
        | DataLeaf (Some (value)) -> [ value ]
        | CompLeaf (Some (value), compr) -> List.init compr (fun _ -> value)
        | Node (l, None) -> vectorToList l
        | Node (l, Some (r)) -> (vectorToList l) @ (vectorToList r)
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
                if normSize / 2 > lst.Length then
                    listToVector (Some(normSize / 2)) lst.[(normSize / 2) ..]
                else
                    CompLeaf(None, normSize / 2)

            match l, r with
            | DataLeaf v1, DataLeaf v2 when v1 = v2 -> CompLeaf(v1, 2)
            | CompLeaf (v1, c1), CompLeaf (v2, c2) when v1 = v2 && c1 = c2 -> CompLeaf(v1, c1 * 2)
            | _, _ -> Node(l, Some(r))
        else
            DataLeaf(Some(lst.[0]))

    type MatrixQuadTree<'elementType> =
        | Node of
            leftTopSubtree: MatrixQuadTree<'elementType> *
            rightTopSubtree: MatrixQuadTree<'elementType> option *
            leftBottomSubtree: MatrixQuadTree<'elementType> option *
            rightBottomSubtree: MatrixQuadTree<'elementType> option
        | CompLeaf of value: 'elementType option * compression: int // compression = side of a compressed square
        | DataLeaf of value: 'elementType option

    let rec matrixToList tree =
        match tree with
        | DataLeaf (Some (value)) -> [ [ value ] ]
        | CompLeaf (Some (value), compr) ->
            let inner = List.init compr (fun _ -> value)
            List.init compr (fun _ -> inner)
        | Node (lt, None, None, None) -> matrixToList lt
        | Node (lt, Some (rt), None, None) -> (matrixToList lt) @ (matrixToList rt)
        | Node (lt, None, Some (lb), None) -> List.map2 (fun a b -> a @ b) (matrixToList lt) (matrixToList lb)
        | Node (lt, Some (rt), Some (lb), Some (rb)) ->
            List.map2 (fun a b -> a @ b) ((matrixToList lt) @ (matrixToList rt)) ((matrixToList lb) @ (matrixToList rb))
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
                    <| new Exception("Can't create empty vector")

                _getMin2Pow <| lst.Length

        let normSizeW =
            if normSizeW.IsSome then
                normSizeW.Value
            else
                let lenW = lst.[0].Length

                if lenW < 1 then
                    raise
                    <| new Exception("Can't create empty vector")

                if not
                   <| List.forall (fun line -> List.length line = lenW) lst then
                    raise
                    <| new Exception("Can't create matrix of inconsistent size")

                _getMin2Pow <| lenW

        if normSizeH > normSizeW then
            Node(
                listToMatrix (Some(normSizeH / 2)) (Some(normSizeW)) lst.[.. (normSizeH / 2 - 1)],
                Some(listToMatrix (Some(normSizeH / 2)) (Some(normSizeW)) lst.[(normSizeH / 2) ..]),
                None,
                None
            )
        elif normSizeH < normSizeW then
            Node(
                listToMatrix (Some(normSizeH)) (Some(normSizeW / 2))
                <| List.map (fun (line: 'elementType list) -> line.[.. (normSizeW / 2 - 1)]) lst,
                None,
                Some(
                    listToMatrix (Some(normSizeH)) (Some(normSizeW / 2))
                    <| List.map (fun (line: 'elementType list) -> line.[(normSizeW / 2) ..]) lst
                ),
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
                    if normSize / 2 > lst.[0].Length then
                        listToMatrix (Some(normSize / 2)) (Some(normSize / 2))
                        <| List.map
                            (fun (line: 'elementType list) -> line.[(normSize / 2) ..])
                            (lst.[.. (normSize / 2 - 1)])
                    else
                        CompLeaf(None, normSize / 2)

                let lb =
                    if normSize / 2 > lst.Length then
                        listToMatrix (Some(normSize / 2)) (Some(normSize / 2))
                        <| List.map
                            (fun (line: 'elementType list) -> line.[.. (normSize / 2 - 1)])
                            (lst.[(normSize / 2) ..])
                    else
                        CompLeaf(None, normSize / 2)

                let rb =
                    if (normSize / 2 > lst.Length
                        && normSize / 2 > lst.[0].Length) then
                        listToMatrix (Some(normSize / 2)) (Some(normSize / 2))
                        <| List.map
                            (fun (line: 'elementType list) -> line.[(normSize / 2) ..])
                            (lst.[(normSize / 2) ..])
                    else
                        CompLeaf(None, normSize / 2)

                match lt, rt, lb, rb with
                | DataLeaf v1, DataLeaf v2, DataLeaf v3, DataLeaf v4 when v1 = v2 && v2 = v3 && v3 = v4 ->
                    CompLeaf(v1, 2)
                | CompLeaf (v1, c1), CompLeaf (v2, c2), CompLeaf (v3, c3), CompLeaf (v4, c4) when
                    (v1 = v2 && c1 = c2)
                    && (v2 = v3 && c2 = c3)
                    && (v3 = v4 && c3 = c4)
                    ->
                    CompLeaf(v1, c1 * 2)
                | _, _, _, _ -> Node(lt, Some(rt), Some(lb), Some(rb))
            else
                DataLeaf(Some(lst.[0].[0]))

    let rec transpose<'elementType> (tree: MatrixQuadTree<'elementType>) =
        match tree with
        | Node (a00, Some (a01), Some (a10), Some (a11)) ->
            Node(transpose a00, Some(transpose a10), Some(transpose a01), Some(transpose a11))
        | Node (a00, None, Some (a10), None) -> Node(transpose a00, Some(transpose a10), None, None)
        | Node (a00, Some (a01), None, None) -> Node(transpose a00, None, Some(transpose a01), None)
        | Node (a00, None, None, None) -> Node(transpose a00, None, None, None)
        | Node _ ->
            raise
            <| new Exception("Invalid tree state for a matrix")
        | _ -> tree

    // wrapper over VectorBinTree
    type Vector<'elementType when 'elementType: equality>(tree_: VectorBinTree<'elementType>) =
        struct
            member this.tree: VectorBinTree<'elementType> = tree_
            member this.list = vectorToList this.tree

            new(arr: 'elementType list) = new Vector<'elementType>(listToVector None arr)
        end

    // wrapper over MatrixQuadTree
    type Matrix<'elementType when 'elementType: equality>(tree_: MatrixQuadTree<'elementType>) =
        struct
            member this.tree: MatrixQuadTree<'elementType> = tree_
            member this.list = matrixToList this.tree
            member this.transpose = Matrix(transpose this.tree)

            new(arr: 'elementType list list) = new Matrix<'elementType>(listToMatrix None None arr)
        end
