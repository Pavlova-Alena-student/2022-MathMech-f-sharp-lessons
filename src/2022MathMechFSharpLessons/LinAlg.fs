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
        | VNode of leftSubtree: VectorBinTree<'elementType> * rightSubtree: VectorBinTree<'elementType>
        | VLeaf of value: 'elementType option * compression: int // compression represents amout of elements compressed. It is always power of 2

    let rec vectorToList tree =
        match tree with
        | VLeaf (Some (value), compr) -> List.init compr (fun _ -> value)
        | VNode (l, r) -> (vectorToList l) @ (vectorToList r)
        | _ -> []

    // both of these map-s expects "func" to handle operations on 'elementType option
    let rec vectorMap func tree =
        match tree with
        | VLeaf (value, compr) -> VLeaf(func value, compr)
        | VNode (l, r) -> VNode(vectorMap func l, vectorMap func r)

    let rec vectorMap2 func tree1 tree2 =
        match tree1, tree2 with
        | VLeaf (v1, c1), VLeaf (v2, c2) when c1 = c2 -> VLeaf(func v1 v2, c1)
        | VNode (l1, r1), VNode (l2, r2) -> VNode(vectorMap2 func l1 l2, vectorMap2 func r1 r2)
        | VLeaf (v1, c1), VNode (l2, r2) ->
            VNode(vectorMap2 func (VLeaf(v1, c1 / 2)) l2, vectorMap2 func (VLeaf(v1, c1 / 2)) r2)
        | VNode (l1, r1), VLeaf (v2, c2) ->
            VNode(vectorMap2 func l1 (VLeaf(v2, c2 / 2)), vectorMap2 func r1 (VLeaf(v2, c2 / 2)))
        | _ ->
            raise
            <| Exception("Can't map2 with vectors of different sizes")

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
            | _, _ -> VNode(l, r)
        else
            VLeaf(Some(lst.[0]), 1)

    type MatrixQuadTree<'elementType> =
        | MNode of
            leftTopSubtree: MatrixQuadTree<'elementType> *
            rightTopSubtree: MatrixQuadTree<'elementType> *
            leftBottomSubtree: MatrixQuadTree<'elementType> *
            rightBottomSubtree: MatrixQuadTree<'elementType>
        | MLeaf of value: 'elementType option * compression: int // compression = side of a compressed square

    let rec matrixToList tree =
        match tree with
        | MLeaf (Some (value), compr) ->
            let inner = List.init compr (fun _ -> value)
            List.init compr (fun _ -> inner)
        | MLeaf (None, compr) -> []
        | MNode (lt, rt, lb, rb) ->
            let rightPart = ((matrixToList rt) @ (matrixToList rb))

            if rightPart = [] then
                ((matrixToList lt) @ (matrixToList lb))
            else
                List.map2 (fun a b -> a @ b) ((matrixToList lt) @ (matrixToList lb)) rightPart

    (*
        | _ ->
            raise
            <| new Exception("Invalid tree state for a matrix")*)

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
                MLeaf(None, normSizeH / 2),
                listToMatrix (Some(normSizeH / 2)) (Some(normSizeW)) lst.[(normSizeH / 2) ..],
                MLeaf(None, normSizeH / 2)
            )
        elif normSizeH < normSizeW then
            MNode(
                listToMatrix (Some(normSizeH)) (Some(normSizeW / 2))
                <| List.map (fun (line: 'elementType list) -> line.[.. (normSizeW / 2 - 1)]) lst,
                listToMatrix (Some(normSizeH)) (Some(normSizeW / 2))
                <| List.map (fun (line: 'elementType list) -> line.[(normSizeW / 2) ..]) lst,
                MLeaf(None, normSizeW / 2),
                MLeaf(None, normSizeW / 2)
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
                | _, _, _, _ -> MNode(lt, rt, lb, rb)
            else
                MLeaf(Some(lst.[0].[0]), 1)

    let rec transpose<'elementType> (tree: MatrixQuadTree<'elementType>) =
        match tree with
        | MNode (a00, a01, a10, a11) -> MNode(transpose a00, transpose a10, transpose a01, transpose a11)
        | _ -> tree

    let rec multMatVec<'matType, 'vecType, 'resType>
        (fAdd: 'resType option -> 'resType option -> 'resType option)
        (fMult: 'matType option -> 'vecType option -> int -> 'resType option)
        (mat: MatrixQuadTree<'matType>)
        (vec: VectorBinTree<'vecType>)
        : VectorBinTree<'resType> =
        match mat, vec with
        | MLeaf (matVal, c1), VLeaf (vecVal, c2) when c1 = c2 -> VLeaf(fMult matVal vecVal (int c1), c1)
        | MNode (a00, a01, a10, a11), VNode (v0, v1) ->
            let a00v0 = multMatVec fAdd fMult a00 v0
            let a01v1 = multMatVec fAdd fMult a01 v1

            let l =
                match a01v1 with
                | VLeaf (None, _) -> a00v0
                | _ -> vectorMap2 fAdd a00v0 a01v1

            let a10v0 = multMatVec fAdd fMult a10 v0
            let a11v1 = multMatVec fAdd fMult a11 v1

            let r =
                match a11v1 with
                | VLeaf (None, _) -> a10v0
                | _ -> vectorMap2 fAdd a10v0 a11v1

            VNode(l, r)
        | MLeaf (matVal, c1), VNode (v0, v1) ->
            let a00v0 = multMatVec fAdd fMult (MLeaf(matVal, c1 / 2)) v0
            let a01v1 = multMatVec fAdd fMult (MLeaf(matVal, c1 / 2)) v1
            let a10v0 = a00v0
            let a11v1 = a01v1
            VNode(vectorMap2 fAdd a00v0 a01v1, vectorMap2 fAdd a10v0 a11v1)
        | MNode (a00, a01, a10, a11), VLeaf (vecVal, c2) ->
            multMatVec fAdd fMult mat (VNode((VLeaf(vecVal, c2 / 2)), (VLeaf(vecVal, c2 / 2))))
        (*let a00v0 = multMatVec fAdd fMult a00 (VLeaf(Some(vecVal), c2 / 2))
            let a10v0 = multMatVec fAdd fMult a10 (VLeaf(Some(vecVal), c2 / 2))
            let l = match a01v1 with
                    | VLeaf (None, _) -> a00v0
                    | _ -> vectorMap2 fAdd a00v0 a01v1
            let a01v1 = multMatVec fAdd fMult a01 (VLeaf(Some(vecVal), c2 / 2))
            let a11v1 = multMatVec fAdd fMult a11 (VLeaf(Some(vecVal), c2 / 2))
            let r = match a11v1 with
                    | VLeaf (None, _) -> a10v0
                    | _ -> vectorMap2 fAdd a10v0 a11v1
            VNode(vectorMap2 fAdd a00v0 a01v1, vectorMap2 fAdd a10v0 a11v1)*)
        | _ ->
            raise
            <| new Exception("Failed to multiply matrix on vector")

    let multVecMat fAdd fMult vec mat =
        multMatVec (Utils.flip fAdd) (Utils.flip fMult) (transpose mat) vec

    let advancedAdd fAdd a b =
        match a, b with
        | Some (a), Some (b) -> Some(fAdd a b)
        | _ -> None

    let advancedMult fAdd fMult a b cnst2pow = // multiplication a * b and the result multiplied on 2^const from integers
        let rec multiply a cnst =
            if cnst = 1 then
                a
            else
                multiply (fAdd a a) (cnst / 2)

        match a, b with
        | Some (a), Some (b) -> Some(multiply (fMult a b) cnst2pow)
        | _ -> None

    // wrapper over VectorBinTree
    type Vector<'elementType when 'elementType: equality>(tree_: VectorBinTree<'elementType>, len: int) =
        struct
            member this.tree: VectorBinTree<'elementType> = tree_
            member this.length = len
            member this.list() = vectorToList this.tree

            member this.mult
                (fAdd: 'resType -> 'resType -> 'resType)
                (fMult: 'elementType -> 'otherType -> 'resType)
                (other: Matrix<'otherType>)
                : Vector<'resType> =
                if this.length <> other.height then
                    raise
                    <| Exception("Can't multiply vector to matrix")

                Vector<'resType>(
                    multVecMat (advancedAdd fAdd) (advancedMult fAdd fMult) this.tree other.tree,
                    other.width
                )

            new(arr: 'elementType list) = new Vector<'elementType>(listToVector None arr, arr.Length)
        end

    // wrapper over MatrixQuadTree
    and Matrix<'elementType when 'elementType: equality>(tree_: MatrixQuadTree<'elementType>, tupleHW: int * int) =
        struct
            member this.tree: MatrixQuadTree<'elementType> = tree_
            member this.height: int = fst tupleHW
            member this.width: int = snd tupleHW
            member this.list() = matrixToList this.tree

            member this.mult
                (fAdd: 'resType -> 'resType -> 'resType)
                (fMult: 'elementType -> 'otherType -> 'resType)
                (other: Vector<'otherType>)
                : Vector<'resType> =
                if this.width <> other.length then
                    raise
                    <| Exception("Can't multiply matrix to vector: sizes don't match")

                Vector<'resType>(
                    multMatVec (advancedAdd fAdd) (advancedMult fAdd fMult) this.tree other.tree,
                    this.height
                )

            member this.transpose() =
                Matrix(transpose this.tree, (this.width, this.height))

            new(arr: 'elementType list list) =
                new Matrix<'elementType>(
                    listToMatrix None None arr,
                    (arr.Length,
                     if arr.Length = 0 then
                         raise
                         <| Exception("Can't create matrix of zero size")
                     else
                         arr.[0].Length)
                )
        end
