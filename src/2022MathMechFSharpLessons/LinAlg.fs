namespace MathMechFSharpLessons

open System

module LinAlg =
    exception TensorMultiplicationException of string
    exception TensorConversionFromListException of string

    // Function to calculate 2**X, which is infinum of all 2**x >= n
    let private _getMin2Pow (n: int) =
        let getRidOfZero (twoPow: int) (n: int) = n ||| (n >>> twoPow)

        (n - 1)
        |> getRidOfZero 1
        |> getRidOfZero 2
        |> getRidOfZero 4
        |> getRidOfZero 8
        |> getRidOfZero 16
        |> (+) 1

    type internal VectorBinTree<'elementType> =
        | VNode of leftSubtree: VectorBinTree<'elementType> * rightSubtree: VectorBinTree<'elementType>
        | VLeaf of value: 'elementType option * compression: int // compression represents amount of elements compressed. It is always power of 2

    let rec private vectorToList tree =
        match tree with
        | VLeaf (Some (value), compr) -> List.init compr (fun _ -> value)
        | VNode (l, r) -> (vectorToList l) @ (vectorToList r)
        | _ -> []

    let private relaxVector<'elementType when 'elementType: equality>
        (left: VectorBinTree<'elementType>)
        (right: VectorBinTree<'elementType>)
        =
        match left, right with
        | VLeaf (lv, lc), VLeaf (rv, rc) when lc = rc && lv = rv -> VLeaf(lv, lc * 2)
        | _ -> VNode(left, right)

    // Deletes None-s from end
    let rec private trimVector tree =
        match tree with
        | VNode (t, VLeaf (None, _)) -> trimVector t
        | _ -> tree

    // Opposite to trimVector, adds None-s to the end until nsize <= current size
    let private extendVector nsize size tree =
        let rec extendVector nsize size tree =
            if size < nsize then
                extendVector nsize (size * 2) (VNode(tree, VLeaf(None, size)))
            else
                tree

        extendVector (_getMin2Pow nsize) (_getMin2Pow size) tree

    // both of these map-s expects "func" to handle operations on 'elementType option
    let rec private vectorMap func tree =
        match tree with
        | VLeaf (value, compr) -> VLeaf(func value, compr)
        | VNode (l, r) -> relaxVector (vectorMap func l) (vectorMap func r)

    let rec private vectorMap2 func tree1 tree2 =
        match tree1, tree2 with
        | VLeaf (v1, c1), VLeaf (v2, c2) when c1 = c2 -> VLeaf(func v1 v2, c1)
        | VNode (l1, r1), VNode (l2, r2) -> relaxVector (vectorMap2 func l1 l2) (vectorMap2 func r1 r2)
        | VLeaf (v1, c1), VNode (l2, r2) ->
            relaxVector (vectorMap2 func (VLeaf(v1, c1 / 2)) l2) (vectorMap2 func (VLeaf(v1, c1 / 2)) r2)
        | VNode (l1, r1), VLeaf (v2, c2) ->
            relaxVector (vectorMap2 func l1 (VLeaf(v2, c2 / 2))) (vectorMap2 func r1 (VLeaf(v2, c2 / 2)))
        | _ ->
            raise // FIXME: name the exception?
            <| Exception("Can't map2 with vectors of different sizes")

    let rec private listToVector<'elementType when 'elementType: equality>
        (normSize: int option)
        (lst: 'elementType list)
        : VectorBinTree<'elementType> =
        let normSize =
            if normSize.IsSome then
                normSize.Value
            else
                _getMin2Pow <| lst.Length

        if normSize > 1 then
            let l = listToVector (Some(normSize / 2)) (lst.[.. (normSize / 2 - 1)])

            let r =
                if normSize / 2 < lst.Length then
                    listToVector (Some(normSize / 2)) lst.[(normSize / 2) ..]
                else
                    VLeaf(None, normSize / 2)

            relaxVector l r
        else if lst.Length = 0 then
            VLeaf(None, 1)
        else
            VLeaf(Some(lst.[0]), 1)

    type internal MatrixQuadTree<'elementType> =
        | MNode of
            leftTopSubtree: MatrixQuadTree<'elementType> *
            rightTopSubtree: MatrixQuadTree<'elementType> *
            leftBottomSubtree: MatrixQuadTree<'elementType> *
            rightBottomSubtree: MatrixQuadTree<'elementType>
        | MLeaf of value: 'elementType option * compression: int // compression = side of a compressed square

    let rec private matrixToList tree =
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

    let private relaxMatrix<'elementType when 'elementType: equality>
        (lt: MatrixQuadTree<'elementType>)
        (rt: MatrixQuadTree<'elementType>)
        (lb: MatrixQuadTree<'elementType>)
        (rb: MatrixQuadTree<'elementType>)
        =
        match lt, rt, lb, rb with
        | MLeaf (ltv, ltc), MLeaf (rtv, rtc), MLeaf (lbv, lbc), MLeaf (rbv, rbc) when
            (ltv = rtv && ltc = rtc)
            && (lbv = rtv && lbc = rtc)
            && (lbv = rbv && lbc = rbc)
            ->
            MLeaf(ltv, ltc * 2)
        | _ -> MNode(lt, rt, lb, rb)

    let rec private listToMatrix<'elementType when 'elementType: equality>
        (normSizeH: int option)
        (normSizeW: int option)
        (lst: 'elementType list list)
        : MatrixQuadTree<'elementType> =
        let normSizeH =
            if normSizeH.IsSome then
                normSizeH.Value
            else
                _getMin2Pow <| lst.Length

        let normSizeW =
            if normSizeW.IsSome then
                normSizeW.Value
            else
                let lenW =
                    if lst.Length = 0 then
                        0
                    else
                        lst.[0].Length

                if not
                   <| List.forall (fun line -> List.length line = lenW) lst then
                    raise
                    <| TensorConversionFromListException("Can't create matrix of inconsistent size")

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
                    if lst.Length <> 0 && normSize / 2 < lst.[0].Length then
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
                        && lst.Length <> 0
                        && normSize / 2 < lst.[0].Length) then
                        listToMatrix (Some(normSize / 2)) (Some(normSize / 2))
                        <| List.map
                            (fun (line: 'elementType list) -> line.[(normSize / 2) ..])
                            (lst.[(normSize / 2) ..])
                    else
                        MLeaf(None, normSize / 2)

                relaxMatrix lt rt lb rb
            elif lst.Length = 0 || lst.[0].Length = 0 then
                MLeaf(None, 1)
            else
                MLeaf(Some(lst.[0].[0]), 1)

    let rec private multVecMat<'vecType, 'matType, 'resType when 'resType: equality>
        (fAdd: 'resType option -> 'resType option -> 'resType option)
        (fMult: 'vecType option -> 'matType option -> int -> 'resType option)
        (vec: VectorBinTree<'vecType>)
        (mat: MatrixQuadTree<'matType>)
        : VectorBinTree<'resType> =
        match vec, mat with
        | VLeaf (vecVal, c1), MLeaf (matVal, c2) when c1 = c2 -> VLeaf(fMult vecVal matVal (int c1), c1)
        | VNode (v0, v1), MNode (a00, a01, a10, a11) ->
            let v0a00 = multVecMat fAdd fMult v0 a00
            let v1a10 = multVecMat fAdd fMult v1 a10

            let l =
                match v1a10 with
                | VLeaf (None, _) -> v0a00
                | _ -> vectorMap2 fAdd v0a00 v1a10

            let v0a01 = multVecMat fAdd fMult v0 a01
            let v1a11 = multVecMat fAdd fMult v1 a11

            let r =
                match v1a11 with
                | VLeaf (None, _) -> v0a01
                | _ -> vectorMap2 fAdd v0a01 v1a11

            relaxVector l r
        | VNode (v0, v1), MLeaf (matVal, c2) ->
            let v0a00 = multVecMat fAdd fMult v0 (MLeaf(matVal, c2 / 2))
            let v1a10 = multVecMat fAdd fMult v1 (MLeaf(matVal, c2 / 2))
            let v0a01 = v0a00
            let v1a11 = v1a10
            relaxVector (vectorMap2 fAdd v0a00 v1a10) (vectorMap2 fAdd v0a01 v1a11)
        | VLeaf (vecVal, c1), MNode (a00, a01, a10, a11) ->
            multVecMat fAdd fMult (VNode((VLeaf(vecVal, c1 / 2)), (VLeaf(vecVal, c1 / 2)))) mat
        | _ ->
            raise
            <| TensorMultiplicationException("Failed to multiply vector on matrix")

    let private wrapFunc func a b =
        match a, b with
        | Some (a), Some (b) -> Some(func a b)
        | _ -> None

    let private advancedMult fAdd fMult a b cnst2pow = // multiplication a * b and the result multiplied on 2^const from integers
        let rec multiply a cnst =
            if cnst = 1 then
                a
            else
                multiply (fAdd a a) (cnst / 2)

        match a, b with
        | Some (a), Some (b) -> Some(multiply (fMult a b) cnst2pow)
        | _ -> None

    // wrapper over MatrixQuadTree
    type Matrix<'elementType when 'elementType: equality>
        private
        (
            tree_: MatrixQuadTree<'elementType>,
            tupleHW: int * int
        ) =
        struct
            member internal this.tree: MatrixQuadTree<'elementType> = tree_
            member this.height: int = fst tupleHW
            member this.width: int = snd tupleHW
            member this.list() = matrixToList this.tree

            new(arr: 'elementType list list) =
                new Matrix<'elementType>(
                    listToMatrix None None arr,
                    (arr.Length,
                     if arr.Length = 0 then
                         0
                     else
                         arr.[0].Length)
                )
        end

    // wrapper over VectorBinTree
    type Vector<'elementType when 'elementType: equality> internal (tree_: VectorBinTree<'elementType>, len: int) =
        struct
            member internal this.tree = tree_
            member this.length = len
            member this.list() = vectorToList this.tree

            member this.mult
                (fAdd: 'resType -> 'resType -> 'resType)
                (fMult: 'elementType -> 'otherType -> 'resType)
                (other: Matrix<'otherType>)
                : Vector<'resType> =
                if this.length <> other.height then
                    raise
                    <| TensorMultiplicationException("Can't multiply vector to matrix: sizes don't match")

                Vector<'resType>(
                    multVecMat
                        (wrapFunc fAdd)
                        (advancedMult fAdd fMult)
                        (extendVector other.width this.length this.tree)
                        other.tree
                    |> trimVector,
                    other.width
                )

            static member map2 func (a: Vector<'elementTypeA>) (b: Vector<'elementTypeB>) =
                Vector(vectorMap2 (wrapFunc func) a.tree b.tree, a.length)

            new(arr: 'elementType list) = new Vector<'elementType>(listToVector None arr, arr.Length)
        end
