namespace MathMechFSharpLessons

open System

module LinAlg =
    type ITensorTree<'elementType when 'elementType: equality> =
        abstract member size: int
        abstract member _actualSize: int
        abstract member value: 'elementType option
        abstract member depth: int

    type ITensor<'elementType when 'elementType: equality> =
        // abstract member list: list
        abstract member transpose: ITensor<'elementType>

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
    //  check preconditions: |arr| > 0, |arr.[i]| = |arr.[j]| > 0 for each i,j ONLY when flag debug is active
    //  add postcondion-checks in tests
    //  make sure equality is needed everywhere it is used

    //  This is not sparse at all, I still need to add relaxation XD

    type VectorBinTree<'elementType when 'elementType: equality>
        (
            size_: int,
            _actualSize_: int,
            a0: VectorBinTree<'elementType> option,
            a1: VectorBinTree<'elementType> option,
            value_: 'elementType option,
            depth_: int
        ) =
        struct
            member this.size: int = size_
            member this._actualSize: int = _actualSize_
            member this.left: VectorBinTree<'elementType> option = a0
            member this.right: VectorBinTree<'elementType> option = a1
            member this.value: 'elementType option = value_
            member this.depth: int = depth_

            member this.listWithTail(tail: 'elementType list) =
                if this.value.IsSome then
                    [ this.value.Value ] @ tail
                elif this.left = None then
                    tail
                elif this.right = None then
                    this.left.Value.listWithTail tail
                else
                    let newTail = this.right.Value.listWithTail tail // rvalue needed -_-
                    this.left.Value.listWithTail newTail

            member this.list: 'elementType list = this.listWithTail []

            new(arr: 'elementType list, _actualSize__: int option) =
                let size__ = arr.Length

                if size__ = 0 then
                    raise
                    <| new Exception("Can't create empty vector")

                let _actualSize__ =
                    if _actualSize__.IsSome then
                        _actualSize__.Value
                    else
                        _getMin2Pow (size__)

                let a0 =
                    if _actualSize__ > 1 then
                        Some(new VectorBinTree<'elementType>(arr.[.. (_actualSize__ / 2 - 1)], Some(_actualSize__ / 2)))
                    else
                        None

                let a1 =
                    if _actualSize__ > 1 && size__ > _actualSize__ / 2 then
                        Some(new VectorBinTree<'elementType>(arr.[(_actualSize__ / 2) ..], Some(_actualSize__ / 2)))
                    else
                        None

                if size__ = 1 then
                    new VectorBinTree<'elementType>(size__, _actualSize__, None, None, Some(arr.[0]), 1)
                elif a0.Value.value.IsSome
                     && a1.Value.value.IsSome
                     && a0.Value.value.Value = a1.Value.value.Value then // T_T
                    new VectorBinTree<'elementType>(
                        size__,
                        _actualSize__,
                        None,
                        None,
                        a0.Value.value,
                        a0.Value.depth + 1
                    )
                else
                    new VectorBinTree<'elementType>(size__, _actualSize__, a0, a1, None, a0.Value.depth + 1)
        end

    type Vector<'elementType when 'elementType: equality>(tree_: VectorBinTree<'elementType>) =
        struct
            member this.tree: VectorBinTree<'elementType> = tree_
            member this.list = this.tree.list

            new(arr: 'elementType list) = new Vector<'elementType>(new VectorBinTree<'elementType>(arr, None))
        end

    type MatrixQuadTree<'elementType when 'elementType: equality>
        (
            sizeH_: int,
            sizeW_: int,
            _actualSizeH_: int,
            _actualSizeW_: int,
            a00: MatrixQuadTree<'elementType> option,
            a01: MatrixQuadTree<'elementType> option,
            a10: MatrixQuadTree<'elementType> option,
            a11: MatrixQuadTree<'elementType> option,
            data_: VectorBinTree<'elementType list> option,
            depth_: int
        ) =
        struct
            member this.sizeH: int = sizeH_
            member this.sizeW: int = sizeW_
            member this._actualSizeH: int = _actualSizeH_
            member this._actualSizeW: int = _actualSizeW_
            member this.leftUp: MatrixQuadTree<'elementType> option = a00
            member this.rightUp: MatrixQuadTree<'elementType> option = a01
            member this.leftDown: MatrixQuadTree<'elementType> option = a10
            member this.rightDown: MatrixQuadTree<'elementType> option = a11
            member this.data: VectorBinTree<'elementType list> option = data_
            member this.depth: int = depth_

            member this.list: 'elementType list list =
                if this.data.IsSome then
                    this.data.Value.list
                else
                    let leftPart =
                        if this.leftUp <> None && this.leftDown <> None then
                            (this.leftUp.Value.list)
                            @ (this.leftDown.Value.list)
                        elif this.leftUp <> None then
                            (this.leftUp.Value.list)
                        else // Note: it should be impossible for left (or top) chunk to be "None", when right (bottom) is not
                            []

                    let rightPart =
                        if this.rightUp <> None && this.rightDown <> None then
                            (this.rightUp.Value.list)
                            @ (this.rightDown.Value.list)
                        elif this.rightUp <> None then
                            (this.rightUp.Value.list)
                        else
                            []

                    List.map (fun (r, l) -> r @ l)
                    <| List.zip leftPart rightPart

            new(arr: 'elementType list list, _actualSizeH__: int option, _actualSizeW__: int option) =
                let sizeH = List.length arr

                if sizeH = 0 then
                    raise
                    <| new Exception("Can't create empty matrix")

                let sizeW = List.length arr.[0]

                if not
                   <| List.forall (fun a -> List.length a = sizeW) arr then
                    raise
                    <| new Exception("Can't create matrix of inconsistent size")

                let _actualSizeH =
                    if _actualSizeH__.IsSome then
                        _actualSizeH__.Value
                    else
                        _getMin2Pow (sizeH)

                let _actualSizeW =
                    if _actualSizeW__.IsSome then
                        _actualSizeW__.Value
                    else
                        _getMin2Pow (sizeW)

                let leftUp: MatrixQuadTree<'elementType> option =
                    if sizeH > 1 && sizeW > 1 then
                        Some(
                            new MatrixQuadTree<'elementType>(
                                List.map
                                    (fun (line: 'elementType list) -> line.[.. (_actualSizeW / 2 - 1)])
                                    (arr.[.. (_actualSizeH / 2 - 1)]),
                                None,
                                None
                            )
                        )
                    else
                        None

                let rightUp: MatrixQuadTree<'elementType> option =
                    if sizeH > 1 && sizeW > 1 then
                        Some(
                            new MatrixQuadTree<'elementType>(
                                List.map
                                    (fun (line: 'elementType list) -> line.[(_actualSizeW / 2) ..])
                                    (arr.[.. (_actualSizeH / 2 - 1)]),
                                None,
                                None
                            )
                        )
                    else
                        None

                let leftDown: MatrixQuadTree<'elementType> option =
                    if sizeH > 1 && sizeW > 1 then
                        Some(
                            new MatrixQuadTree<'elementType>(
                                List.map
                                    (fun (line: 'elementType list) -> line.[.. (_actualSizeW / 2 - 1)])
                                    (arr.[(_actualSizeH / 2) ..]),
                                None,
                                None
                            )
                        )
                    else
                        None

                let rightDown: MatrixQuadTree<'elementType> option =
                    if sizeH > 1 && sizeW > 1 then
                        Some(
                            new MatrixQuadTree<'elementType>(
                                List.map
                                    (fun (line: 'elementType list) -> line.[(_actualSizeW / 2) ..])
                                    (arr.[(_actualSizeH / 2) ..]),
                                None,
                                None
                            )
                        )
                    else
                        None

                let data: VectorBinTree<'elementType list> option =
                    if sizeH = 1 || sizeW = 1 then
                        Some(new VectorBinTree<'elementType list>(arr, Some(_actualSizeW * _actualSizeH)))
                    else
                        None

                let depth: int =
                    if leftUp.IsSome then
                        leftUp.Value.depth + 1
                    elif data.IsSome then
                        data.Value.depth + 1
                    else
                        1

                new MatrixQuadTree<'elementType>(
                    sizeH,
                    sizeW,
                    _actualSizeH,
                    _actualSizeW,
                    leftUp,
                    rightUp,
                    leftDown,
                    rightDown,
                    data,
                    depth
                )
        end

    type Matrix<'elementType when 'elementType: equality>(tree_: MatrixQuadTree<'elementType>) =
        struct
            member this.tree = tree_

            member this.list = this.tree.list

            new(arr: 'elementType list list) =
                new Matrix<'elementType>(new MatrixQuadTree<'elementType>(arr, None, None))
        end

    let rec transposeMatrixTree<'elementType when 'elementType: equality> (tree: MatrixQuadTree<'elementType>) = //(s: int) (t: int) =
        new MatrixQuadTree<'elementType>(
            tree.sizeW,
            tree.sizeH,
            tree._actualSizeW,
            tree._actualSizeH,
            (if tree.leftUp.IsSome then
                 Some(transposeMatrixTree <| tree.leftUp.Value)
             else
                 None),

            (if tree.leftUp.IsSome then
                 Some(transposeMatrixTree <| tree.leftDown.Value)
             else
                 None),

            (if tree.leftUp.IsSome then
                 Some(transposeMatrixTree <| tree.rightUp.Value)
             else
                 None),

            (if tree.leftUp.IsSome then
                 Some(transposeMatrixTree <| tree.rightDown.Value)
             else
                 None),

            tree.data,
            tree.depth
        )

    let transpose<'elementType when 'elementType: equality> (tensor: Matrix<'elementType>) = //(s: int) (t: int) =
        new Matrix<'elementType>(transposeMatrixTree <| tensor.tree)
