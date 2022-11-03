namespace MathMechFSharpLessons

open System

module LinAlg =
    let _getMin2Pow (n: int) =
        let getRidOfZero (twoPow: int) (n: int) = n ||| (n >>> twoPow)

        (n - 1)
        |> getRidOfZero 1
        |> getRidOfZero 2
        |> getRidOfZero 4
        |> getRidOfZero 8
        |> getRidOfZero 16
        |> (+) 1

    // FIXME:
    //  create another constructor, so it is not counting _getMin2Pow every iteration
    //  check preconditions: |arr| > 0, |arr.[i]| = |arr.[j]| > 0 for each i,j
    //  add postcondion-checks in tests
    //  make sure equality is needed everywhere it is used
    
    //  This is not sparse at all, I still need to add relaxation XD

    type _VectorBinTree<'elementType when 'elementType: equality>(arr: 'elementType list) =
        struct
            //if arr.size < 1 then
            //    raize <| Exception("Can't create vector of 0 size")
            member this.size: int = List.length arr
            member this._actualSize = _getMin2Pow (this.size)

            member this.left: _VectorBinTree<'elementType> option =
                if this.size > 1 then
                    Some(_VectorBinTree (arr.[.. (this._actualSize / 2)]))
                else
                    None

            member this.right: _VectorBinTree<'elementType> option =
                if this.size > 2 then
                    Some(_VectorBinTree (arr.[(this._actualSize / 2) ..]))
                else
                    None

            member this.value: 'elementType option =
                if this.size = 1 then
                    Some(arr.[0])
                else
                    None

            member this.depth: int =
                if this.left.IsSome then
                    this.left.Value.depth + 1
                else
                    0
        end

    type Vector<'elementType when 'elementType: equality>(arr: 'elementType list) =
        struct
            member this.tree = _VectorBinTree<'elementType> (arr)
        end

    type _MatrixQuadTree<'elementType when 'elementType: equality>(arr: 'elementType list list) =
        struct
            member this.sizeH: int = List.length arr
            member this.sizeW: int = List.length arr.[0]

            member this._actualSizeH: int = _getMin2Pow (this.sizeH)
            member this._actualSizeW: int = _getMin2Pow (this.sizeW)

            member this.leftUp: _MatrixQuadTree<'elementType> option =
                if this.sizeH > 1 && this.sizeW > 1 then
                    let _actualSizeW = this._actualSizeW

                    Some(
                        _MatrixQuadTree (
                            List.map
                                (fun (line: 'elementType list) -> line.[.. (_actualSizeW / 2)])
                                arr.[.. (this._actualSizeH / 2)]
                        )
                    )
                else
                    None

            member this.rightUp: _MatrixQuadTree<'elementType> option =
                if this.sizeH > 1 && this.sizeW > 2 then
                    let _actualSizeW = this._actualSizeW

                    Some(
                        _MatrixQuadTree (
                            List.map
                                (fun (line: 'elementType list) -> line.[(_actualSizeW / 2) ..])
                                arr.[.. (this._actualSizeH / 2)]
                        )
                    )
                else
                    None

            member this.leftDown: _MatrixQuadTree<'elementType> option =
                if this.sizeH > 2 && this.sizeW > 1 then
                    let _actualSizeW = this._actualSizeW

                    Some(
                        _MatrixQuadTree (
                            List.map
                                (fun (line: 'elementType list) -> line.[.. (_actualSizeW / 2)])
                                arr.[(this._actualSizeH / 2) ..]
                        )
                    )
                else
                    None

            member this.rightDown: _MatrixQuadTree<'elementType> option =
                if this.sizeH > 2 && this.sizeW > 2 then
                    let _actualSizeW = this._actualSizeW

                    Some(
                        _MatrixQuadTree (
                            List.map
                                (fun (line: 'elementType list) -> line.[(_actualSizeW / 2) ..])
                                arr.[(this._actualSizeH / 2) ..]
                        )
                    )
                else
                    None

            member this.value: 'elementType option =
                if this.sizeH = 1 && this.sizeW = 1 then
                    Some(arr.[0].[0])
                else
                    None

            member this.depth: int =
                if this.leftUp.IsSome then
                    this.leftUp.Value.depth + 1
                else
                    0
        end

    type Matrix<'elementType when 'elementType: equality>(arr: 'elementType list list) =
        struct
            member this.tree = _MatrixQuadTree<'elementType> (arr)
        end

    let rec transpose<'elementType when 'elementType: equality> (tensor: Matrix<'elementType>) = //(s: int) (t: int) =
        tensor // will be easy when I impl constructor new(mat, mat option, mat option, mat option)
