namespace MathMechFSharpLessons

module Task5 =
    open LinAlg

    type Graph = { adjacency: Matrix<int> }

    type GraphVertexes =
        | IntProperties of Vector<int>
        | PositiveProperties of Vector<uint>
        | BinProperties of Vector<bool>
        member this.tickFront(g: Graph) : GraphVertexes =
            match this with
            | IntProperties (props) -> IntProperties(props.mult (+) (*) g.adjacency)
            | PositiveProperties (props) -> PositiveProperties(props.mult (+) (fun a b -> a * uint (b)) g.adjacency)
            | BinProperties (props) -> BinProperties(props.mult (||) (fun p value -> p && (value <> 0)) g.adjacency)

        member this.length =
            match this with
            | IntProperties (props) -> props.length
            | PositiveProperties (props) -> props.length
            | BinProperties (props) -> props.length

        member this.notIn(other: GraphVertexes) : bool =
            match this, other with
            | BinProperties (thisProps), BinProperties (otherProps) ->
                let masked =
                    Vector.map2 (fun a b -> if b = false then a else false) thisProps otherProps

                let rec allZero (tree: VectorBinTree<bool>) : bool = // TODO: NO VECTORBINTREEEEE!!!
                    match tree with
                    | VLeaf (None, _) -> true
                    | VLeaf (Some (false), _) -> true
                    | VNode (l, r) -> (allZero l) && (allZero r)
                    | _ -> false

                not (allZero (masked.tree))
            | _ -> false // TODO: What does "in" mean in other cases?

        static member op_Addition(a: GraphVertexes, b: GraphVertexes) =
            match a, b with
            | IntProperties (a), IntProperties (b) -> IntProperties(Vector.map2 (+) a b)
            | PositiveProperties (a), PositiveProperties (b) -> PositiveProperties(Vector.map2 (+) a b)
            | BinProperties (a), BinProperties (b) -> BinProperties(Vector.map2 (||) a b)

    let BFS (front: GraphVertexes) (graph: Graph) : GraphVertexes =
        let rec BFS (front: GraphVertexes) (graph: Graph) (acc: GraphVertexes) : GraphVertexes =
            if front.notIn acc then
                BFS(front.tickFront graph) graph (acc + front)
            else
                acc

        BFS front graph (BinProperties(Vector(List.replicate front.length false)))
