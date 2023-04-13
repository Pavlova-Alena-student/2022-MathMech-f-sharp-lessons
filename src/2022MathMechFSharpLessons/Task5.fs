namespace MathMechFSharpLessons

module Task5 =
    open LinAlg

    type Graph = { adjacency: Matrix<int> }

    type GraphVertexes =
        { properties: Vector<int> }
        member this.tickFront(g: Graph) : GraphVertexes =
            { properties = this.properties.mult (+) (*) g.adjacency }

        member this.notIn(other: GraphVertexes) : bool =
            let masked =
                Vector.map2 (fun a b -> if b = 0 then a else 0) this.properties other.properties

            let rec allZero (tree: VectorBinTree<int>) : bool =
                match tree with
                | VLeaf (None, _) -> true
                | VLeaf (Some (0), _) -> true
                | VNode (l, r) -> (allZero l) && (allZero r)
                | _ -> false

            not (allZero (masked.tree))

        static member op_Addition(a: GraphVertexes, b: GraphVertexes) =
            { properties = Vector.map2 (+) a.properties b.properties }

    let BFS (front: GraphVertexes) (graph: Graph) : GraphVertexes =
        let rec BFS (front: GraphVertexes) (graph: Graph) (visited: GraphVertexes) : GraphVertexes =
            if front.notIn visited then
                BFS(front.tickFront graph) graph (visited + front)
            else
                visited

        BFS front graph { properties = Vector(List.replicate front.properties.length 0) }
