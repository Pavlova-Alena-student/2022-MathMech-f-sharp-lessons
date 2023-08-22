namespace MathMechFSharpLessons

module Task5 =
    open LinAlg

    type Graph = { adjacency: Matrix<bool> }

    type GraphVertexes =
        { properties: Vector<bool> }
        member this.tickFront(g: Graph) : GraphVertexes = // BFS step
            { properties = this.properties.mult (||) (&&) g.adjacency }

        member this.mask(visited: GraphVertexes) : GraphVertexes = // front without already visited vertexes
            { properties = Vector.map2 (fun a mask -> if not mask then a else false) this.properties visited.properties }

        member this.isEmpty: bool = // checks if front is empty
            Vector.allEqualTo false this.properties

        static member op_Addition(a: GraphVertexes, b: GraphVertexes) =
            { properties = Vector.map2 (||) a.properties b.properties }

    let accumulateDepth (positions: GraphVertexes) (depth: int) (acc: Vector<int option>) : Vector<int option> =
        Vector.map2 (fun acc pos -> if pos then Some(depth) else acc) acc positions.properties

    let BFS (graph: Graph) (vertex: int) : Vector<int option> = // finds shortest pathes from vertex to all other vertexes
        let rec BFS
            (graph: Graph)
            (front: GraphVertexes)
            (visited: GraphVertexes)
            (depth: int)
            (acc: Vector<int option>)
            =
            if front.isEmpty then
                acc
            else
                let newVisited = visited + front

                BFS
                    graph
                    ((front.tickFront graph).mask newVisited)
                    newVisited
                    (depth + 1)
                    (accumulateDepth front depth acc)

        let visited = { properties = Vector(List.replicate graph.adjacency.height false) }

        let front =
            { properties =
                Vector(
                    List.replicate graph.adjacency.height false
                    |> List.updateAt vertex true
                ) }

        let acc = Vector(List.replicate graph.adjacency.height Option<int>.None)
        BFS graph front visited 0 acc
