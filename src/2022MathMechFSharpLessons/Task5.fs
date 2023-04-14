namespace MathMechFSharpLessons

module Task5 =
    open LinAlg

    type Graph = { adjacency: Matrix<int> }

    type GraphVertexes =
        { properties: Vector<int option> }
        member this.tickFront(g: Graph) : GraphVertexes = // BFS step
            { properties =
                this.properties.mult
                    (Option.map2 (+))
                    (fun opt elem ->
                        match opt with
                        | Some (opt) -> Some(opt * elem)
                        | _ -> None)
                    g.adjacency }

        member this.mask(visited: bool list) : GraphVertexes = // maskes front with already visited vertexes
            { properties =
                Vector.map2 (fun a visit -> if visit = false then a else None) this.properties (Vector(visited)) }

        member this.visit(visited: bool list) : bool list = // adds front vertexes to visited
            (Vector.map2
                (fun a visit ->
                    match a with
                    | Some (_) -> true
                    | _ -> visit)
                this.properties
                (Vector(visited)))
                .list ()

        member this.isEmpty: bool = // checks if front is empty
            Vector.allEqualTo None this.properties

        static member op_Addition(a: GraphVertexes, b: GraphVertexes) =
            { properties = Vector.map2 (Option.map2 (+)) a.properties b.properties }

    let BFS (graph: Graph) (vertex: int) : Vector<int option> = // finds shortest pathes from vertex to all other vertexes
        let rec BFS (graph: Graph) (front: GraphVertexes) (visited: bool list) (acc: GraphVertexes) =
            if front.isEmpty then
                acc
            else
                BFS graph ((front.tickFront graph).mask (visited)) (front.visit (visited)) (acc + front)

        let visited = List.replicate graph.adjacency.height false

        let front =
            { properties =
                Vector(
                    List.replicate graph.adjacency.height Option<int>.None
                    |> List.updateAt vertex (Some(0))
                ) }

        let acc = front
        (BFS graph front visited acc).properties
