module MaxFlow

open DataTypes

type Vertex<'a> = { Id: int; Value: 'a }

type Edge<'a> = { Source: 'a; Sink: 'a; Capacity: int }

type NodeType =
    | Source
    | Sink
    | Skill of Skill
    | Decoration of Decoration
    | Slot of Slot

type MaxFlowPlot = {
    Vertices: Vertex<NodeType> list
    Edges: Edge<NodeType> list
}

let assembleGraph
    (wantedSkills: (Skill * int) list)
    (decorationMap: Map<Skill, Map<Decoration, int>>)
    (decorationSlots: Slot list)
    : MaxFlowPlot =

    let source = [ Source ]

    let skills, skillQuantityEdges =
        [
            for skill, requestedCount in wantedSkills ->
                Skill skill,
                {
                    Source = Source
                    Sink = Skill skill
                    Capacity = requestedCount
                }
        ]
        |> List.unzip

    let matchingSkillRank skill (decoration: Decoration) =
        decoration.Skills
        |> Array.where (fun sr -> skillRankOfSkill skill sr)
        |> Array.tryExactlyOne

    let skillToDecorationCapacity skill (decoration: Decoration) =
        decoration
        |> matchingSkillRank skill
        |> Option.map (fun sr -> sr.Level)
        |> Option.defaultValue 0

    let decorations, skillToDecorationAssignment =
        [
            for KeyValue(skill, decorationCounts) in decorationMap do
                for KeyValue(decoration, count) in decorationCounts do
                    [
                        for i in 0..count ->
                            Decoration decoration,
                            {
                                Source = Skill skill
                                Sink = Decoration decoration
                                Capacity = skillToDecorationCapacity skill decoration
                            }
                    ]
        ]
        |> List.groupBy (List.unzip >> fst)
        |> List.map (fun (nodes, pairs) -> nodes, pairs |> List.map (List.map snd) |> List.concat)
        |> List.unzip
        |> fun (nodes, edges) -> nodes |> List.concat, edges |> List.concat

    let slots, decorationToSlotAssignment =
        [
            for ((DataTypes.Slot slotSize) as slot) in decorationSlots do
                Slot slot,
                [
                    for decoration in decorations ->
                        {
                            Source = decoration
                            Sink = Slot slot
                            Capacity = (if slotSize = 4 then 2 else 1)
                        }
                ]
        ]
        |> List.unzip
        |> fun (nodes, edges) -> nodes, edges |> List.concat

    let slotToSinkEdges = [
        for node in slots do
            match node with
            | Slot((DataTypes.Slot slotSize) as slot) ->
                yield {
                    Source = Slot slot
                    Sink = Sink
                    Capacity = (if slotSize = 4 then 2 else 1)
                }
            | _ -> ()
    ]

    let sink = [ Sink ]

    let nodes =
        [ source; skills; decorations; slots; sink ]
        |> List.concat
        |> List.mapi (fun i node -> { Id = i; Value = node })

    let edges =
        [
            skillQuantityEdges
            skillToDecorationAssignment
            decorationToSlotAssignment
            slotToSinkEdges
        ]
        |> List.concat

    { Vertices = nodes; Edges = edges }

type SearchableEdge<'a> = {
    Edge: Edge<'a>
    Flow: int
    ForwardResidualCapacity: int
    ReverseResidualCapacity: int
    Source: SearchableVertex<'a> ref
    Sink: SearchableVertex<'a> ref
}

and SearchableVertex<'a> = {
    Vertex: Vertex<'a>
    SourceEdges: SearchableEdge<'a> ref list
    SinkEdges: SearchableEdge<'a> ref list
}

type SearchableMaxFlowPlot<'a> = {
    Vertices: SearchableVertex<'a> ref list
    Edges: SearchableEdge<'a> ref list
}



let initializeSearchableMaxFlowPlot (maxFlowPlot: MaxFlowPlot) : SearchableMaxFlowPlot<NodeType> =
    let nodes = [
        for node in maxFlowPlot.Vertices ->
            ref {
                Vertex = node
                SourceEdges = []
                SinkEdges = []
            }
    ]

    let edges = [
        for edge in maxFlowPlot.Edges ->
            ref {
                Edge = edge
                Flow = 0
                ForwardResidualCapacity = edge.Capacity
                ReverseResidualCapacity = 0
                Source =
                    nodes
                    |> List.filter (fun node -> node.Value.Vertex.Value = edge.Source)
                    |> List.tryExactlyOne
                    |> Option.get
                Sink =
                    nodes
                    |> List.filter (fun node -> node.Value.Vertex.Value = edge.Sink)
                    |> List.tryExactlyOne
                    |> Option.get
            }
    ]

    for node in nodes do
        node.Value <- {
            node.Value with
                SourceEdges =
                    edges
                    |> List.filter (fun edge -> edge.Value.Sink = node || edge.Value.Source = node)
                SinkEdges =
                    edges
                    |> List.filter (fun edge -> edge.Value.Source = node || edge.Value.Source = node)
        }

    { Vertices = nodes; Edges = edges }

type Path = ((SearchableVertex<NodeType> ref * SearchableEdge<NodeType> ref) list * SearchableVertex<NodeType> ref)

let validSinkEdge (edge: SearchableEdge<NodeType> ref) = edge.Value.ForwardResidualCapacity > 0

let validSourceEdge (edge: SearchableEdge<NodeType> ref) = edge.Value.ReverseResidualCapacity > 0

let breadthFirstSearch (searchableMaxFlowPlot: SearchableMaxFlowPlot<NodeType>) (paths: Path list) : Path list =
    match paths with
    | [] ->
        let sources =
            searchableMaxFlowPlot.Vertices
            |> List.filter (fun vertex -> vertex.Value.Vertex.Value = Source)

        [
            for src in sources do
                for edge in src.Value.SinkEdges do
                    let nextNode = edge.Value.Sink
                    [ src, edge ], nextNode
        ]
    | _ -> [
        for path, currentNode in paths do
            let forwardEdges = currentNode.Value.SinkEdges |> List.filter validSinkEdge
            let reverseEdges = currentNode.Value.SourceEdges |> List.filter validSourceEdge

            for edge in forwardEdges do
                let nextNode = edge.Value.Sink
                (currentNode, edge) :: path, nextNode

            for edge in reverseEdges do
                let nextNode = edge.Value.Source
                (currentNode, edge) :: path, nextNode
      ]

// let rec findPaths (searchableMaxFlowPlot : SearchableMaxFlowPlot<NodeType>) (paths : Path list) : Path list =
//   let potentialPaths = breadthFirstSearch searchableMaxFlowPlot paths
//   let completePaths = potentialPaths |> List.filter (fun path -> path)