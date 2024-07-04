module MaxFlowMap
  open DataTypes
  open Helpers
  
  type NumberedDecoration =
    {
      Id: int
      Decoration: Decoration
    }

  type NumberedSlot =
    {
      Id: int
      Slot: Slot
    }

  type NodeType = 
    | Source
    | Sink 
    | Skill of Skill
    | Decoration of NumberedDecoration
    | DecorationSlot of NumberedSlot

  /// By convention, the node closer to the source is on the left.
  type Adjacency = 
  | Forward of NodeType*NodeType
  | Reverse of NodeType*NodeType


  module Adjacency =
    let src = function
    | Forward (src, sink) -> src
    | Reverse (src, sink) -> sink

    let sink = function
    | Forward (src, sink) -> sink
    | Reverse (src, sink) -> src

    let edge = function
    | Forward (src,sink) -> (src,sink)
    | Reverse (src,sink) -> (src,sink)

    let isSame adj1 adj2 = 
      edge adj1 = edge adj2

  type MaxFlowByMap =
    { 
      DecorationAssignmentLimit: int
      Adjacency: Map<NodeType, Adjacency list>
      Capacity: Map<NodeType*NodeType, int>
      Flow: Map<NodeType*NodeType, int>
    }

  type Path = Adjacency list

  let assembleMaxFlowByMap (wantedSkills: (Skill * int) list) (decorationMap : Map<Skill, Map<Decoration, int>>) (decorationSlots : Slot list) : MaxFlowByMap =

    let source = Source

    let skills, skillCapacity, skillFlow = 
      [ for skill, requestedCount in wantedSkills ->
        let edge = (source, Skill skill)
        let adjacencies = [(source, Forward edge); (Skill skill, Reverse edge)]
        let capacity = (edge, requestedCount)
        let flow = (edge, 0)
        adjacencies, capacity, flow
      ] |> List.unzip3
      |> (fun (s, c, f) -> s |> List.concat, c, f)

    let matchingSkillRank skill (decoration:Decoration) = 
      decoration.Skills |> Array.where (fun sr -> skillRankOfSkill skill sr) |> Array.tryExactlyOne

    let skillToDecorationCapacity skill (decoration:Decoration) =
      decoration 
      |> matchingSkillRank skill
      |> Option.map (fun sr -> sr.Level)
      |> Option.defaultValue 0

    let decorations, decorationCapacity, decorationFlow = 
      [ for KeyValue (skill, decorationCounts) in decorationMap do 
          for KeyValue (decoration, count) in decorationCounts do
            for i in 1..count ->
              let decoNode = Decoration { Id = i; Decoration = decoration }
              let edge = (Skill skill, decoNode)
              let adjacencies = [ Skill skill, Forward edge; decoNode, Reverse edge ]
              let capacity = (edge, skillToDecorationCapacity skill decoration)
              let flow = (edge, 0)
              adjacencies, capacity, flow
      ] |> List.unzip3
      |> (fun (s, c, f) -> s |> List.concat, c, f)
  
    let decorationNodes = decorationCapacity |> List.map fst |> List.map snd |> Set.ofList

    let slots, slotCapacity, slotFlow = 
      [ for (slotSize, slots') in decorationSlots |> List.groupBy (fun (Slot n) -> n) do 
          for i, ((Slot slotSize) as slot) in slots' |> List.indexed do
            for decorationNode in decorationNodes do
              let decoration = 
                decorationNode |> 
                  function 
                  | Decoration d -> Some d.Decoration 
                  | _ -> None 
                |> Option.get
              if decoration.Slot <= slotSize then
                let slotNode = DecorationSlot {Id = i+1; Slot = slot}
                let edge = (decorationNode, slotNode)
                let adjacencies = [decorationNode, Forward edge; slotNode, Reverse edge]
                let capacity = (edge, List.min [(if slotSize = 4 then 2 else 1); (decoration.Skills |> Array.sumBy (fun sr -> sr.Level))])
                let flow = (edge, 0)
                adjacencies, capacity, flow
      ] 
      |> List.unzip3
      |> (fun (s, c, f) -> s |> List.concat, c, f)

    let slotNodes = slotCapacity |> List.map fst |> List.map snd |> Set.ofList

    let sink, sinkCapacity, sinkFlow = 
      [ for slotNode in slotNodes do 
          let edge = (slotNode, Sink)
          let adjacencies = [slotNode, Forward edge; Sink, Reverse edge]
          let capacity = (edge, slotNode |> function | DecorationSlot s when s.Slot = Slot 4 -> 2 | DecorationSlot s -> 1 | _ -> 0)
          let flow = (edge, 0)
          adjacencies, capacity, flow
      ] 
      |> List.unzip3
      |> (fun (s, c, f) -> s |> List.concat, c, f)

    let adjacencies = [skills; decorations; slots; sink] |> List.concat |> List.groupBy (fun (node, adjacency) -> node) |> List.map (fun (node, originalAdjacencies) -> node, originalAdjacencies |> List.map snd)
    let capacities = [skillCapacity; decorationCapacity; slotCapacity; sinkCapacity] |> List.concat
    let flows = [skillFlow; decorationFlow; slotFlow; sinkFlow] |> List.concat

    { Adjacency = adjacencies |> Map.ofList; Capacity = capacities |> Map.ofList; Flow = flows |> Map.ofList; DecorationAssignmentLimit = slotNodes |> Seq.length }

  let residualForwardCapacity (graph:MaxFlowByMap) (edge:NodeType*NodeType) = 
    let capacity = graph.Capacity |> Map.tryFind edge
    let flow = graph.Flow |> Map.tryFind edge
    capacity |> Option.bind (fun c -> flow |> Option.map (fun f -> c - f))

  let residualReverseCapacity (graph:MaxFlowByMap) (edge:NodeType*NodeType) =
    // Per Ford-Fulkerson, the capacity of the residual reverse edge can be found by:
    // Given:
    // - Capacity of the *non-residual* reverse edge as (C(rev))
    // - Reverse flow as (F(rev))
    // - Capacity of the residual reverse edge as (Cr(rev))
    // -   Cr(rev) = C(rev) - F(rev)
    // This matches the residual capacity in the forward direction: Cr = C - F
    // However, in our case, there are no reverse edges; thus C(rev) is always 0
    // Reverse flow is the negative of flow, thus F(rev) = -F
    // 0 - (-F) is just F
    graph.Flow |> Map.tryFind edge

  let residualCapacity (graph:MaxFlowByMap) (adjacency:Adjacency) =
    match adjacency with
    | Forward (src, sink) -> residualForwardCapacity graph (src, sink)
    | Reverse (src, sink) -> residualReverseCapacity graph (src, sink)
    |> Option.defaultValue 0

  let decorationAssignmentEdges (graph:MaxFlowByMap) = 
    graph.Flow.Keys 
    |> List.ofSeq
    |> List.filter (function | (Decoration d, DecorationSlot ds) -> true | _ -> false)

  let addForwardFlow pathFlow edge g = 
    g.Flow |> Map.change edge (Option.map (fun f -> f + pathFlow))

  let addReverseFlow pathFlow edge g = 
    g.Flow |> Map.change edge (Option.map (fun f -> f - pathFlow))

  let findMaxPathFlow (graph:MaxFlowByMap) (path:Path) = 
    path |> List.map (fun edge -> residualCapacity graph edge) |> List.min

  let addPathFlow (path:Path) pathFlow (graph:MaxFlowByMap)  =
    let updateGraph graph adjacency = 
      match adjacency with 
      | Forward (src, sink) -> { graph with Flow = graph |> addForwardFlow pathFlow (src, sink)}
      | Reverse (src, sink) -> { graph with Flow = graph |> addReverseFlow pathFlow (src, sink)}
    
    path |> List.fold updateGraph graph

  let assignFlowFromPath (path:Path) (graph:MaxFlowByMap) =
    let pathFlow = findMaxPathFlow graph path
    addPathFlow path pathFlow graph

  let withinDecorationSlotLimit (graph:MaxFlowByMap) =
    let decorationAssignmentEdges = decorationAssignmentEdges graph
    let edgesWithAssignedFlow = 
      decorationAssignmentEdges
      |> List.choose (fun edge -> graph.Flow |> Map.tryFind edge)
      |> List.filter (fun assignedFlow -> assignedFlow > 0)
    (edgesWithAssignedFlow |> List.length) <= graph.DecorationAssignmentLimit

  let nAdjacenciesInPath (path:Path) (adj:Adjacency) =
    path |> List.filter (fun pAdj -> Adjacency.edge pAdj = Adjacency.edge adj) |> List.length
 
  let fewerOccurrancesThanCapacities graph currentPath adjacency =
    (nAdjacenciesInPath currentPath adjacency) < residualCapacity graph adjacency

  let oppositeDirectionInPath currentPath adjacency = 
    currentPath |> List.exists (fun pathAdjacency -> not (pathAdjacency = adjacency) && (Adjacency.edge pathAdjacency = Adjacency.edge adjacency) )

  let rec findPathDFS (path:Path) (graph:MaxFlowByMap) : Path option = 
    
    let depthFirstSearch (currentPath: Path) (state: Path option) (possibleEdge: Adjacency) = 
      match state with 
      | Some path -> Some path
      | None -> graph |> findPathDFS (possibleEdge :: currentPath)

    let findNextAdjacency (graph:MaxFlowByMap) (currentPath: Path) (node: NodeType) =
      let nextNodes = graph.Adjacency |> Map.tryFind node |> Option.defaultValue []
      let possibleNextMoves = nextNodes |> List.filter (fun adj -> fewerOccurrancesThanCapacities graph currentPath adj && not (oppositeDirectionInPath currentPath adj))
      match possibleNextMoves with 
      | [] -> None
      | moves -> moves |> List.fold (depthFirstSearch currentPath) None
      
    match path with
    | [] -> 
      findNextAdjacency graph path Source 
    | Forward (src, sink) :: rest when sink = Sink && (graph |> assignFlowFromPath path) |> withinDecorationSlotLimit -> Some path
    | adjacency :: rest -> 
      let node = adjacency |> Adjacency.sink
      findNextAdjacency graph path node
  
  let rec findPathBFS (paths:Path list) (graph:MaxFlowByMap) : Path option = 
    let findNextAdjacencies (path:Path) : Path list =
      let node = 
        match path with
        | adjacency :: rest -> adjacency |> Adjacency.sink
        | [] -> Source
      
      let nextNodes = graph.Adjacency |> Map.tryFind node |> Option.defaultValue []
      let possibleNextMoves = nextNodes |> List.filter (fun adj -> fewerOccurrancesThanCapacities graph path adj && not (oppositeDirectionInPath path adj))
      possibleNextMoves |> List.map (fun nm -> nm :: path)

    let nextPaths = paths |> List.map findNextAdjacencies |>  List.concat
    printfn "BFS depth %i: %i paths found" (nextPaths |> List.tryHead |> Option.defaultValue [] |> List.length) (nextPaths |> List.length)

    let completingNextPaths =  
      nextPaths |> List.filter (fun path -> match path with | Forward (src, sink) :: rest when sink = Sink && graph |> assignFlowFromPath path |> withinDecorationSlotLimit -> true | _ -> false)

    match completingNextPaths, nextPaths with
    | path :: rest, _ -> Some path
    | _, [] -> None
    | _, nextPaths -> findPathBFS nextPaths graph

  let printNodeType nodeType = 
    match nodeType with
    | Source -> "Source"
    | Sink -> "Sink"
    | Skill s -> sprintf "%s" s.Name
    | Decoration d -> sprintf "%s %i" d.Decoration.Name d.Id
    | DecorationSlot d -> sprintf "Slot %i #%i" (d.Slot |> fun (Slot s) -> s) d.Id

  let printAdjacency (adjacency:Adjacency) =
    match adjacency with
    | Forward (src, sink) -> sprintf " -> %s" (sink |> printNodeType)
    | Reverse (src, sink) -> sprintf " (Reverse) -> %s" (src |> printNodeType)

  let printPath (path:Path) = 
    let orderedPath = path |> List.rev
    orderedPath |> List.fold (fun (outputString:string) adj -> outputString + (printAdjacency adj)) "Source"

  [<TailCallAttribute>]
  let rec ford_fulkerson (graph:MaxFlowByMap) : MaxFlowByMap = 
    match graph |> findPathBFS [[]] with
    | Some path -> 
      let pathFlow = path |> findMaxPathFlow graph
      printfn "Found Path! Assigning %i flow to:\n %A" pathFlow (path |> printPath)
      let updatedGraph = (graph |> addPathFlow path pathFlow)
      let totalFlow =
        updatedGraph
        |> decorationAssignmentEdges 
        |> List.choose (fun edge -> graph.Flow |> Map.tryFind edge)
        |> List.filter (fun assignedFlow -> assignedFlow > 0)
        |> List.sum 
      printfn "Total Flow: %i" totalFlow
      ford_fulkerson updatedGraph 
    | None -> 
      printfn "No more paths found!"
      graph


  type BetterDecoration = 
  | PairDecoration of (Skill * int) * (Skill * int)
  | SingleDecoration of Skill * int

  type DecorationAssignmentState = {
    RequestedSkills : (Skill * int) list
    Decorations : Map<Skill, (BetterDecoration * int) list>
    DecorationSlots : (Slot * BetterDecoration option) list
  }

  type AssignmentResult = 
  | Failed
  | Done of (Slot * BetterDecoration) list
  | Next of DecorationAssignmentState
  | Replacing of DecorationAssignmentState

  let decorationSkillLevel skill decoration = 
    match decoration with 
    | SingleDecoration (dSkill, level) ->
      if dSkill = skill then level else 0
    | PairDecoration ((dSkill1, level1), (dSkill2, level2)) ->
      let skill1Level = if dSkill1 = skill then level1 else 0
      let skill2Level = if dSkill2 = skill then level2 else 0
      skill1Level + skill2Level

  let skillFromSkillRanks skills skillRank = 
    skills |> List.filter (fun skill -> skillRankOfSkill skill skillRank) |> List.tryExactlyOne

  let assign_decoration (state:DecorationAssignmentState) : AssignmentResult =
    let emptySlots = state.DecorationSlots |> List.filter (fun (slot, deco) -> deco.IsNone)
    let assignedDecos = state.DecorationSlots |> List.choose (fun (slot, deco) -> deco)
    let assignedSkills = 
      [ for deco in assignedDecos do
          match deco with
          | SingleDecoration (skill, count) -> [skill, count]
          | PairDecoration ((skill1, count1), (skill2, count2)) -> [skill1, count1; skill2, count2]
      ] |> List.concat
      |> List.groupBy fst
      |> List.map (fun (key, values) -> key, values |> List.map snd |> List.sum)

    let skillBalance = 
      [ for skill, requestedAmount in state.RequestedSkills do
          let assignedPoints = assignedSkills |> List.tryFind (fun (sk, i) -> sk = skill) |> Option.map snd |> Option.defaultValue 0
          skill, requestedAmount - assignedPoints      
      ]

    match emptySlots with 
    | [] -> Done (state.DecorationSlots |> List.choose (fun (slot, deco) -> deco |> Option.map (fun dec -> slot, dec)))
    | slots ->
      let nextSkillToAssign = skillBalance |> List.tryFind (fun (skill, balance) -> balance > 0)
      match nextSkillToAssign with 
      | None -> Done (state.DecorationSlots |> List.choose (fun (slot, deco) -> deco |> Option.map (fun dec -> slot, dec)))
      | Some (skill, remainingSkillPointsNeeded) ->
        let decorationsWithThisSkill = state.Decorations |> Map.tryFind skill |> Option.defaultValue []
        let decorations = decorationsWithThisSkill |> List.groupBy (fun (decoration, count) -> decorationSkillLevel skill decoration)
        
        Done (state.DecorationSlots |> List.choose (fun (slot, deco) -> deco |> Option.map (fun dec -> slot, dec)))

  let maximumPossibleSkillAssignment 
    (requestedSkills : (Skill * int) list) 
    (decorationSlots : (Slot * BetterDecoration option) list)
    (decorations : Map<Skill, (BetterDecoration * int) list>) =
      let fullyMatchedDecoration skills decoration = 
        match decoration with
        | SingleDecoration (sk, lv) -> skills |> List.contains sk
        | PairDecoration ((sk1, lv1), (sk2, lv2)) -> [sk1; sk2] |> List.forall (fun decoSkill -> skills |> List.contains decoSkill) 
      
      let maxSkillDecorations = 
        decorations 
        |> Map.values
        |> List.ofSeq
        |> List.distinct
        |> List.concat
        |> List.filter (fun (deco, count) ->  deco |> fullyMatchedDecoration (requestedSkills |> List.map fst))

      

      ()