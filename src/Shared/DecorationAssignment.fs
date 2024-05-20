module DecorationAssignment
  open DataTypes
  open Helpers
  open FSharp.Core
  ///
  /// Compares a SkillRank to a Skill to determine if the SkillRank is of the skill.
  /// Used due to SkillRank having Id and Skill, which are different, and confusing, and prone to bugs.
  /// 
  let skillRankOfSkill (skill:Skill) (sr:SkillRank) =
    sr.Skill = skill.Id

  ///
  /// Check if a given decoration provides a given skill.
  /// 
  let decoContainsSkill (skill:Skill) (deco: Decoration) = 
    deco.Skills |> Array.filter (skillRankOfSkill skill) |> (not << Array.isEmpty)
  ///
  /// Check if a given decoration has any skill that is not the provided skill.
  ///
  let decoContainsOtherSkill (skill:Skill) (deco: Decoration) =
    deco.Skills |> Array.filter (not << (skillRankOfSkill skill))  |> (not << Array.isEmpty)

  ///
  /// Calculates the level of a particular skill in a decoration. If the skill is not present, returns None.
  /// 
  let decoSkillLevel (skill:Skill) (deco: Decoration) =
    deco.Skills |> Array.filter (skillRankOfSkill skill) |> Array.tryExactlyOne |> Option.map (fun sr -> sr.Level)

  ///
  /// Checks if the given decoration is a "singleton" - The decoration is not size 4, contains only 1 skill, and that skill is level 1.
  /// 
  let isSingletonDecoration skill decoration =
    (decoContainsSkill skill decoration) 
    && (not (decoContainsOtherSkill skill decoration)) 
    && (decoration |> decoSkillLevel skill) = Some 1

  ///
  /// Finds the singleton decoration for a given skill in a list of decorations, if it exists.
  /// 
  let singletonDecoration decorations skill = 
    decorations 
      |> List.filter (isSingletonDecoration skill)
      |> List.tryExactlyOne

  /// Calclulates the slot size of the singleton decoration of a given skill in a list of decorations.
  let decorationSlotSize decorations skill = 
    singletonDecoration decorations skill
    |> Option.map (fun deco -> Slot deco.Slot)


  let haveEnoughDecorations (wantedSkills:(Skill * int) list) (singletonDecorations: Map<Skill, (Decoration * int)>) = 
    let singletonDecorationCounts = 
      singletonDecorations |> Map.map (fun key value -> value |> snd)
    
    let decorationsAvailable skill = 
      singletonDecorationCounts 
      |> Map.tryFind skill |> Option.defaultValue 0

    wantedSkills 
    |> List.forall (fun (skill, nWanted) -> decorationsAvailable skill >= nWanted)


  ///
  /// Captures a list of slots to allocate decorations to, and returns a function which will emit slots as requested until the captured list would be empty.
  /// 
  /// Allocates smallest slots for a requested size first, then larger slots if available.
  /// If insufficient slots remain, will return None.
  /// 
  let captureSlotsToAllocate (availableSlotSizes: Map<Slot, int>) : int -> Slot -> Slot list option =
    let mutable unallocatedSlotSizes = availableSlotSizes
    let rec allocate n ((Slot slotSize) as slot) : Slot list option = 
      match slotSize with 
      | s when s > 3 -> None
      | s ->
        let slotsToAllocate = unallocatedSlotSizes |> Map.find slot
        match n, slotsToAllocate with
        | requested, available when available < requested ->
          unallocatedSlotSizes <- unallocatedSlotSizes |> Map.add slot 0
          let sameSize = List.replicate available slot
          let larger = allocate (requested - available) (Slot (slotSize + 1))
          larger |> Option.map (fun l -> sameSize @ l)
        | requested, available -> // when available >= requested
          unallocatedSlotSizes <- unallocatedSlotSizes |> Map.add slot (available - requested)
          Some (List.replicate requested slot)
    allocate


  ///
  /// Tries to assign decorations from a given list into the provided slots. Returns None if unable to do so.
  /// 
  let assignNonFourDecorations (wantedSkills: (Skill * int) list) (decoMap : Map<Skill, Map<Decoration, int>>) (decorationSlots : Slot list) = 
    // Sanitization, preparing data structures for later
    let decorationSlots = decorationSlots |> List.filter (fun (Slot slot) -> List.contains slot [1;2;3] )
    let decorations : Decoration list = 
      decoMap 
      |> Map.values 
      |> List.ofSeq
      |> List.map (fun m -> 
          m 
          |> Map.toList 
          |> List.map (fun (k,v) -> k)
        )
      |> List.concat

    let singletonDecorations = 
      decoMap 
        |> Map.map (fun skill decorations -> 
          decorations 
          |> Map.toList 
          |> List.choose (fun (deco, n) -> if isSingletonDecoration skill deco then Some (deco, n) else None )
          |> List.tryExactlyOne
          )
        |> Map.filter (fun key value -> value.IsSome)
        |> Map.map (fun key value -> value |> Option.get )

    // First, check if we even have enough decorations in our list for each skill.
    if not (haveEnoughDecorations wantedSkills singletonDecorations) then None else
      /// If we have enough decorations, check if we can assign to slots
      let skillsBySlotSize = 
        wantedSkills 
        |> List.groupBy (fun (skill, lvl) -> skill |> decorationSlotSize decorations)
        |> List.choose (fun (slot, skills) -> slot |> Option.map (fun sl -> sl, skills))
        |> Map.ofList

      let decosNeededBySlotSize = 
        skillsBySlotSize |> Map.map (fun key value -> value |> List.map snd |> List.sum)

      let availableSlotSizes = 
        decorationSlots |> List.countBy id |> Map.ofList

      let asTriple (map:Map<Slot, int>) =
        ( map |> Map.tryFind (Slot 1) |> Option.defaultValue 0
        , map |> Map.tryFind (Slot 2) |> Option.defaultValue 0
        , map |> Map.tryFind (Slot 3) |> Option.defaultValue 0
        )
      
      match decosNeededBySlotSize |> asTriple, availableSlotSizes |> asTriple with
      | (n1, n2, n3), (a1, a2, a3) when (a3 >= n3) && ((a3+a2) >= (n3 + n2)) && ((a3+a2+a1) >= (n3+n2+n1)) -> 
        let allocate = captureSlotsToAllocate availableSlotSizes
        [ for slot, skillsNeeded in skillsBySlotSize |> Map.toList |> List.sortByDescending (fun ((Slot slot), _skillList) -> slot) do
            for skill, nNeeded in skillsNeeded do
              let decoration, n = singletonDecorations |> Map.find skill 
              let chosenDecorations = List.replicate n decoration
              let slots = allocate nNeeded slot
              slots |> Option.map (fun s -> List.zip s chosenDecorations)
        ] 
        |> Option.sequenceList 
        |> Option.map List.concat
      | _ -> None


  type PairDecorationLookup(keys:Set<Skill>, defaultValue:(int)) =
    inherit SymmetricMatrix<Skill,(Decoration * int) option>(keys, None)

    member this.TryRemove((keys:(Skill*Skill)), (n:int)) =
      this.Get(keys) 
      |> Option.bind (fun (decoration, count) -> 
        if count >= n then 
          this.Set(keys, Some (decoration, count - n))
          Some (List.replicate n decoration)
        else
          None
      )

    member this.Count keys = 
      this.Get(keys) |> Option.map snd |> Option.defaultValue 0

    member this.CountAll key =
      this.GetAll(key) |> Array.sumBy (Option.map snd >> Option.defaultValue 0)

    static member ChooseFromSeq(args) : PairDecorationLookup = SymmetricMatrix.ChooseFromSeq(args) :?> PairDecorationLookup
    static member TryFromSeq(args) : PairDecorationLookup option = SymmetricMatrix.TryFromSeq(args) |> Option.map (fun x -> x :?> PairDecorationLookup)
    static member FromSeq(args) : PairDecorationLookup = SymmetricMatrix.FromSeq(args) :?> PairDecorationLookup



  let buildDecorationStructures (requestedSkills:(Skill * int) list) (decorations:(Decoration * int) list) =
    let skills = requestedSkills |> List.map fst

    let containsRequestedSkill ((decoration, _count):(Decoration * int)) = 
      decoration.Skills |> Array.exists (fun sr -> skills |> List.exists (fun skill -> skillRankOfSkill skill sr))

    let isHardDecoration ((decoration, _count):(Decoration * int)) = 
      (decoration.Skills |> Array.head).Level = 3

    let isPair ((decoration, _count):(Decoration * int)) = 
      decoration.Skills |> Array.forall (fun sr -> skills |> List.exists (fun skill -> skillRankOfSkill skill sr))

    let decorations = decorations |> List.filter containsRequestedSkill
    let singletons, fourStars = decorations |> List.partition (fun (decoration, n) -> decoration.Slot < 4)
    let hard, remainingFourStars = fourStars |> List.partition isHardDecoration
    let pairs, fourStarSingletons = remainingFourStars |> List.partition isPair

    let keysFromDecoration ((decoration:Decoration), (n:int)) =
      match decoration.Skills with
      | [|sr|] -> skills |> List.filter (fun skill -> skillRankOfSkill skill sr) |> List.tryExactlyOne |> Option.map (fun skill -> skill, skill)
      | [|sr1; sr2|] -> 
        let sk1 = skills |> List.filter (fun skill -> skillRankOfSkill skill sr1) |> List.tryExactlyOne
        let sk2 = skills |> List.filter (fun skill -> skillRankOfSkill skill sr2) |> List.tryExactlyOne
        sk1 |> Option.bind (fun sk1 -> sk2 |> Option.map (fun sk2 -> sk1, sk2))
      | _ -> None

    let keysFromDecoration' = Option.bind keysFromDecoration

    let pairsMatrix = PairDecorationLookup.ChooseFromSeq(keysFromDecoration', None, (pairs |> List.map Some)) :?> PairDecorationLookup

    singletons, hard, pairsMatrix, fourStarSingletons


  let decorationAssignmentHeuristic availableDecos availableSlotSizes requestedSkills = 
    let twoDArray = Array2D.create 10 10 0
    let test = (twoDArray |> Array2D.get) 4, 4
    0
    
    // Elimination of impossibles: 
      // - We don't have enough of a skill in our decorations to meet the requested skills
      // - The sum of requested skills is greater than the max capacity of the slot sizes
    
    // If the above is true, we can maybe make an assignment.
    // How to deal with 4* 
    // - Find distance between current needs and "solvable with singleton only" needs
    // - -  Current needs: 
    // - - - May not have enough singleton decorations for a skill, that could be solved with 4*
    // - - - May not have enough total decoration capacity for skills if assigned as singletons
    // - - Singleton only needs:
    // - - - Enough singleton decorations to meet each skill need
    // - - - Enough slot capacity to fill all skills of a size

    // - Need to augment Singleton only algorithm to allow for 4*, to solve first current need limitation
    // - - 4* slots can be assigned to, 
    // - - 4* singleton decorations are available and can be assigned
    // - - - 4* singletons must be assigned first, and only until we can assign with true singletons
    // - - - - Calculate difference between true singleton and 4* singleton decorations available
    // - - - - Effectively, first assign only the 4* singletons needed, then we can do normal assignment as above

    // - Then, need to solve getting from current needs to augmented singleton only needs
    // - - Calculate gap in:
    // - - - Slot capacity vs skills needed - If skills needed exceed slots, we need this many decorations "double dipping"
    // - - - Problems example 1: 
    // - - - - For skills A and B:
    // - - - - I have an A+ decoration, and two AB decorations. I need 2 A and 2 B skills, across 2 4* slots. Assigning the A+ is incorrect because A+ and AB overcap A and don't meet B.
    // - - - Problem example 2:
    // - - - - For skills A, B, C:
    // - - - - I have 3 4* slots. I need 2 points for each of A, B, and C. I have 2 of each AB, AC, and BC decorations. 
    // - - - - An assignment of AB, AB will not work, because I do not have a C+ decoration.
    // - - - Both problems seem to stem from when the needed amount for a skill hits 0, as this reduces the number of available decorations.
    // - - - 
    // - - - Problem example 3:
    // - - - - I have skills A, B, C, D: I need 4 A, 2B, 1C, 1D.
    // - - - - I have 4 4* slots. I have 1 A+, 1 AB, 1 AC, 1 AD, 1 BD
    // - - - - If I assign from the skill with the highest amount needed, 
    // - - - - I might assign (4, 2, 1, 1) -> AB (3, 1, 1, 1) -> AC (2, 1, 0, 1) -> AD (1, 1, 0, 0). I don't have a second AB, so I can't solve this.
    // - - - This contrasts with the first case, where assigning A+ last would be better.
    // - - - Problem example 4:
    // - - - - I need skills 4A, 1B, 1C, 1D, 1E - I have A+, AB, AC, AD, AE. I would need AB/AC/AD/AE; can't choose A+ first
    
    // Things that won't work:
    // - Choose decoration that satisfies highest skill(s)? - Fails Examples 3
    // - Assign decoration+ first? - Fails Example 1, 4
    // - Assign decorations by skill with fewest decorations? - Fails Example 2

    // Thoughts:
    // Look at minimizing decoration loss? As in, if I satisfy a skill, which decoration options do I lose?
    // Skills needed vs skills available in decorations?
    // - 1: A: (2, 4) -> 2, B: (2, 2) -> 0; choose decoration with B -> choose decoration with B
    // - 2: (2,4) (2,4) (2,4) -> AB -> (1,3) (1,3) (2,4) -> 0,2 0,2 2,4
    // - 3: 4,5 2,2 1,1 1,2 -> Choose B, AB vs BD are equivalent -> AB -> 3,4 1,1 1,1 1,2 -> BD -> 3,4 0,0 1,1 0,1 -> eliminate AD -> 3,3 0,0 1,1 0,0 -> AC -> AA
    // - 4: 4,6 1,1 1,1 1,1 1,1 -> AB -> 3,5 0,0 1,1 1,1 1,1 -> AC -> 2,4 0,0 0,0 1,1 1,1 -> AD -> 1,3, 0,0 0,0, 0,0, 1,1 -> AE -> done

    // Something like "reach"? "reach" - distance?
    // Remaining slots - max from decos?

    // min (needed-reach)
    // - 1: 2,3 2,2 -> AB -> 1,2 1,1 -> AB -> done
    // - 2: 2,3 2,3 2,3 -> AB -> 1,2 1,2 2,2 -> AC -> 0,1 1,1 1,1 -> BC
    // - 3: 4,5 2,2 1,1 1,2 -> BA -> 3,4 1,1 1,1 1,2 -> BD -> 2,3 0,0 1,1 0,1 -> AC -> 1,2, 0,0 0,0 1,1 -> DA -> 0,0 0,0 0,0 0,0
    // - 4: 4,5 1,1 1,1 1,1 1,1 -> BA -> 3,4 0,0 1,1 1,1 1,1 -> CA -> 2,3 0,0 0,0 1,1 1,1 -> DA -> 1,2 0,0 0,0 0,0 1,1 -> EA -> 0,0 0,0 0,0 0,0 0,0
    
    // - 4, but with BC as well
    // 4,5 1,2 1,2 1,1 1,1 -> DA -> 3,4 1,2 1,2 0,0 1,1 -> EA -> 2,3 1,2 1,2 0,0 0,0 -> A+ -> 0,1 1,1 1,1 0,0 0,0 -> BC -> it works?
    // 2, but minus BC?
    // 2,3 2,3 2,3 -> AB -> 1,2 1,2 2,2 -> AC -> 0,1 1,1 1,1 -> BC
    // 2,3 2,3 2,3 -> AC -> 1,2 2,2 1,2 -> BA -> 0,1 1,1 1,1 -> BC
    // 2,3 2,3 2,3 -> BC -> 2,2 1,2 1,2 -> AB -> 1,1 0,1 1,1 -> AC

    // 2, but minus an AB and a BC?
    // 2,3 2,2 2,3 -> AB -> 1,2 1,1 2,2 -> BC -> 1,1, 0,0 1,1 -> AC

    // 2, but only one of each?
    // 2,2 2,2 2,2 -> AB -> 1,1 1,1 2,2 -> AC -> 0,0 1,1 1,1 -> BC