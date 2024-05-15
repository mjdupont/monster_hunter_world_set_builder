module DecorationAssignment
  open DataTypes
  open Helpers
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