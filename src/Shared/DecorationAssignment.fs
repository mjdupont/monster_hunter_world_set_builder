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
  /// Returns the skills contained in a Decoration, along with their levels.
  /// 
  let containedSkills (skills: Skill list) (deco:Decoration) = 
    deco.Skills 
    |> Array.choose (fun decoSr -> skills |> List.filter (fun sk -> skillRankOfSkill sk decoSr) |> List.tryExactlyOne |> Option.map (fun sk -> sk, decoSr.Level))
    |> List.ofArray

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

  // Algorithm Approach:
  //
  // Preparation:
  // Shrink decorations to include only those for which all skills they can provide are requested.
  // Set aside all wildcard "Any 4* with" decorations for all 4* decorations with only one skill contributing.
  // Verify we have enough decorations + slots to satisfy each skill
  // If we have excess slots such that our requested skills is greater than our skill capacity, set aside extra slots
  // Move to assignment:
  //
  // Choose a decoration for which all skills can contribute, but not over-cap. 
  // Start with 4* as singleton decorations are generally easier to get/fill/solve.
  // Assign that decoration.
  // Repeat until no decorations exist for which all skills can contribute.
  // Check if we have satisfied skills; if not, move to replacement.
  //
  // Replacement:
  // Considerations:
  // We may not have a valid assignment, if we have enough slots to provide all skill points, and enough decorations in each skill, but no valid way to assign them.
  // - Example: I want 2 Attack, 2 Crit, 2 Offensive Guard, in 3 slots, with a Crit+, Attack +, Offensive Guard/Attack, and Offensive Guard/Crit Deco.
  // All decorations started able to contribute both skills
  // - Any remaining decorations no longer can contribute both skills because other assigned decorations capped out one or more of their skills.
  // If we removed an existing decoration, we might open up decorations that now could fully contribute. 
  // Given assigned Decos AD, and remaining decos RD, can we find a pair in RD whose skill contribution is greater than the best pair of one AD + RD?

  type DecorationContribution =
  | FullContribution of Decoration
  | PartialContribution of Decoration
  | NoContribution of Decoration

  let decorationContribution skills requestedSkills decoration = 
    let containedSkills = decoration |> containedSkills skills

    let fullyContributes ((containedSkill:Skill), (containedLevel:int)) ((requestedSkill:Skill), (requestedLevel:int)) =
      (containedSkill = requestedSkill) && (containedLevel <= requestedLevel)

    let partiallyContributes ((containedSkill:Skill), (containedLevel:int)) ((requestedSkill:Skill), (requestedLevel:int)) =
      (containedSkill = requestedSkill)

    let allContainedSkillsContribute requestedSkills containedSkills = 
      containedSkills |> List.forall (fun containedSkill -> requestedSkills |> List.exists (fullyContributes containedSkill))
    
    let someContainedSkillsContribute requestedSkills containedSkills = 
      containedSkills |> List.exists (fun containedSkill -> requestedSkills |> List.exists (partiallyContributes containedSkill))
    
    match containedSkills with
    | cs when cs |> (allContainedSkillsContribute requestedSkills) -> FullContribution decoration
    | cs when cs |> (someContainedSkillsContribute requestedSkills) -> PartialContribution decoration
    | _ -> NoContribution decoration


  let isContributingSkill (remainingSkillNeed: (Skill * int) list) (skill, level) =
    remainingSkillNeed 
    |> List.filter (fun (rSkill, rLevel) -> rSkill = skill && rLevel >= level)
    |> List.length >= 1
  
  let fullyContributes (skills: Skill list) (remainingSkillNeed: (Skill * int) list) decoration =
    let containedSkills = decoration |> containedSkills skills
    containedSkills |> List.forall (isContributingSkill remainingSkillNeed)

  let validDecoration (skills: Skill list) (remainingSkillNeed: (Skill * int) list) (Slot slotSize) (decoration: Decoration) =
    fullyContributes skills remainingSkillNeed decoration
    && decoration.Slot <= slotSize

  let findValidDecorations (skills: Skill list) (remainingSkillNeed: (Skill * int) list) (decorations: Decoration list) (Slot slotSize) = 
    let validDecorations, nonContributingDecorations = decorations |> List.partition (validDecoration skills remainingSkillNeed (Slot slotSize))
    validDecorations |> (function | [] -> None | decos -> Some (decos, nonContributingDecorations))

  let validExactDecoration (skills: Skill list) (remainingSkillNeed: (Skill * int) list) (Slot slotSize) (decoration: Decoration) =
    fullyContributes skills remainingSkillNeed decoration
    && decoration.Slot = slotSize

  let findValidExactDecorations (skills: Skill list) (remainingSkillNeed: (Skill * int) list) (decorations: Decoration list) (Slot slotSize) = 
    let validDecorations, nonContributingDecorations = decorations |> List.partition (validExactDecoration skills remainingSkillNeed (Slot slotSize))
    validDecorations |> (function | [] -> None | decos -> Some (decos, nonContributingDecorations))
  
  
  let updateRemainingSkillNeed (skills: Skill list) (newDecoration: Decoration) (skillNeed: (Skill * int) list) =
    let newlyAddedSkills = newDecoration |> containedSkills skills
    let folder rSkillNeed (skillToRemove, valueToRemove) =
      rSkillNeed |> List.map (fun (rSkill, rValue) -> if rSkill = skillToRemove then rSkill, rValue - valueToRemove else rSkill, rValue)
    newlyAddedSkills |> List.fold folder skillNeed

  open Helpers.List
  // TODO: Rename this
  let updateRemainingSkillNeedRemovedDecoration (skills: Skill list) (newDecoration: Decoration) (skillNeed: (Skill * int) list) =
    let removedSkills = newDecoration |> containedSkills skills
    let folder rSkillNeed (skillToAdd, valueToAdd) =
      rSkillNeed |> List.map (fun (rSkill, rValue) -> if rSkill = skillToAdd then rSkill, rValue + valueToAdd else rSkill, rValue)
    removedSkills |> List.fold folder skillNeed


  // A successful replacement will return Some if:
  // After removing the candidate decoration:
  // - I can choose another decoration to fully populate this slot
  // - I can choose another decoration slot, for which I can fully populate the slot
  let tryReplaceOneDecorationWithTwo skills unusedSlots unusedDecorations skillNeed (slot, decoration) : (((Slot * Decoration) * (Slot * Decoration)) * (Slot list * Decoration list * Decoration list)) option =
    let skillNeedWithoutDecoration = skillNeed |> updateRemainingSkillNeedRemovedDecoration skills decoration 

    match slot |> findValidDecorations skills skillNeedWithoutDecoration unusedDecorations with
    | None -> None
    | Some (validDecorations, invalidDecorations) ->
      let findDecorationAllowingSecondSlot remainingDecorations decoration : ((Slot * Decoration) * (Slot list * Decoration list * Decoration list)) option = 
        let skillNeedAfterFirstReplacement = skillNeedWithoutDecoration |> updateRemainingSkillNeed skills decoration
        let slotThatFitsASecondDecoration = unusedSlots |> List.tryRemoveByAndWith (findValidDecorations skills skillNeedAfterFirstReplacement remainingDecorations)
        match slotThatFitsASecondDecoration with
        | Some ((matchingSlot, unmatchingSlots), (secondValidDecoration :: otherValidDecorations, invalidDecorations2)) ->
          Some ((matchingSlot, secondValidDecoration), (unmatchingSlots, otherValidDecorations, List.concat [invalidDecorations; invalidDecorations2]))
        | _ -> None


      match validDecorations |> List.tryRemoveByAndWithR findDecorationAllowingSecondSlot with
      | Some ((firstDecoration, unmatchedDecorations), ((secondSlot, secondDecoration), (unmatchedSlots, validDecorations, invalidDecorations))) -> 
        Some (((slot, firstDecoration), (secondSlot, secondDecoration)), (unmatchedSlots, validDecorations, invalidDecorations))
      | _ -> None
      




  let assignDecorations 
    (skills : Skill list)
    (requestedSkills : (Skill * int) list) 
    (decorationSlots : Slot list)
    (decorations : (Decoration list))
    : (Slot * Decoration option) list option =

      let rec reassignDecoration' 
        (remainingSkillNeed: (Skill * int) list) 
        ((assignedDecorationSlots : (Slot * Decoration) list), (unassignedDecorationSlots : Slot list)) 
        ((fullContributingDecorations : Decoration list), (replacementDecorations : Decoration list)) 
        : (Slot * Decoration option) list option =
        match remainingSkillNeed |> List.filter (fun (skill, need) -> need > 0) with
        | [] -> 
          let assignedSlots = assignedDecorationSlots |> List.map (fun (slot, deco) -> slot, Some deco)
          let unassignedSlots = unassignedDecorationSlots |> List.map (fun slot -> slot, None)
          Some (List.concat [assignedSlots; unassignedSlots])
        | _ ->

          // We want to have a function that recursively checks until two decoration slots can be fit with new decorations at maximum contribution.
          // This function should return an option so we can use it with tryRemoveByAndWith
          let projectionForRemovedSlots = (tryReplaceOneDecorationWithTwo skills unassignedDecorationSlots replacementDecorations remainingSkillNeed)

          let maybeRemovableDecorationSlot = assignedDecorationSlots |> List.tryRemoveByAndWith projectionForRemovedSlots
          match maybeRemovableDecorationSlot with
          | Some (((replacedSlot, replacedDecoration), remainingAssignedDecorations), (((firstNewSlot, firstNewDecoration), (secondNewSlot, secondNewDecoration)), (unassignedSlots, validDecorations, invalidDecorations)) ) -> 
            let newAssignedDecorationSlots = (secondNewSlot, secondNewDecoration) :: (firstNewSlot, firstNewDecoration) :: remainingAssignedDecorations
            let newSkillNeed = 
              remainingSkillNeed 
              |> updateRemainingSkillNeed skills firstNewDecoration
              |> updateRemainingSkillNeed skills secondNewDecoration
            let stillFullyContributing, noLongerContributingAfterAssignment = validDecorations |> List.partition (fullyContributes skills newSkillNeed) 


            reassignDecoration' newSkillNeed (newAssignedDecorationSlots, unassignedSlots) (stillFullyContributing, List.concat [noLongerContributingAfterAssignment; invalidDecorations])

          | _ -> None
        

      and assignDecoration' 
        (remainingSkillNeed: (Skill * int) list) 
        ((assignedDecorationSlots : (Slot * Decoration) list), (unassignedDecorationSlots : Slot list)) 
        ((fullContributingDecorations : Decoration list), (replacementDecorations : Decoration list)) 
        : (Slot * Decoration option) list option =

        match unassignedDecorationSlots |> List.tryRemoveByAndWith (findValidDecorations skills remainingSkillNeed fullContributingDecorations) with
        | Some ((decorationSlot, remainingSlots), (fullyContributingDecoration :: otherFullyContributingDecorations, noLongerContributingDecorations)) ->
          let newlyAssignedSlot = decorationSlot, fullyContributingDecoration          
          let newRemainingSkillNeed = remainingSkillNeed |> updateRemainingSkillNeed skills fullyContributingDecoration
          let stillFullyContributing, noLongerContributingAfterAssignment = otherFullyContributingDecorations |> List.partition (fullyContributes skills newRemainingSkillNeed)
          let newReplacementDecorations = List.concat [replacementDecorations; noLongerContributingDecorations; noLongerContributingAfterAssignment]
          assignDecoration' newRemainingSkillNeed (newlyAssignedSlot :: assignedDecorationSlots, remainingSlots) (stillFullyContributing, newReplacementDecorations)

        | _ -> reassignDecoration' remainingSkillNeed (assignedDecorationSlots, unassignedDecorationSlots) (fullContributingDecorations, replacementDecorations)


      let largestDecorationSlot = decorationSlots |> List.map (fun (Slot s) -> s) |> List.max 
      let allDecorationSlotSizes = [1..largestDecorationSlot] |> List.map Slot
      let allValidDecorations, invalidDecorations = 
        allDecorationSlotSizes 
        |> List.choose (findValidExactDecorations skills requestedSkills decorations)
        |> List.unzip
        |> (fun (a,b) -> a |> List.concat, b |> List.concat)
      assignDecoration' requestedSkills ([], decorationSlots) (allValidDecorations, [])
