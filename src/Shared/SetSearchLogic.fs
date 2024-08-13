module SetSearchLogic

open GameDataTypes
open Helpers
open ModelData
open DecorationAssignment
open Helpers.Prelude

let armorByType armor = 
  armor |> List.groupBy (fun a -> a.Type) |> Map.ofSeq

let remainingSkillNeed skills chosenSet (requestedSkill: (Skill * int) list)= 
  let achievedSkills = ChosenSet.skillCount skills chosenSet
  [
    for skill, count in requestedSkill do
        let achievedCount =
            achievedSkills
            |> List.filter (fun (aSkill, aCount) -> aSkill = skill)
            |> List.tryExactlyOne
            |> Option.map snd
            |> Option.defaultValue 0

        let remainingNeed = count - achievedCount
        if remainingNeed > 0 then yield skill, remainingNeed else ()
  ]

let charmSkillContribution skills remainingSkillNeed (charm, charmRank) =
    charmRank |> skillContribution skills remainingSkillNeed

let armorSkillContribution skills decorations remainingSkillNeed decorationReach (armor: Armor) =
    let armorContribution = armor |> skillContribution skills remainingSkillNeed
    let decoContribution =
        armor.Slots |> asCounts |> decorationReach

    armorContribution + decoContribution


let calculateReachOfChosenSet skills decorations chosenSet charms armorByType remainingSkillNeed = 
    let getReachOfBestPiece armorType = 
      
      armorByType 
      |> Map.find armorType 
      |> List.tryHead 
      |> Option.map (armorSkillContribution skills decorations remainingSkillNeed (optimalDecorationReach skills decorations remainingSkillNeed))
      |> Option.defaultValue 0 

    let unassignedPieces = ChosenSet.getUnassignedPieces chosenSet
    let bestUnassignedReach = unassignedPieces |> List.map getReachOfBestPiece |> List.sum
    let charmReach = charms |> List.tryHead |> Option.map (charmSkillContribution skills remainingSkillNeed) |> Option.defaultValue 0
    
    let assignedArmorUnassignedDecorationsReach = 
      chosenSet 
      |> ChosenSet.getAssignedPieces
      |> List.map (snd >> DecorationSlots.asSlots)
      |> List.concat
      |> List.filter (fun (slot, deco) -> deco |> Option.isNone)
      |> List.map fst
      |> asCounts
      |> (optimalDecorationReach skills decorations remainingSkillNeed)

    bestUnassignedReach + charmReach + assignedArmorUnassignedDecorationsReach

let getUnassignedSlots accumulatedSet = 
    let _assignedArmorSlots, unassignedArmorSlots =
      accumulatedSet
      |> ChosenSet.getAssignedPieces
      |> List.map snd
      |> List.map DecorationSlots.asSlots
      |> List.concat
      |> List.partition (snd >> Option.isSome)
    
    let _assignedWeaponSlots, unassignedWeaponSlots =
        accumulatedSet.Weapon 
        |> Option.map (fun (weapon, weaponSlots) -> weaponSlots |> DecorationSlots.asSlots |> List.partition (snd >> Option.isSome))
        |> Option.defaultValue ([], [])

    let unassignedSlots = unassignedArmorSlots @ unassignedWeaponSlots
    unassignedSlots


let inline itemWithEmptySlots (item: 'a when 'a : (member Slots: Slot array)) = 
  item, (item.Slots |> DecorationSlots.FromSlots)



let removeUnboundDecorations (fixedSet:ChosenSet) (accumulatedSet:ChosenSet) = 
  { accumulatedSet with 
      Headgear = fixedSet.Headgear |> Option.maybeDefaultValue (accumulatedSet.Headgear |> Option.map (fst >> itemWithEmptySlots))
      Chest = fixedSet.Chest |> Option.maybeDefaultValue (accumulatedSet.Chest |> Option.map (fst >> itemWithEmptySlots))
      Gloves = fixedSet.Gloves |> Option.maybeDefaultValue (accumulatedSet.Gloves |> Option.map (fst >> itemWithEmptySlots))
      Waist = fixedSet.Waist |> Option.maybeDefaultValue (accumulatedSet.Waist |> Option.map (fst >> itemWithEmptySlots))
      Legs = fixedSet.Legs |> Option.maybeDefaultValue (accumulatedSet.Legs |> Option.map (fst >> itemWithEmptySlots))    
  }


let tryRemoveLastAssignment fixedSet accumulatedSet = 
  match fixedSet.Charm, accumulatedSet.Charm with
  | None, Some charm -> Some {accumulatedSet with Charm = None}
  | _ ->
      ArmorType.allTypes 
      |> List.filter (fun at -> ChosenSet.tryGetPiece (at, fixedSet) |> Option.isNone)
      |> List.rev
      |> List.tryHead
      |> Option.map (fun piece -> (accumulatedSet |> ChosenSet.setArmor piece None))


let simplisticReachHeuristic (slots: (Slot * int) seq) = 
  [for Slot s, count in slots ->
    let decoSlotWeight = 
      match s with
      | 4 -> 2
      | 3 -> 1
      | 2 -> 1
      | 1 -> 1
      | _ -> 0
    count * decoSlotWeight
  ] |> List.sum


module DecorationAllocation = 

  let allocateToDecorationSlot assignedDecorations (decorationSlot: DecorationSlot) =
      match decorationSlot with
      | Some(Slot s, Some decoration) -> Some(assignedDecorations, Some(Slot s, Some decoration))
      | None -> Some(assignedDecorations, None)
      | Some(Slot s, None) ->
          match
              assignedDecorations
              |> List.partition (fun (Slot aSlot, _decoration) -> aSlot = s)
          with
          | (_slot, decoration) :: rest, notMatching -> Some(rest @ notMatching, Some(Slot s, Some decoration))
          | [], _ -> Some(assignedDecorations, decorationSlot)

  let allocateToDecorationSlots assignedDecorations (decorationSlots: DecorationSlots) = option {
      let! remainingDecos, firstSlot = allocateToDecorationSlot assignedDecorations decorationSlots.First
      let! remainingDecos, secondSlot = allocateToDecorationSlot remainingDecos decorationSlots.Second
      let! remainingDecos, thirdSlot = allocateToDecorationSlot remainingDecos decorationSlots.Third
      
      return
          remainingDecos,
          {
              decorationSlots with
                  First = firstSlot
                  Second = secondSlot
                  Third = thirdSlot
          }
  }

  let allocateToWeapon (assignedDecorations, chosenSet) = option {
      let! weapon, decorationSlots = chosenSet.Weapon
      let! remainingDecos, weaponDecorationSlots = decorationSlots |> allocateToDecorationSlots assignedDecorations

      return
          remainingDecos,
          {
              chosenSet with
                  Weapon = Some(weapon, weaponDecorationSlots)
          }
  }

  let allocateToArmor (assignedDecorations, chosenSet) armorType = option {
      let! armor, decorationSlots = ChosenSet.tryGetPiece (armorType, chosenSet) // TODO: If we don't find armor here, we don't actually want to fail
      let! remainingDecos, assignedDecorationSlots = decorationSlots |> allocateToDecorationSlots assignedDecorations
      return remainingDecos, ChosenSet.setArmor armorType (Some(armor, assignedDecorationSlots)) chosenSet
  }

  let allocateDecorations chosenSet assignedDecorations : ChosenSet option = option {
      let! decorations, chosenSet = allocateToWeapon (assignedDecorations, chosenSet)
      let chosenPieces, emptyPieces =
          ArmorType.allTypes
          |> List.partition (fun armorType -> ChosenSet.tryGetPiece (armorType, chosenSet) |> Option.isSome)
      
      let! decorations, chosenSet =
          chosenPieces
          |> List.fold
              (fun state next -> state |> Option.bind (fun s -> allocateToArmor s next))
              (Some(decorations, chosenSet))
      match decorations with
      | [] -> 
        return chosenSet
      | _ -> 
        return! None
  }
open DecorationAllocation

let tryAssignDecorations skills remainingSkillNeed decorations chosenSet =
  option {
    let unassignedSlots = chosenSet |> getUnassignedSlots |> List.map fst |> asCounts
    let! decorationSolution = findDecorationsSatisfyingSkills skills remainingSkillNeed unassignedSlots decorations
    let decorationsToAllocate = 
      decorationSolution 
      |> List.choose (fun (s, mDeco) -> mDeco |> Option.map (fun mDec -> s, mDec))
      |> List.sortByDescending fst

    return! allocateDecorations chosenSet decorationsToAllocate
  }



let tryAssignArmor (armorByType: Map<ArmorType, Armor List>) accumulatedSet =
  let lookupNextArmorPiece armorType = option {
    let! pieces = armorByType |> Map.tryFind armorType
    let! maybeChosenPieces = 
      match pieces with 
      | headPiece :: restPieces -> Some (headPiece, restPieces)
      | _ -> None
    return armorType, maybeChosenPieces
  }
  let maybeNextArmorPiece = 
    accumulatedSet 
    |> ChosenSet.getUnassignedPieces
    |> List.choose lookupNextArmorPiece
    |> List.tryHead
  
  match maybeNextArmorPiece with
  | Some (armorType, ((piece:Armor), rest)) ->
    let newArmorByType = armorByType |> Map.add armorType rest
    let newAccumulatedSet = accumulatedSet |> ChosenSet.setArmor armorType (Some (piece, piece.Slots |> DecorationSlots.FromSlots))
    
    Some (newAccumulatedSet, newArmorByType)
  | None -> None



let tryAssignCharm charms accumulatedSet = 
  match accumulatedSet.Charm, charms with
  | None, nextCharm :: remainingCharms ->
    let newAccumulatedSet = {accumulatedSet with Charm = Some nextCharm}
    Some (newAccumulatedSet, remainingCharms)
  | None, [] -> None
  | Some charm, _ -> None




let tryAssignNext skills remainingSkillNeed accumulatedSet armorByType charms decorations =

  let tryAssignArmor = tryAssignArmor armorByType >> Option.map (fun (s, a) -> (s, a, charms))
  let tryAssignCharm = tryAssignCharm charms >> Option.map (fun (s, c) -> (s, armorByType, c))
  let tryAssignDecorations = tryAssignDecorations skills remainingSkillNeed decorations >> Option.map (fun s -> s, armorByType, charms)
  

  accumulatedSet 
  |> (tryAssignArmor |> Option.withFallback tryAssignCharm |> Option.withFallback tryAssignDecorations)


let isCompleteSet skills requestedSkills accumulatedSet =
  let remainingSkillNeed = remainingSkillNeed skills accumulatedSet requestedSkills
  remainingSkillNeed |> List.isEmpty

let rec assignArmor3' 
      skills
      (fixedSet: ChosenSet)
      (decorations: (Decoration * int) list)
      (requestedSkills: (Skill * int) list)
      
      (charms: (Charm * CharmRank) list)
      (armorByType: Map<ArmorType, Armor List>)
      (accumulatedSet : ChosenSet)
      : ((ChosenSet * Map<ArmorType, Armor List> * (Charm * CharmRank) list) option)
      =
      let remainingSkillNeed = remainingSkillNeed skills accumulatedSet requestedSkills

      let optimalDecorationReach' = memoize optimalDecorationReach skills decorations remainingSkillNeed
      let armorByType = armorByType |> Map.map (fun key armorPieces -> armorPieces |> List.sortByDescending (armorSkillContribution skills decorations remainingSkillNeed simplisticReachHeuristic))
      let charms = charms |> List.sortByDescending (charmSkillContribution skills remainingSkillNeed)
      let distance = remainingSkillNeed |> distance
      let reach = calculateReachOfChosenSet skills decorations accumulatedSet charms armorByType remainingSkillNeed 

      let tryRewindSet () = 
        match accumulatedSet |> tryRemoveLastAssignment fixedSet with
        | Some rewoundSet ->
          assignArmor3' skills fixedSet decorations requestedSkills charms armorByType rewoundSet
        | None -> 
          None

      match tryAssignNext skills remainingSkillNeed accumulatedSet armorByType charms decorations with
      | _ when distance > reach -> 
        tryRewindSet ()
      | None -> 
        tryRewindSet ()
      | Some (updatedSet, remainingArmor, remainingCharms) when updatedSet |> isCompleteSet skills requestedSkills -> 
        Some (updatedSet, remainingArmor, remainingCharms)
      | Some (updatedSet, remainingArmor, remainingCharms) -> 
        assignArmor3' skills fixedSet decorations requestedSkills remainingCharms remainingArmor updatedSet


let assignArmor3 
    skills
    (chosenSet: ChosenSet)
    (armorByType: Map<ArmorType, Armor List>)
    (charms: (Charm * CharmRank) list)
    (decorations: (Decoration * int) list)
    (requestedSkills: (Skill * int) list)
    =

    // Preprocess armorByType, charms
    // Identify Set Skills, choose combinations of pieces from those sets, fix these pieces 
    // Iterate on armor assignments for each of these subsets

    let rec assignArmor3outer accumulatedSets fixedSet workingSet' armor' charms' = 
      match assignArmor3' skills chosenSet decorations requestedSkills charms' armor' workingSet' with
      | Some (finishedSet, remainingArmor, remainingCharms) when accumulatedSets |> List.length >= 9 ->
        finishedSet :: accumulatedSets
      | Some (finishedSet, remainingArmor, remainingCharms) -> 
        let nextSet = finishedSet |> (removeUnboundDecorations fixedSet) |> tryRemoveLastAssignment fixedSet
        match nextSet with
        | Some newWorkingSet -> 
          assignArmor3outer (finishedSet :: accumulatedSets) fixedSet newWorkingSet remainingArmor remainingCharms 
        | None -> accumulatedSets
      | None -> accumulatedSets

    assignArmor3outer [] chosenSet chosenSet armorByType charms
         

   
