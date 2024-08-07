module SetSearchLogic

open GameDataTypes
open Helpers
open ModelData
open DecorationAssignment
open Helpers.Prelude

let armorByType armor = 
  armor |> List.groupBy (fun a -> a.Type) |> Map.ofSeq

let requestedSkillsDifference (requestedSkill: (Skill * int) list) (achievedSkill: (Skill * int) list) = [
    for skill, count in requestedSkill do
        let achievedCount =
            achievedSkill
            |> List.filter (fun (aSkill, aCount) -> aSkill = skill)
            |> List.tryExactlyOne
            |> Option.map snd
            |> Option.defaultValue 0

        let remainingNeed = count - achievedCount
        if remainingNeed > 0 then yield skill, remainingNeed else ()
]

let charmSkillContribution skills remainingSkillNeed (charm: Charm) =
    charm.Ranks
    |> Array.maxBy (fun (cr: CharmRank) -> cr.Level)
    |> skillContribution skills remainingSkillNeed

let armorSkillContribution skills decorations remainingSkillNeed (armor: Armor) =
    let armorContribution = armor |> skillContribution skills remainingSkillNeed

    let optimalDecorationReach' = memoize optimalDecorationReach skills decorations remainingSkillNeed

    let decoContribution =
        armor.Slots |> asCounts |> optimalDecorationReach'

    armorContribution + decoContribution

let orderAndPruneArmor skills remainingSkillNeed decorations (armor:Armor list) = 
  let optimalDecorationReach' = memoize optimalDecorationReach skills decorations remainingSkillNeed
  
  let armorWithNeededSkills, armorWithOnlyDecorations = armor |> List.partition (fun piece -> piece |> skillContribution skills remainingSkillNeed > 0)
  //TODO: This heuristic could be looked at again.
  let bestArmorWithOnlyDecorations = 
    armorWithOnlyDecorations 
    |> List.sortByDescending (fun pc -> pc.Slots |> asCounts |> optimalDecorationReach', pc.Defense)
    |> List.tryHead
    |> Option.map (fun x -> [x])
    |> Option.defaultValue []
  
  armorWithNeededSkills @ bestArmorWithOnlyDecorations

let skillContributionFromPiece skills remainingSkillNeed (armor, decorationSlots:DecorationSlots) =
  (armor |> skillContribution skills remainingSkillNeed) 
  + 
  ((DecorationSlots.skillsFromDecorationSlots decorationSlots) |> Array.sumBy (fun (sr:SkillRank) -> sr.Level))

let orderAndPruneArmorByType chosenSet skills remainingSkillNeed decorations charms armorByType =
  let prunedArmor = armorByType |> Map.map (fun armorType armorPieces -> armorPieces |> orderAndPruneArmor skills remainingSkillNeed decorations)
  let distance = remainingSkillNeed |> distance
  
  let maxLevelsInArmor = 
    prunedArmor 
    |> Map.map (fun (armorType:ArmorType) armorPieces -> 
        let highestPieceInArmorMap = 
          armorPieces 
          |> List.map (fun armor -> armor |> armorSkillContribution skills decorations remainingSkillNeed)
          |> List.sortDescending
          |> List.tryHead |> Option.defaultValue 0
        ChosenSet.tryGetPiece(armorType, chosenSet) 
        |> Option.map (skillContributionFromPiece skills remainingSkillNeed) 
        |> Option.defaultValue highestPieceInArmorMap
        )
  let maxCharmContrib = charms |> List.map (charmSkillContribution skills remainingSkillNeed) |> List.tryMax |> Option.defaultValue 0
  let reachIfAllOtherPiecesAreBest = 
    [ for at in ArmorType.allTypes -> 
      at, 
      (ArmorType.allTypes 
      |> List.filter (fun at' -> at' <> at) 
      |> List.map (fun at'' -> Map.tryFind at'' maxLevelsInArmor |> Option.defaultValue 0)
      |> List.sum) + maxCharmContrib
    ] |> Map.ofList
  let prunedArmor2 = 
    prunedArmor 
    |> Map.map (fun at armorPieces -> armorPieces |> List.filter (fun ap -> (ap |> armorSkillContribution skills decorations remainingSkillNeed) + (reachIfAllOtherPiecesAreBest |> Map.find at) > 0) )
  prunedArmor2
  

let rec assignArmor
    skills
    (chosenSet: ChosenSet)
    (armorByType: Map<ArmorType, Armor List>)
    (charms: Charm list)
    (decorations: (Decoration * int) list)
    (requestedSkills: (Skill * int) list)
    =
    printfn "Assigning Armor Piece"
    let achievedSkills = ChosenSet.skillCount skills chosenSet
    let remainingSkillNeed = requestedSkillsDifference requestedSkills achievedSkills
    printfn "Remaining Skill Need: %A" remainingSkillNeed

    let unassignedPieces = ChosenSet.getUnassignedPieces chosenSet
    let setPiece armorType (armor:Armor) = ChosenSet.setArmor armorType (Some (armor, DecorationSlots.FromSlots armor.Slots)) chosenSet
    
    let newArmorByType = armorByType |> orderAndPruneArmorByType chosenSet skills remainingSkillNeed decorations charms

    let candidateAssignedPiece = unassignedPieces |> List.tryHead |> Option.bind (fun armorType -> newArmorByType |> Map.tryFind armorType |> Option.map (fun pcs -> armorType, pcs))
    printfn "Assigning Piece: %A" candidateAssignedPiece

    match candidateAssignedPiece, chosenSet.Charm with
    | Some (armorType, armorForThisSlot), _ ->

      let validArmorScored =
          armorForThisSlot
          |> List.map (fun a -> a, a |> armorSkillContribution skills decorations remainingSkillNeed)
          |> List.sortByDescending snd
      
      let newChosenSet = (validArmorScored |> List.head |> fst |> setPiece armorType)
      printfn "Assigning Next Piece..."
      assignArmor skills newChosenSet newArmorByType charms decorations requestedSkills 

    | None, None -> // Charm
      printfn "Assigning Charm..."
      let scoredCharms = 
        if chosenSet.Charm |> Option.isSome
        then []
        else
          charms
          |> List.map (fun c -> c, c |> charmSkillContribution skills remainingSkillNeed)
          |> List.sortByDescending snd
      match scoredCharms |> List.tryHead with
      | Some (charm, _) -> Some ({chosenSet with Charm = Some (charm, charm.Ranks |> Array.maxBy (fun cr -> cr.Level))})
      | _ -> None
    
    | None, Some charm ->
      printfn "Done"
      Some chosenSet

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

let tryAssignDecorations skills remainingSkillNeed unassignedSlots decorations chosenSet =
  option {
    let! decorationSolution = findDecorationsSatisfyingSkills skills remainingSkillNeed unassignedSlots decorations
    let decorationsToAllocate = 
      decorationSolution 
      |> List.choose (fun (s, mDeco) -> mDeco |> Option.map (fun mDec -> s, mDec))
      |> List.sortByDescending fst

    return! allocateDecorations chosenSet decorationsToAllocate
  }

let rec findSet
    // Not accumulated
    skills
    (decorations: (Decoration * int) list)
    (weapon: Weapon, weaponSlots: DecorationSlots)
    (requestedSkills: (Skill * int) list)

    // Accumulated
    (chosenSet: ChosenSet)
    (armorByType: Map<ArmorType, Armor list>)
    (charms: Charm list)
    =
    let achievedSkills = ChosenSet.skillCount skills chosenSet
    let remainingSkillNeed = requestedSkillsDifference requestedSkills achievedSkills
    let _assignedArmorSlots, unassignedArmorSlots =
        chosenSet
        |> ChosenSet.getAssignedPieces
        |> List.map snd
        |> List.map DecorationSlots.asSlots
        |> List.concat
        |> List.partition (snd >> Option.isSome)

    let _assignedWeaponSlots, unassignedWeaponSlots =
        weaponSlots |> DecorationSlots.asSlots |> List.partition (snd >> Option.isSome)

    let unassignedSlots = unassignedArmorSlots @ unassignedWeaponSlots

    printfn "Remaining skill need: %A" remainingSkillNeed
    printfn "Empty slots: %A" unassignedSlots
    printfn "ChosenSet: %A" chosenSet

    match tryAssignDecorations skills remainingSkillNeed (unassignedSlots |> List.map fst |> asCounts) decorations chosenSet with
    | Some chosenSet ->
      Some (chosenSet, armorByType, charms)
    | None ->
        match assignArmor skills chosenSet armorByType charms decorations requestedSkills with
        | Some newChosenSet ->
            findSet skills decorations (weapon, weaponSlots) requestedSkills newChosenSet armorByType charms
        | None -> None