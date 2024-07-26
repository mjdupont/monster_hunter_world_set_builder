module SetSearchLogic

open DataTypes
open Helpers
open ModelData
open DecorationAssignment


let requestedSkillsDifference (requestedSkill:(Skill * int) list) (achievedSkill:(Skill * int) list) =
  [for skill, count in requestedSkill do
    let achievedCount = 
      achievedSkill |> List.filter (fun (aSkill, aCount) -> aSkill = skill)
      |> List.tryExactlyOne
      |> Option.map snd
      |> Option.defaultValue 0
    let remainingNeed = count - achievedCount
    if remainingNeed > 0 
    then yield skill, remainingNeed
    else ()
  ]

let charmSkillContribution skills remainingSkillNeed (charm:Charm) = 
  charm.Ranks |> Array.maxBy (fun (cr:CharmRank) -> cr.Level) |> skillContribution skills remainingSkillNeed

let armorSkillContribution skills decorations remainingSkillNeed (armor:Armor) =
  let armorContribution = armor |> skillContribution skills remainingSkillNeed
  let decoContribution = armor.Slots |> asCounts |> calculateReach skills decorations remainingSkillNeed
  armorContribution + decoContribution

let assignArmor skills (chosenSet:ChosenSet) (armor: Armor list) (charms : Charm list) (decorations: (Decoration * int) list) (requestedSkills:(Skill * int) list) =
  let achievedSkills = ChosenSet.skillCount skills chosenSet
  let remainingSkillNeed = requestedSkillsDifference requestedSkills achievedSkills
  
  let unassignedPieces = ChosenSet.getUnassignedPieces chosenSet
  let validArmor = armor |> List.filter (fun a -> unassignedPieces |> List.contains a.Type)
  let validArmorScored = validArmor |> List.map (fun a -> a, a |> armorSkillContribution skills decorations remainingSkillNeed) |> List.sortByDescending snd

  let validCharmsScored = charms |> List.map (fun c -> c, c |> charmSkillContribution skills remainingSkillNeed) |> List.sortByDescending snd
  
  match validArmorScored, validCharmsScored with
  | (bestArmor, armorContrib) :: restArmor, [] ->
    Some (ChosenSet.setArmor bestArmor.Type (Some (bestArmor, bestArmor.Slots |> DecorationSlots.FromSlots)) chosenSet)
  | (bestArmor, armorContrib) :: restArmor, (bestCharm, charmContrib) :: restCharm when armorContrib > charmContrib ->
    Some (ChosenSet.setArmor bestArmor.Type (Some (bestArmor, bestArmor.Slots |> DecorationSlots.FromSlots)) chosenSet)

  | [], (bestCharm, charmContrib) :: restCharm ->
    Some ({ chosenSet with Charm = Some (bestCharm, bestCharm.Ranks |> Array.maxBy (fun (cr:CharmRank) -> cr.Level))})
  | (bestArmor, armorContrib) :: restArmor, (bestCharm, charmContrib) :: restCharm when charmContrib > armorContrib -> 
    Some ({ chosenSet with Charm = Some (bestCharm, bestCharm.Ranks |> Array.maxBy (fun (cr:CharmRank) -> cr.Level))})
  | _, _ -> None

let allocateToDecorationSlot assignedDecorations (decorationSlot:DecorationSlot) =
  match decorationSlot with
  | Some (Slot s, Some decoration) -> Some (assignedDecorations, Some (Slot s, Some decoration))
  | None -> Some (assignedDecorations, None)
  | Some (Slot s, None) -> 
    match assignedDecorations |> List.partition (fun (Slot aSlot, _decoration) -> aSlot = s) with
    | (_slot, decoration) :: rest, notMatching -> Some (rest @ notMatching, Some (Slot s, Some decoration))
    | [], _ -> None

let allocateToDecorationSlots assignedDecorations (decorationSlots:DecorationSlots) = 
  option {
    let! remainingDecos, firstSlot = allocateToDecorationSlot assignedDecorations decorationSlots.First
    let! remainingDecos, secondSlot = allocateToDecorationSlot remainingDecos decorationSlots.Second
    let! remainingDecos, thirdSlot = allocateToDecorationSlot remainingDecos decorationSlots.Third
    return remainingDecos, {decorationSlots with First = firstSlot; Second = secondSlot; Third = thirdSlot}
  }

let allocateToWeapon (assignedDecorations, chosenSet)  =
  option {
    let! weapon, decorationSlots = chosenSet.Weapon
    let! remainingDecos, weaponDecorationSlots = 
      decorationSlots
      |> allocateToDecorationSlots assignedDecorations
    return remainingDecos, { chosenSet with Weapon = Some (weapon, weaponDecorationSlots) }
  }

let allocateToArmor (assignedDecorations, chosenSet) armorType =
  option {
    let! armor, decorationSlots = ChosenSet.getPiece (armorType, chosenSet)
    let! remainingDecos, assignedDecorationSlots = 
      decorationSlots
      |> allocateToDecorationSlots assignedDecorations
    return remainingDecos, ChosenSet.setArmor armorType (Some (armor, assignedDecorationSlots)) chosenSet
    }

let allocateDecorations chosenSet assignedDecorations : ChosenSet option =
  option {
    let! decorations, chosenSet = allocateToWeapon (assignedDecorations, chosenSet)
    let! decorations, chosenSet = 
      ArmorType.allTypes |> List.fold (fun state next -> state |> Option.bind (fun s -> allocateToArmor s next)) (Some (decorations, chosenSet))
    match decorations with
    | [] -> return chosenSet
    | _ -> return! None
  }

let rec findSet skills (chosenSet:ChosenSet) (armor: Armor list) (charms : Charm list) (decorations: (Decoration * int) list) (weapon: Weapon) (requestedSkills:(Skill * int) list) = 
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
    weapon.Slots 
    |> DecorationSlots.FromSlots
    |> DecorationSlots.asSlots
    |> List.partition (snd >> Option.isSome)

  let unassignedSlots = unassignedArmorSlots @ unassignedWeaponSlots

  match assignDecorations skills remainingSkillNeed (unassignedSlots |> List.map fst |> asCounts) decorations with
  | Some assignment -> 
    let assignedDecorations = 
      assignment 
      |> List.choose (fun (s, mDeco) -> mDeco |> Option.map (fun mDec -> s, mDec))
      |> List.sortByDescending fst

    match allocateDecorations chosenSet assignedDecorations with
    | Some chosenSet -> Some chosenSet
    | None -> 
      match assignArmor skills chosenSet armor charms decorations requestedSkills with
      | Some newChosenSet -> findSet skills newChosenSet armor charms decorations weapon requestedSkills
      | None -> None
    
  | None -> 
    match assignArmor skills chosenSet armor charms decorations requestedSkills with
    | Some newChosenSet -> findSet skills newChosenSet armor charms decorations weapon requestedSkills
    | None -> None
  