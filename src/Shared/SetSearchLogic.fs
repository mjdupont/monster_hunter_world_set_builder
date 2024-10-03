module SetSearchLogicMod

open APIDataTypes
open Helpers
open ModelData
open DecorationAssignment
open Helpers.Prelude
open GameData.APIData
open SetSearchLogic.Interfaces
open SetSearchLogic.GameData

let armorByType armor =
    armor |> List.groupBy (fun a -> a.Type) |> Map.ofSeq

let remainingSkillNeed chosenSet (requestedSkills: ('s * int) list when Skill<'s>) =
    let achievedSkills = ChosenSet.skillCount chosenSet

    [
        for (skill, requestedLevel) in requestedSkills do
            let achievedCount =
                achievedSkills
                |> List.filter (fun (aSkill, aCount) -> aSkill = skill)
                |> List.tryExactlyOne
                |> Option.map snd
                |> Option.defaultValue 0

            let remainingNeed = requestedLevel - achievedCount
            if remainingNeed > 0 then yield (skill, remainingNeed) else ()
    ]

let charmSkillContribution remainingSkillNeed (charm:'c when Charm<'c, 's>) =
    charm |> skillContribution remainingSkillNeed

let armorSkillContribution remainingSkillNeed decorationReach (armor: 'a when Armor<'a, 's, 'd, 'sb>) : int =
    let armorContribution = armor |> skillContribution remainingSkillNeed
    let decoContribution = armor.Slots |> asCounts |> decorationReach

    armorContribution + decoContribution


let calculateReachOfChosenSet (decorations: ('d * int) list) (chosenSet:ChosenSet<'s, 'a, 'w, 'c, 'd, 'sb> ) (charms : 'c list) armorByType remainingSkillNeed =
    let getReachOfBestPiece armorType =
        armorByType
        |> Map.find armorType
        |> List.tryHead
        |> Option.map (armorSkillContribution remainingSkillNeed (SetSearchLogic.GameData.simplisticReachHeuristic))
        |> Option.defaultValue 0

    let unassignedPieces = ChosenSet.getUnassignedPieces chosenSet

    let bestUnassignedReach =
        unassignedPieces |> List.map getReachOfBestPiece |> List.sum

    let charmReach =
        charms
        |> List.tryHead
        |> Option.map (charmSkillContribution remainingSkillNeed)
        |> Option.defaultValue 0

    let assignedArmorUnassignedDecorationsReach =
        chosenSet
        |> ChosenSet.getAssignedPieces
        |> List.map (snd >> DecorationSlots.asSlots)
        |> List.concat
        |> List.filter (fun (slot, deco) -> deco |> Option.isNone)
        |> List.map fst
        |> asCounts
        |> (simplisticReachHeuristic)

    let weaponDecorationReach =
        chosenSet.Weapon
        |> Option.map (snd >> DecorationSlots.asSlots >> (List.map fst) >> asCounts)
        |> Option.defaultValue []
        |> (simplisticReachHeuristic)

    let hardDecorationReach =
        hardSkillContribution remainingSkillNeed decorations [ (Slot 4, 9) ]

    bestUnassignedReach
    + charmReach
    + assignedArmorUnassignedDecorationsReach
    + weaponDecorationReach
    + hardDecorationReach

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
        |> Option.map (fun (weapon, weaponSlots) ->
            weaponSlots |> DecorationSlots.asSlots |> List.partition (snd >> Option.isSome))
        |> Option.defaultValue ([], [])

    let unassignedSlots = unassignedArmorSlots @ unassignedWeaponSlots
    unassignedSlots


let inline itemWithEmptySlots (item: 'i when 'i :> IHasSlots<'d> ) =
    item, (item.Slots |> DecorationSlots.FromSlots)

let removeUnboundDecorations (fixedSet: ChosenSet<'s, 'a, 'w, 'c, 'd, 'sb>) (accumulatedSet: ChosenSet<'s, 'a, 'w, 'c, 'd, 'sb>) = {
    accumulatedSet with
        Headgear =
            fixedSet.Headgear
            |> Option.maybeDefaultValue (accumulatedSet.Headgear |> Option.map (fst >> itemWithEmptySlots))
        Chest =
            fixedSet.Chest
            |> Option.maybeDefaultValue (accumulatedSet.Chest |> Option.map (fst >> itemWithEmptySlots))
        Gloves =
            fixedSet.Gloves
            |> Option.maybeDefaultValue (accumulatedSet.Gloves |> Option.map (fst >> itemWithEmptySlots))
        Waist =
            fixedSet.Waist
            |> Option.maybeDefaultValue (accumulatedSet.Waist |> Option.map (fst >> itemWithEmptySlots))
        Legs =
            fixedSet.Legs
            |> Option.maybeDefaultValue (accumulatedSet.Legs |> Option.map (fst >> itemWithEmptySlots))
}


let tryRemoveLastAssignment fixedSet accumulatedSet =
    match fixedSet.Charm, accumulatedSet.Charm with
    | None, Some charm -> Some { accumulatedSet with Charm = None }
    | _ ->
        ArmorType.allTypes
        |> List.filter (fun at -> ChosenSet.tryGetPiece (at, fixedSet) |> Option.isNone)
        |> List.rev
        |> List.tryHead
        |> Option.map (fun piece -> (accumulatedSet |> ChosenSet.setArmor piece None))

module DecorationAllocation =

    let allocateToDecorationSlot assignedDecorations (decorationSlot: DecorationSlot<'d, 's>) =
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

    let allocateToDecorationSlots assignedDecorations (decorationSlots: DecorationSlots<'d, 's>) = option {
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

    let allocateDecorations chosenSet assignedDecorations : ChosenSet<'s, 'a, 'w, 'c, 'd, 'sb> option = option {
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
        | [] -> return chosenSet
        | _ -> return! None
    }

open DecorationAllocation

module Assignment =

    let tryAssignDecorations remainingSkillNeed decorations chosenSet =
        printfn "Trying to assign decorations..."

        option {
            let unassignedSlots = chosenSet |> getUnassignedSlots |> List.map fst |> asCounts

            let! decorationSolution =
                findDecorationsSatisfyingSkills remainingSkillNeed unassignedSlots decorations

            let decorationsToAllocate =
                decorationSolution
                |> List.choose (fun (s, mDeco) -> mDeco |> Option.map (fun mDec -> s, mDec))
                |> List.sortByDescending fst

            let decorations = allocateDecorations chosenSet decorationsToAllocate
            return! decorations
        }



    let tryAssignArmor (armorByType: Map<ArmorType, 'a list>) accumulatedSet =
        printfn "Trying to assign armor..."

        let lookupNextArmorPiece armorType = option {
            let! pieces = armorByType |> Map.tryFind armorType

            let! maybeChosenPieces =
                match pieces with
                | headPiece :: restPieces -> Some(headPiece, restPieces)
                | _ -> None

            return armorType, maybeChosenPieces
        }

        let maybeNextArmorPiece =
            accumulatedSet
            |> ChosenSet.getUnassignedPieces
            |> List.choose lookupNextArmorPiece
            |> List.tryHead

        match maybeNextArmorPiece with
        | Some(armorType, ((piece: 'a), rest)) ->
            let newArmorByType = armorByType |> Map.add armorType rest

            let newAccumulatedSet =
                accumulatedSet
                |> ChosenSet.setArmor armorType (Some(piece, piece.Slots |> DecorationSlots.FromSlots))

            Some(newAccumulatedSet, newArmorByType)
        | None -> None



    let tryAssignCharm charms accumulatedSet =
        printfn "Trying to assign charm..."

        match accumulatedSet.Charm, charms with
        | None, nextCharm :: remainingCharms ->
            let newAccumulatedSet = {
                accumulatedSet with
                    Charm = Some nextCharm
            }

            Some(newAccumulatedSet, remainingCharms)
        | None, [] -> None
        | Some charm, _ -> None




    let tryAssignNext remainingSkillNeed accumulatedSet armorByType charms decorations =

        let tryAssignArmor =
            tryAssignArmor armorByType >> Option.map (fun (s, a) -> (s, a, charms))

        let tryAssignCharm =
            tryAssignCharm charms >> Option.map (fun (s, c) -> (s, armorByType, c))

        let tryAssignDecorations =
            tryAssignDecorations remainingSkillNeed decorations
            >> Option.map (fun s -> s, armorByType, charms)


        accumulatedSet
        |> (tryAssignCharm
            |> Option.withFallback tryAssignArmor
            |> Option.withFallback tryAssignDecorations)

///
/// Tries to first assign armor pieces to the set that have the armor-unique skill;
/// Returns a list of valid sets satisfying armor-unique skills
///
let tryAssignArmorUniqueSkills armorByType armorSets (decorations:'d list) chosenSet (requestedSkills:('s * int) list) : ChosenSet<'s, 'a, 'w, 'c, 'd, 'sb> list =
    let partitionedSkills =
        partitionSkills armorSets decorations (requestedSkills |> List.map (fun (requestedSkill, _) -> requestedSkill))

    let armorUniqueSkills = partitionedSkills.ArmorUniqueSkills

    let inline containsUniqueSkill (uniqueSkill:'s when Skill<'s>) (equipItem:'i when 'i :> IProvidesSkills<'s>) =
        equipItem.Skills
        |> List.filter (fun (skill, _) -> skill = uniqueSkill)
        |> (not << List.isEmpty)


    // For each unique skill, get all the pieces that contain that skill.
    // Copy the chosenSet for each of those pieces that can be added to the chosenSet, and add that piece.
    // Repeat for the next skill, for all previously found chosenSets.
    let addPieceWithUniqueSkill (chosenSets: ChosenSet<'s, 'a, 'w, 'c, 'd, 'sb> list) (uniqueSkill: 's when Skill<'s>) = [
        for cSet in chosenSets do
            match (ChosenSet.getUnassignedPieces cSet) with
            | [] -> () //ChosenSet has no room for more pieces
            | unassignedPieces ->
                for unassignedPiece in unassignedPieces do
                    let piecesWithUniqueSkill =
                        Map.tryFind unassignedPiece armorByType
                        |> Option.defaultValue []
                        |> List.filter (containsUniqueSkill uniqueSkill)

                    for pieceWithUniqueSkill in piecesWithUniqueSkill do
                        yield
                            cSet
                            |> ChosenSet.setArmor
                                unassignedPiece
                                (Some(pieceWithUniqueSkill, pieceWithUniqueSkill.Slots |> DecorationSlots.FromSlots))
    ]

    armorUniqueSkills |> List.fold addPieceWithUniqueSkill [ chosenSet ]



open Assignment

let isCompleteSet requestedSkills accumulatedSet =
    let remainingSkillNeed = remainingSkillNeed accumulatedSet requestedSkills
    remainingSkillNeed |> List.isEmpty



let rec assignArmor3'
    (fixedSet: ChosenSet<'s, 'a, 'w, 'c, 'd, 'sb>)
    (decorations: ('d * int) list)
    (requestedSkills: ('s * int) list)

    (charms: 'c list)
    (armorByType: Map<ArmorType, 'a list>)
    (accumulatedSet: ChosenSet<'s, 'a, 'w, 'c, 'd, 'sb>)
    : ((ChosenSet<'s, 'a, 'w, 'c, 'd, 'sb> * Map<ArmorType, 'a list> * 'c list) option) =
    let remainingSkillNeed = remainingSkillNeed accumulatedSet requestedSkills

    let armorByType =
        armorByType
        |> Map.map (fun key armorPieces ->
            armorPieces
            |> List.sortByDescending (
                armorSkillContribution remainingSkillNeed simplisticReachHeuristic
            ))

    let charms =
        charms
        |> List.sortByDescending (charmSkillContribution remainingSkillNeed)

    let distance = remainingSkillNeed |> distance

    let reach =
        calculateReachOfChosenSet decorations accumulatedSet charms armorByType remainingSkillNeed

    //printfn "reach:%i distance:%i" reach distance

    let tryRewindSet () =
        match accumulatedSet |> tryRemoveLastAssignment fixedSet with
        | Some rewoundSet -> assignArmor3' fixedSet decorations requestedSkills charms armorByType rewoundSet
        | None -> None

    match tryAssignNext remainingSkillNeed accumulatedSet armorByType charms decorations with
    | _ when distance > reach -> tryRewindSet ()
    | None -> tryRewindSet ()
    | Some(updatedSet, remainingArmor, remainingCharms) when updatedSet |> isCompleteSet requestedSkills ->
        printfn "Found set!"
        Some(updatedSet, remainingArmor, remainingCharms)
    | Some(updatedSet, remainingArmor, remainingCharms) ->
        assignArmor3' fixedSet decorations requestedSkills remainingCharms remainingArmor updatedSet

type TemporarySetBonus<'s> =
  { Ranks : ArmorSetBonusRank<'s> list }
  with
    interface ISetBonus<'s> with
      member this.Ranks = this.Ranks
                

type TemporaryArmorSet<'sb, 's> when 'sb : equality and 'sb :> ISetBonus<'s> = 
  { ArmorSetBonus : 'sb option }
  with 
    interface IContainsSetBonus<'sb, 's> with
      member this.SetBonus = this.ArmorSetBonus

type TemporarySkill = 
  {SkillId : int}
    with 
      interface ISkill with
        member this.SkillId = this.SkillId

let assignArmor3
    n_to_find
    skills
    (chosenSet: ChosenSet<'s, 'a, 'w, 'c, 'd, 'sb>)
    (armorByType: Map<ArmorType, 'a List>)
    (charms: ('c list))
    (decorations: ('d * int) list)
    (requestedSkills: ('s * int) list)
    (armorSets: ArmorSet list)
    =

    let setsWithUniqueSkills =
        tryAssignArmorUniqueSkills armorByType armorSets (decorations |> List.map fst) chosenSet requestedSkills

    let rec assignArmor3outer accumulatedSets fixedSet workingSet' armor' charms' =
        printfn "Accumulated sets: %i" (List.length accumulatedSets)

        match assignArmor3' chosenSet decorations requestedSkills charms' armor' workingSet' with
        | Some(finishedSet, remainingArmor, remainingCharms) when
            (finishedSet :: accumulatedSets) |> List.length >= n_to_find
            ->
            printfn $"Found {n_to_find} sets!"
            finishedSet :: accumulatedSets

        | Some(finishedSet, remainingArmor, remainingCharms) ->
            printfn "Committed set: running again"
            let newAccumulatedSets = (finishedSet :: accumulatedSets)

            let nextSet =
                finishedSet
                |> (removeUnboundDecorations fixedSet)
                |> tryRemoveLastAssignment fixedSet

            match nextSet with
            | Some newWorkingSet ->
                assignArmor3outer newAccumulatedSets fixedSet newWorkingSet remainingArmor remainingCharms
            | None -> newAccumulatedSets
        | None -> accumulatedSets

    [
        for set in setsWithUniqueSkills -> assignArmor3outer [] set set armorByType charms
    ]
    |> List.concat