module DecorationAssignment

open APIDataTypes
open Helpers
open FSharp.Core
open GameData.APIData

open SetSearchLogic.Interfaces

let inline skillContribution (remainingSkillNeed: ('s * int) list) (skillSource: 'ss) : int 
    when Skill<'s> 
    and 'ss :> IProvidesSkills<'s>
    =
    let joined, _, _ =
        List.join (fun (requestedSkill, _) (containedSkill, _) -> containedSkill = requestedSkill) remainingSkillNeed skillSource.Skills

    joined
    |> List.map (fun ((_, requestedLevel), (_, containedLevel)) -> min requestedLevel containedLevel)
    |> List.sum

let inline contributes
    (remainingSkillNeed: ('s * int) list )
    (skillSource: 'ss)
    : bool when Skill<'s>
    and 'ss :> IProvidesSkills<'s>
    =
    skillContribution remainingSkillNeed skillSource > 0


let validDecoration
    (remainingSkillNeed: ('s * int) list when Skill<'s>)
    maxSizeSlot
    (decoration: 'd when Decoration<'d, 's>)
    =
    contributes remainingSkillNeed decoration
    && decoration.Slot <= maxSizeSlot

let validHardDecoration (remainingSkillNeed: ('s * int) list when Skill<'s>) (decoration: 'd when Decoration<'d, 's>) =
    contributes remainingSkillNeed decoration
    && (decoration |> skillContribution remainingSkillNeed) = 3


let addSkillsToRequestedSkills (skillNeed: ('s * int) list when Skill<'s>) (newSkills: ('s * int) list when Skill<'s>) =

    let folder skillNeed (skillToRemove, valueToRemove) =
        [ for (rSkid, rSlvl) as requestedSkill in skillNeed do
          match rSkid = skillToRemove, rSlvl - valueToRemove with 
          | true, remainingSkill when remainingSkill > 0 ->
              yield (rSkid, (rSlvl - valueToRemove))
          | true, remainingSkill when remainingSkill <= 0 ->
              () // Remove skills whose value is reduced below 0
          | false, _ -> yield requestedSkill
          | _ -> yield requestedSkill
        ]

    newSkills
    |> List.fold folder skillNeed
    |> List.filter (fun (skill, need) -> need > 0)

let addSkillsFromItem (newItem: 'd when 'd :> IProvidesSkills<'s>) (skillNeed: ('s * int) list when Skill<'s>) =
    addSkillsToRequestedSkills skillNeed newItem.Skills


let optimalDecorationReach (decorations: ('d * int) list when Decoration<'d, 's>) (requestedSkills: ('s * int) list when Skill<'s>) decorationSlots =
    let skillreach = simplisticReachHeuristic decorationSlots
    let hardContribution = SetSearchLogic.GameData.hardSkillContribution requestedSkills decorations decorationSlots
    
    let hardRequestedSkills =
        requestedSkills
        |> List.filter (fun (requestedSkill, _) -> (SetSearchLogic.GameData.hardDecorationExistsForSkill (decorations |> List.map fst) requestedSkill))

    let hardDecorations =
        decorations
        |> List.filter (fun (deco, count) -> validHardDecoration requestedSkills deco)

    let maxPossibleHardDecorations =
        hardRequestedSkills
        |> List.map (fun (skill, count) ->
            skill,
            min
                (count / 3)
                (hardDecorations
                 |> List.filter (fun (deco, count) -> SetSearchLogic.GameData.containsSkill skill deco)
                 |> List.tryExactlyOne
                 |> Option.map snd
                 |> Option.defaultValue 0))
        |> List.map snd
        |> List.sum

    let contributionBySize =
        function
        | Slot 4, count ->
            let maxPossibleHardDecos = (min maxPossibleHardDecorations count)
            let otherSize4Decos = count - maxPossibleHardDecos
            (maxPossibleHardDecos * 3) + (otherSize4Decos * 2)
        | (_, n) -> n

    decorationSlots |> List.map contributionBySize |> List.sum

let distance (requestedSkills: ('s * int) list when Skill<'s>) =
    requestedSkills |> List.map (fun (skill, level) -> level) |> List.sum

let reachHeuristic requestedSkills (decorations: ('d * int) list when 'd :> Decoration<'d, 's>) slots =
    let maxContribution =
        decorations
        |> List.map (fun (decoration, count) -> (skillContribution requestedSkills decoration))
        |> List.tryMax
        |> Option.defaultValue 1

    slots
    |> List.map (function
        | (Slot 4, n) -> maxContribution * n
        | (_, n) -> n)
    |> List.sum

let findDecorationsSatisfyingSkills
    (requestedSkills: ('s * int) list when Skill<'s>)
    (decorationSlots: (Slot * int) list)
    (decorations: ('d * int) list)
    : (Slot * 'd option) list option when Decoration<'d, 's> and Skill<'s>=

    let rec assignDecorationsDFS decorationAssignments (unassignedSlots: (Slot * int) list) requestedSkills (decorations: ('d * int) list when Decoration<'d, 's>) =
        match requestedSkills, unassignedSlots, decorations with
        | [], _, _ -> Some decorationAssignments
        | _, [], _
        | _, _, [] -> None
        | requestedSkills, unassignedSlots, decorations ->

            let reachEstimate =
                reachHeuristic requestedSkills decorations unassignedSlots

            let remainingDistance = requestedSkills |> distance

            if reachEstimate < remainingDistance then
                None
            else
                // Restructure this to avoid sorting each iteration
                let decorations =
                    decorations
                    |> List.filter (fun (decoration, count) ->
                        if decoration.Slot = Slot 4 then
                            skillContribution requestedSkills decoration >= 2
                        else
                            skillContribution requestedSkills decoration >= 1)
                    |> List.sortByDescending (fun (decoration, count) ->
                        skillContribution requestedSkills decoration)

                let maxSlotSize =
                    unassignedSlots
                    |> List.sortByDescending (fun ((Slot size), _count) -> size)
                    |> List.head
                    |> fst

                let chooseDecoration =
                    fun (decoration, count) ->
                        if validDecoration requestedSkills maxSlotSize decoration then
                            Some decoration
                        else
                            None

                match decorations |> List.tryRemoveByAndWith chooseDecoration with
                | None -> None
                | Some(((chosenDecoration, nChosenDecorations), remainingDecorations), _projection) ->
                    match
                        option {
                            let! (chosenSlot, _nChosenSlot) =
                                unassignedSlots
                                |> List.filter (fun (size, _count) -> size >= chosenDecoration.Slot)
                                |> List.sortBy (fun ((Slot size), _count) -> size)
                                |> List.tryHead

                            let remainingSlots =
                                unassignedSlots
                                |> List.map (fun ((Slot s), count) ->
                                    if (Slot s) = chosenSlot then
                                        ((Slot s), count - 1)
                                    else
                                        ((Slot s), count))
                                |> List.filter (fun ((Slot s), count) -> count > 0)

                            let newDecorationAssignments =
                                (chosenSlot, Some chosenDecoration) :: decorationAssignments

                            let remainingRequestedSkills =
                                requestedSkills |> addSkillsFromItem chosenDecoration

                            let newRemainingDecorations =
                                if nChosenDecorations <= 1 then
                                    remainingDecorations
                                else
                                    (chosenDecoration, nChosenDecorations - 1) :: remainingDecorations

                            return!
                                assignDecorationsDFS
                                    newDecorationAssignments
                                    remainingSlots
                                    remainingRequestedSkills
                                    newRemainingDecorations
                        }
                    with
                    | Some completeAssignments -> Some completeAssignments
                    | None -> assignDecorationsDFS decorationAssignments unassignedSlots requestedSkills remainingDecorations



    let rec assignMinimalDecorations
        (decorationAssignments : (Slot * 'd option) list)
        slots
        (extendedSlots, reservedSlots)
        requestedSkills
        (decorations: ('d * int) list when Decoration<'d, 's> and Skill<'s>)
        : (Slot * 'd option) list option
        =

        let allocateReservedSlot ((extendedSlots: Slot list), (reservedSlots: Slot list)) =
            match extendedSlots |> List.sort, reservedSlots |> List.sort with
            | [], smallest :: rest -> Some([ smallest ], rest)
            | _, [] -> None
            | smallestE :: restE, reserved ->
                match reserved |> List.partition (fun s -> s > smallestE) with
                | largerR :: restLargerR, _restSmallerR ->
                    // Swap smallest E for smallest larger R
                    Some((largerR :: restE), (smallestE :: restLargerR))
                | [], smallestSmallerR :: restSmallerR -> Some((smallestSmallerR :: (smallestE :: restE)), restSmallerR)
                | [], [] -> None //Note reserved is already checked to be non-empty above; this case should never be hit. None is still the correct answer.



        let attemptedAssignment =
            assignDecorationsDFS decorationAssignments ((extendedSlots @ slots) |> asCounts) requestedSkills decorations

        match attemptedAssignment with
        | Some a -> Some (a @ (reservedSlots |> List.map (fun slot -> slot, None)))
        | None ->
            match allocateReservedSlot (extendedSlots, reservedSlots) with
            | Some(newExtendedSlots, newReservedSlots) ->
                assignMinimalDecorations
                    decorationAssignments
                    slots
                    (newExtendedSlots, newReservedSlots)
                    requestedSkills
                    decorations
            | None -> None




    // Shrink the set of available slots to the minimum, and expand as needed.

    let actualReach =
        decorationSlots |> optimalDecorationReach decorations requestedSkills

    let remainingDistance = requestedSkills |> distance

    let excessSpace = actualReach - remainingDistance
    let largeSlots = excessSpace / 2
    let smallSlots = excessSpace % 2

    let decorationSlotsNonCount = decorationSlots |> asItems

    let fold4s ((unreservedSlots, reservedSlots, rest) as state) slot =
        match slot with
        | Slot 4 when rest > 0 -> (unreservedSlots, slot :: reservedSlots, rest - 1)
        | _ -> (slot :: unreservedSlots, reservedSlots, rest)

    let fold1s ((unreservedSlots, reservedSlots, rest) as state) slot =
        match slot with
        | Slot s when rest > 0 && s < 4 -> (unreservedSlots, slot :: reservedSlots, rest - 1)
        | _ -> (slot :: unreservedSlots, reservedSlots, rest)

    let unreservedSlots, reservedSlots, rest =
        decorationSlotsNonCount |> List.fold fold4s ([], [], largeSlots)

    let smallSlots = (rest * 2) + smallSlots

    let unreservedSlots, reservedSlots', rest =
        unreservedSlots |> List.fold fold1s ([], [], smallSlots)

    let reservedSlots = reservedSlots @ reservedSlots'

    let output =
        assignMinimalDecorations [] unreservedSlots ([], reservedSlots) requestedSkills decorations

    output