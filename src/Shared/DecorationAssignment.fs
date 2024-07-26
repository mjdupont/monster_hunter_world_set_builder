module DecorationAssignment

open DataTypes
open Helpers
open FSharp.Core

///
/// Compares a SkillRank to a Skill to determine if the SkillRank is of the skill.
/// Used due to SkillRank having Id and Skill, which are different, and confusing, and prone to bugs.
///
let skillRankOfSkill (skill: Skill) (sr: SkillRank) = sr.Skill = skill.Id

///
/// Returns the skills contained in an object that contains skills, along with their levels.
///
let inline containedSkills (skills: Skill list) (skillSource:'a when 'a:(member Skills : SkillRank array)) =
    skillSource.Skills
    |> Array.choose (fun decoSr ->
        skills
        |> List.filter (fun sk -> skillRankOfSkill sk decoSr)
        |> List.tryExactlyOne
        |> Option.map (fun sk -> sk, decoSr.Level))
    |> List.ofArray

///
/// Check if a given decoration provides a given skill.
///
let decoContainsSkill (skill: Skill) (deco: Decoration) =
    deco.Skills |> Array.filter (skillRankOfSkill skill) |> (not << Array.isEmpty)

///
/// Check if a given decoration has any skill that is not the provided skill.
///
let decoContainsOtherSkill (skill: Skill) (deco: Decoration) =
    deco.Skills
    |> Array.filter (not << (skillRankOfSkill skill))
    |> (not << Array.isEmpty)

///
/// Calculates the level of a particular skill in a decoration. If the skill is not present, returns None.
///
let decoSkillLevel (skill: Skill) (deco: Decoration) =
    deco.Skills
    |> Array.filter (skillRankOfSkill skill)
    |> Array.tryExactlyOne
    |> Option.map (fun sr -> sr.Level)

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
    decorations |> List.filter (isSingletonDecoration skill) |> List.tryExactlyOne

/// Calclulates the slot size of the singleton decoration of a given skill in a list of decorations.
let decorationSlotSize decorations skill =
    singletonDecoration decorations skill |> Option.map (fun deco -> Slot deco.Slot)


let inline skillContribution skills remainingSkillNeed (skillSource:'a when 'a:(member Skills : SkillRank array)) =
    let cSkills = skillSource |> containedSkills skills

    let joined, _, _ =
        List.join (fun (rSkill, rLevel) (cSkill, cLevel) -> cSkill = rSkill) remainingSkillNeed cSkills

    joined
    |> List.map (fun ((_, rLevel), (_, cLevel)) -> min rLevel cLevel)
    |> List.sum

let inline contributes (skills: Skill list) (remainingSkillNeed: (Skill * int) list) (skillSource:'a when 'a:(member Skills : SkillRank array)) =
    skillContribution skills remainingSkillNeed skillSource > 0


let validDecoration
    (skills: Skill list)
    (remainingSkillNeed: (Skill * int) list)
    (Slot maxSizeSlot)
    (decoration: Decoration)
    =
    contributes skills remainingSkillNeed decoration
    && decoration.Slot <= maxSizeSlot

let validHardDecoration (skills: Skill list) (remainingSkillNeed: (Skill * int) list) (decoration: Decoration) =
    contributes skills remainingSkillNeed decoration
    && (decoration |> skillContribution skills remainingSkillNeed) = 3


let updateRemainingSkillNeed (skills: Skill list) (newDecoration: Decoration) (skillNeed: (Skill * int) list) =
    let newlyAddedSkills = newDecoration |> containedSkills skills

    let folder rSkillNeed (skillToRemove, valueToRemove) =
        rSkillNeed
        |> List.map (fun (rSkill, rValue) ->
            if rSkill = skillToRemove then
                rSkill, rValue - valueToRemove
            else
                rSkill, rValue)

    newlyAddedSkills
    |> List.fold folder skillNeed
    |> List.filter (fun (skill, need) -> need > 0)



let isHardSkill skills decorations skill =
    decorations
    |> List.filter (decoContainsSkill skill)
    |> List.choose (
        (containedSkills skills)
        >> List.filter (fun (skill', level) -> skill = skill' && level = 3)
        >> List.tryExactlyOne
    )
    |> List.tryExactlyOne
    |> Option.isSome

let asCounts (xs: 'a seq) = xs |> Seq.countBy id |> List.ofSeq

let asItems (xs: ('a * int) seq) = 
  [
      for x, count in xs do
          for i in 1..count -> x
  ]


let calculateReach skills decorations requestedSkills decorationSlots =
  let hardRequestedSkills =
    requestedSkills
    |> List.filter (fun (skill, count) -> skill |> (isHardSkill skills (decorations |> List.map fst)))

  let hardDecorations =
      decorations
      |> List.filter (fun (deco, count) -> validHardDecoration skills requestedSkills deco)

  let maxPossibleHardDecorations =
      hardRequestedSkills
      |> List.map (fun (skill, count) ->
          skill,
          min
              (count / 3)
              (hardDecorations
              |> List.filter (fun (deco, count) -> decoContainsSkill skill deco)
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
          (maxPossibleHardDecorations * 3) + (otherSize4Decos * 2)
      | (_, n) -> n

  decorationSlots |> List.map contributionBySize |> List.sum

let assignDecorations
    (skills: Skill list)
    (requestedSkills: (Skill * int) list)
    (decorationSlots: (Slot * int) list)
    (decorations: (Decoration * int) list)
    : (Slot * Decoration option) list option =

    //Assign decorations that aren't in paired decorations:
    let rec assignDecorationsBFrec decorationAssignments (slots: (Slot * int) list) requestedSkills decorations =
        match requestedSkills, slots, decorations with
        | [], _, _ -> Some decorationAssignments
        | _, [], _
        | _, _, [] -> None
        | requestedSkills, slots, decorations ->
            let contributions =
                decorations
                |> List.map (fun (decoration, count) ->
                    decoration, count, (skillContribution skills requestedSkills decoration))

            let maxContribution = contributions |> List.map (fun (a, b, c) -> c) |> List.max

            let reach =
                slots
                |> List.map (function
                    | (Slot 4, n) -> maxContribution * n
                    | (_, n) -> n)
                |> List.sum

            let distance = requestedSkills |> List.map snd |> List.sum

            if reach < distance then
                None
            else
                let decorations =
                    decorations
                    |> List.filter (fun (decoration, count) ->
                        if decoration.Slot = 4 then
                            skillContribution skills requestedSkills decoration >= 2
                        else
                            skillContribution skills requestedSkills decoration >= 1)
                    |> List.sortByDescending (fun (decoration, count) ->
                        skillContribution skills requestedSkills decoration)

                let maxSlotSize =
                    slots
                    |> List.sortByDescending (fun ((Slot size), _count) -> size)
                    |> List.head
                    |> fst

                let chooseDecoration =
                    fun (decoration, count) ->
                        if validDecoration skills requestedSkills maxSlotSize decoration then
                            Some decoration
                        else
                            None

                match decorations |> List.tryRemoveByAndWith chooseDecoration with
                | None -> None
                | Some(((chosenDecoration, nChosenDecorations), remainingDecorations), _projection) ->
                    match
                        option {
                            let! (chosenSlot, _nChosenSlot) =
                                slots
                                |> List.filter (fun ((Slot size), _count) -> size >= chosenDecoration.Slot)
                                |> List.sortBy (fun ((Slot size), _count) -> size)
                                |> List.tryHead

                            let remainingSlots =
                                slots
                                |> List.map (fun ((Slot s), count) ->
                                    if (Slot s) = chosenSlot then
                                        ((Slot s), count - 1)
                                    else
                                        ((Slot s), count))
                                |> List.filter (fun ((Slot s), count) -> count > 0)

                            let newDecorationAssignments =
                                (chosenSlot, Some chosenDecoration) :: decorationAssignments

                            let remainingRequestedSkills =
                                requestedSkills |> updateRemainingSkillNeed skills chosenDecoration

                            let newRemainingDecorations =
                                if nChosenDecorations <= 1 then
                                    remainingDecorations
                                else
                                    (chosenDecoration, nChosenDecorations - 1) :: remainingDecorations

                            return!
                                assignDecorationsBFrec
                                    newDecorationAssignments
                                    remainingSlots
                                    remainingRequestedSkills
                                    newRemainingDecorations
                        }
                    with
                    | Some completeAssignments -> Some completeAssignments
                    | None -> assignDecorationsBFrec decorationAssignments slots requestedSkills remainingDecorations



    let rec assignMinimalDecorations
        decorationAssignments
        slots
        (extendedSlots, reservedSlots)
        requestedSkills
        decorations
        =

        let allocateReservedSlot ((extendedSlots: Slot list), (reservedSlots: Slot list)) =
            match extendedSlots |> List.sort, reservedSlots |> List.sort with
            | [], smallest :: rest -> Some([ smallest ], rest)
            | _, [] -> None
            | smallestE :: restE, reserved ->
                match reserved |> List.partition (fun s -> s > smallestE) with
                | largerR :: restLargerR, _restSmallerR ->
                    // Swap smallest E for smallest largerR
                    Some((largerR :: restE), (smallestE :: restLargerR))
                | [], smallestSmallerR :: restSmallerR -> Some((smallestSmallerR :: (smallestE :: restE)), restSmallerR)
                | [], [] -> None //Note reserved is already checked to be non-empty above; this case should never be hit. None is still the correct answer.



        let assignment =
            assignDecorationsBFrec
                decorationAssignments
                ((extendedSlots @ slots) |> asCounts)
                requestedSkills
                decorations

        match assignment with
        | Some a -> Some(a @ (reservedSlots |> List.map (fun slot -> slot, None)))
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

    let actualReach = decorationSlots |> calculateReach skills decorations requestedSkills
    let distance = requestedSkills |> List.map snd |> List.sum

    let excessSpace = actualReach - distance
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

    assignMinimalDecorations [] unreservedSlots ([], reservedSlots) requestedSkills decorations