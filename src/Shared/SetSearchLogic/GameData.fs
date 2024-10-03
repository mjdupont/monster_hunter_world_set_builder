namespace SetSearchLogic

/// This module contains the methods dealing with the particular datatypes in GameDataTypes
module GameData =
    open SetSearchLogic.Interfaces
    open Helpers.Prelude 

    ///
    /// Check if a given item provides a given skill.
    ///
    let containsSkill (skill: 's when Skill<'s>) (item: 'o when 'o :> IProvidesSkills<'s>) =
        item.Skills |> List.filter (fun (s, l) -> s = skill) |> (not << List.isEmpty)

    ///
    /// Determines if a hard decoration exists for the given skill.
    ///
    let hardDecorationExistsForSkill (decorations: 'd list) (skill: 's) : bool when Decoration<'d, 's> and Skill<'s> =
        decorations
        |> List.filter (containsSkill skill)
        |> List.choose 
            ((fun i -> i.Skills)
            >> List.filter (fun (skill', level) -> skill= skill' && level = 3)
            >> List.tryExactlyOne
            )
        |> List.tryExactlyOne
        |> Option.isSome

    ///
    /// Calculates how much a given set of requested skills might benefit from Hard decorations.
    ///
    let hardSkillContribution
        (requestedSkills: ('s * int) list when Skill<'s>)
        (decorations: ('d * int) list when Decoration<'d, 's>)
        (slotCounts: (Slot * int) list)
        =
        let hardDecorations =
            decorations
            |> List.filter (fun (decoration, count) ->
                decoration.Skills |> List.exists (fun (skill, level) -> level = 3))

        let possibleHardContributions = [
            for (requestedSkill, level) in requestedSkills do
                for hardDecoration, count in hardDecorations do
                    if
                        hardDecoration.Skills
                        |> List.tryExactlyOne
                        |> Option.map (fun (decoSkill, level) -> requestedSkill = decoSkill)
                        |> Option.defaultValue false
                    //Note this relies on integer division truncating any fractional component
                    then
                        yield min (level / 3) count
        ]

        let nSize4Slots =
            slotCounts
            |> List.filter (fun ((Slot s), count) -> s = 4)
            |> List.tryExactlyOne
            |> Option.map (snd)
            |> Option.defaultValue 0

        (min (possibleHardContributions |> List.sum) nSize4Slots)

    type SkillCategory =
    | ArmorSetSkill
    | ArmorUniqueSkill
    | DecorationSkill
    | ArmorSetAndDecorationSkill
    | ArmorSetAndUniqueSkill

    ///
    /// Categorizes a skill as either an armor set skill, an armor-unique skill, or a decoration skill
    ///
    let categorizeSkill (armorSets: 'aset list when 'aset :> IContainsSetBonus<'sb, 's>) (decorations: 'd list when Decoration<'d, 's>) (skill: 's when Skill<'s>) =
        let skillId = skill.SkillId
        let armorSetBonusSkillIds =
            armorSets
            |> List.choose (fun aSet -> aSet.SetBonus)
            |> List.map (fun asb -> asb.Ranks)
            |> List.concat
            |> List.map (fun asbr -> asbr.Skill.SkillId)
            |> List.distinct

        let decorationSkillBonusIds =
            decorations
            |> List.map (fun deco -> deco.Skills)
            |> List.concat
            |> List.map (fst >> (fun s -> s.SkillId))
            |> List.distinct

        let isArmorSetSkill =
            armorSetBonusSkillIds |> List.exists (fun asbsi -> asbsi = skillId)

        let isDecorationSkill =
            decorationSkillBonusIds |> List.exists (fun dsbi -> dsbi = skillId)

        match isArmorSetSkill, isDecorationSkill with
        | true, true -> ArmorSetAndDecorationSkill
        | true, false -> ArmorSetSkill
        | false, true -> DecorationSkill
        | false, false -> ArmorUniqueSkill

    type PartitionedSkills<'s> when 's :> ISkill = {
        ArmorSetSkills: 's list
        DecorationSkills: 's list
        ArmorUniqueSkills: 's list
        ArmorSetAndDecorationSkills: 's list
    }

    ///
    /// Splits skills into ArmorSet skills, Armor-Unique skills, Decoration skills, ArmorSet/Decoration Skills (Mind's Eye/Ballistics, Guard Up)
    ///
    let partitionSkills (armorSets: 'aset list when ArmorSet<'aset>) decorations (skills: 's list when Skill<'s>) =
        let mapped =
            skills |> List.groupBy (categorizeSkill armorSets decorations) |> Map.ofList

        {
            ArmorSetSkills = mapped |> Map.tryFind ArmorSetSkill |> Option.defaultValue []
            DecorationSkills = mapped |> Map.tryFind DecorationSkill |> Option.defaultValue []
            ArmorUniqueSkills = mapped |> Map.tryFind ArmorUniqueSkill |> Option.defaultValue []
            ArmorSetAndDecorationSkills = mapped |> Map.tryFind ArmorSetAndDecorationSkill |> Option.defaultValue []
        }


    let slotReachHeuristic =
        function
        | Slot 4 -> 2
        | _ -> 1

    let simplisticReachHeuristic (slots: (Slot * int) seq) =
        [ for Slot s, count in slots -> 
          count * ((slotReachHeuristic (Slot s))) 
        ]
        |> List.sum