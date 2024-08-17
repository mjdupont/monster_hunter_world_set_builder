/// This module contains the methods dealing with the particular datatypes in GameDataTypes
module GameData
  module APIData =
    open APIDataTypes
      
    ///
    /// Compares a SkillRank to a Skill to determine if the SkillRank is of the skill.
    /// Used due to SkillRank having Id and Skill, which are different, and confusing, and prone to bugs.
    ///
    let skillRankOfSkill (skill: Skill) (sr: SkillRank) = sr.Skill = skill.Id

    ///
    /// Calculates the highest level of each skill.
    ///
    let skillCaps skills =
        skills
        |> List.map (fun skill -> skill, (skill.Ranks |> Array.map (fun sr -> sr.Level) |> Array.max))
        |> Map.ofList

    let asCounts (xs: 'a seq) = xs |> Seq.countBy id |> List.ofSeq

    let asItems (xs: ('a * int) seq) = [
        for x, count in xs do
            for i in 1..count -> x
    ]


    ///
    /// Returns the skills contained in an object that contains skills, along with their levels.
    ///
    let inline containedSkills (skills: Skill list) (skillSource: 'a when 'a: (member Skills: SkillRank array)) =
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

    ///
    /// Calclulates the slot size of the singleton decoration of a given skill in a list of decorations.
    ///
    let decorationSlotSize decorations skill =
        singletonDecoration decorations skill |> Option.map (fun deco -> Slot deco.Slot)

    let maxSkillLevelOfDecoration skills decoration =
      decoration
      |> containedSkills skills
      |> List.map (fun (skill, level) ->
          skillCaps skills
          |> Map.tryFind skill
          |> Option.defaultValue 0
          |> (fun x -> int (ceil (float x) / (float level))))
      |> List.min

    let allDecorations skills (decorations: Decoration list) =
        decorations
        |> List.map (fun decoration -> decoration, maxSkillLevelOfDecoration skills decoration)


    ///
    /// Determines if a hard decoration exists for the given skill.
    ///
    let hardDecorationExistsForSkill skills decorations skill =
        decorations
        |> List.filter (decoContainsSkill skill)
        |> List.choose (
            (containedSkills skills)
            >> List.filter (fun (skill', level) -> skill = skill' && level = 3)
            >> List.tryExactlyOne
        )
        |> List.tryExactlyOne
        |> Option.isSome

    ///
    /// Categorizes a skill as either an armor set skill, an armor-unique skill, or a decoration skill
    /// 
    let categorizeSkill armorSets (decorations:Decoration list) (skill:Skill) =
      let armorSetBonusSkillIds = 
        armorSets 
        |> List.choose (fun (aSet:ArmorSet) -> aSet.Bonus ) 
        |> List.map (fun asb -> asb.Ranks |> List.ofArray) |> List.concat
        |> List.map (fun asbr -> asbr.Skill.Skill)
        |> List.distinct
      let decorationSkillBonusIds =
        decorations 
        |> List.map (fun deco -> deco.Skills |> List.ofArray) |> List.concat
        |> List.map (fun sr -> sr.Skill)
        |> List.distinct
      
      let isArmorSetSkill = armorSetBonusSkillIds |> List.exists (fun asbsi -> asbsi = skill.Id)
      let isDecorationSkill = decorationSkillBonusIds |> List.exists (fun dsbi -> dsbi = skill.Id)

      match isArmorSetSkill, isDecorationSkill with
      | true, true -> ArmorSetAndDecorationSkill
      | true, false -> ArmorSetSkill
      | false, true -> DecorationSkill
      | false, false -> ArmorUniqueSkill

    ///
    /// Splits skills into ArmorSet skills, Armor-Unique skills, and decoration skills
    /// 
    let partitionSkills armorSets decorations skills =
      let mapped = skills |> List.groupBy (categorizeSkill armorSets decorations) |> Map.ofList
      {| ArmorSetSkills = mapped |> Map.tryFind ArmorSetSkill |> Option.defaultValue []
         DecorationSkills = mapped |> Map.tryFind DecorationSkill |> Option.defaultValue []
         ArmorUniqueSkills = mapped |> Map.tryFind ArmorUniqueSkill |> Option.defaultValue []
         ArmorSetAndDecorationSkills = mapped |> Map.tryFind ArmorSetAndDecorationSkill |> Option.defaultValue []
      |}