module GameDataParser
    open APIDataTypes
    open SetSearch.Interfaces

    let parseArmor (armor:APIDataTypes.MHWGameExcelData.Armor) : APIDataTypes.MHWGameData.Armor = 
      {
        Set = {SetId = armor.Set_Group}
        Skills = armor.Skills |> List.map (fun aSkill -> {Id = aSkill.Id}, aSkill.Level)
        Slots = armor.Slots
      }

    let parseDecoration (decoration:APIDataTypes.MHWGameExcelData.Decoration) : APIDataTypes.MHWGameData.Decoration = 
      { Skills = [decoration.Skill1; decoration.Skill2] |> List.filter (fun sk -> sk.Id <> 0) |> List.map (fun sk -> {Id = sk.Id}, sk.Level)
        Slot = APIDataTypes.Slot decoration.Size
      }

    let parseSkill (skill: APIDataTypes.MHWGameExcelData.SkillLevel) :APIDataTypes.MHWGameData.Skill = 
      { Id = skill.Id }