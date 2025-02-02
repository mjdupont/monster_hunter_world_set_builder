module GameDataTypes
open Interfaces


type Skill = 
    Skill of APIDataTypes.Skill
        with 
            interface ISkill with
                member this.SkillId = 
                    let (Skill skill) = this
                    skill.Id
                member this.Name = 
                    let (Skill skill) = this
                    skill.Name
                member this.MaxRank = 
                    let (Skill skill) = this
                    (skill.Ranks |> List.maxBy (fun (sr:APIDataTypes.SkillRank) -> sr.Level)).Level