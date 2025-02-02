module Interfaces

type ISkill =
    abstract SkillId : int
    abstract Name : string
    abstract MaxRank : int

type Skill<'skill when 'skill :> ISkill and 'skill : comparison> = 'skill