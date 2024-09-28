namespace SetSearchLogic

module Interfaces = 
    type Slot = Slot of int

    type ISkill = 
        abstract SkillId : int

    type SkillAndLevel = 
        { SkillID : int; 
          SkillLevel: int
        }
        interface ISkill with
            member this.SkillId = this.SkillID

    type ArmorSetBonusRank = 
        { RequiredPieces: int
          SkillId : int
        }

    type IProvidesSkills = 
        abstract Skills : SkillAndLevel list

    type IContainsSlots<'d> =
        abstract Slots : (Slot * 'd option) list
        abstract EmptySlots : Slot list

    type ISetBonus = 
        abstract Ranks : ArmorSetBonusRank list
    type SetBonus<'sb> when 'sb : equality and 'sb :> ISetBonus = 'sb

    type IContainsSetBonus<'sb> when SetBonus<'sb>= 
        abstract SetBonus: 'sb option 

    type IDecoration =
        inherit IProvidesSkills
        abstract Slot : Slot

    type Charm<'c when 'c :> IProvidesSkills> = 'c

    type Skill<'s when 's :> ISkill> = 's
    type SkillSource<'s when 's :> IProvidesSkills> = 's
    type Decoration<'d when 'd :> IDecoration> = 'd

    type Armor<'a, 'd, 'sb when 'a :> IProvidesSkills and 'a :> IContainsSlots<'d> and SetBonus<'sb> and 'a :> IContainsSetBonus<'sb>> = 'a

