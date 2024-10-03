namespace SetSearchLogic

module Interfaces = 
    type Slot = Slot of int

    type ISkill = 
        abstract SkillId : int
    
    ///<summary> For the purpose of the Set Search logic, a skill only needs to provide a unique identifier.</summary>
    type Skill<'s when 's :> ISkill and 's : equality> = 's

    type IProvidesSkills<'s> when Skill<'s> = 
        abstract Skills : ('s * int) list

    type IHasSlots<'d> =
        abstract Slots : Slot list

    type IHasActiveSlots<'d> = 
        abstract ActiveSlots : (Slot * 'd option) list
        abstract SetActiveSlots : (Slot * 'd option) list -> IHasActiveSlots<'d>

    // type ISetBonus<'s> =
    //     abstract SetBonus : 's
    //     abstract RequiredPieces: int

    // type SetBonus<'sb, 's> when 'sb : equality and 'sb :> ISetBonus<'s> = 'sb

    // type IContainsSetBonus<'aset, 's> when SetBonus<', 's>= 
    //     abstract SetBonus: 's option
    //     abstract Set: 'aset 

    type IDecoration<'s> when Skill<'s> =
        inherit IProvidesSkills<'s>
        abstract Slot : Slot

    type Charm<'c, 's when Skill<'s> and 'c :> IProvidesSkills<'s>> = 'c
    
    type Decoration<'d, 's 
        when Skill<'s>
        and 'd :> IDecoration<'s>
        > = 'd

    type Armor<'a, 's, 'd
        when Skill<'s> 
        and Decoration<'d, 's> 
        and 'a :> IProvidesSkills<'s> 
        and 'a :> IHasSlots<'d> 
        // and SetBonus<'sb, 's> 
        // and 'a :> IContainsSetBonus<'sb, 's> 
        // and SetBonus<'sb, 's>
        > = 'a