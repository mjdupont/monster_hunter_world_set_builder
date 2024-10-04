namespace SetSearchLogic

module Interfaces = 
    type Slot = Slot of int
    
    ///<summary> For the purpose of the Set Search logic, a skill only needs to provide a unique identifier.</summary>
    type Skill<'s when 's : equality> = 's

    type IProvidesSkills<'s> when Skill<'s> = 
        abstract Skills : ('s * int) list

    type IHasSlots<'d> =
        abstract Slots : Slot list

    type IHasActiveSlots<'d> = 
        abstract ActiveSlots : (Slot * 'd option) list
        abstract SetActiveSlots : (Slot * 'd option) list -> IHasActiveSlots<'d>

    type ArmorSet<'set when 'set : equality> = 'set
    type IPartOfSet<'set when ArmorSet<'set>> = 
        abstract Set : 'set 

    type IDecoration<'s> when Skill<'s> =
        inherit IProvidesSkills<'s>
        abstract Slot : Slot

    type Charm<'c, 's when Skill<'s> and 'c :> IProvidesSkills<'s>> = 'c

    type Decoration<'d, 's 
        when Skill<'s>
        and 'd :> IDecoration<'s>
        > = 'd

    type Armor<'a, 's, 'd, 'set
        when Skill<'s> 
        and Decoration<'d, 's> 
        and ArmorSet<'set>
        and 'a :> IProvidesSkills<'s> 
        and 'a :> IHasSlots<'d> 
        and 'a :> IPartOfSet<'set>
        > = 'a

    type ISetBonusRank<'s when Skill<'s>> = 
        abstract RequiredPieces: int
        abstract Skill: 's
    type SetBonusRank<'sbr, 's when 'sbr :> ISetBonusRank<'s> and Skill<'s>> = 'sbr
    

    type ISetBonus<'set, 's, 'sbr when SetBonusRank<'sbr, 's> and ArmorSet<'set>> =
        abstract Set: 'set
        abstract Ranks: 'sbr list

    type SetBonus<'sb, 'set, 's, 'sbr> 
      when 'sb :> ISetBonus<'set, 's, 'sbr> 
      and ArmorSet<'set> 
      and Skill<'s> 
      and SetBonusRank<'sbr, 's>
      = 'sb