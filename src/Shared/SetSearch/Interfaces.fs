namespace SetSearch

module Interfaces = 
    type Slot = Slot of int

    /// <summary> For the purpose of the Set Search logic, a Skill only needs to provide a unique identifier.
    /// `int` is used here for simplicity. Any other equatable and comparable type might be substituted here.
    /// </summary>
    type ISkill = 
        abstract SkillId : int

    /// <summary> For the purpose of the Set Search logic, a Skill only needs to provide a unique identifier.
    /// `int` is used here for simplicity. Any other equatable and comparable type might be substituted here.
    /// </summary>
    type Skill<'skill when 'skill :> ISkill> = 'skill

    type IProvidesSkills<'skill> when Skill<'skill> = 
        abstract Skills : ('skill * int) list

    type IHasSlots =
        abstract Slots : Slot list

    type IDecoration<'skill> when Skill<'skill> =
        inherit IProvidesSkills<'skill>
        abstract Slot : Slot


    type Decoration<'decoration, 'skill 
        when Skill<'skill>
        and 'decoration :> IDecoration<'skill>
        > = 'decoration

    /// <summary> For the purpose of the Set Search logic, a Set only needs to provide a unique identifier.
    /// `int` is used here for simplicity. Any other equatable and comparable type might be substituted here.
    /// </summary>
    type IPartOfArmorSet = 
        abstract SetId : int

    type ArmorSet<'armorset when 'armorset :> IPartOfArmorSet> = 'armorset

    /// <summary> For the purpose of the Set Search logic, a Set only needs to provide a unique identifier.
    /// `string` is used here for simplicity. Any other equatable and comparable type might be substituted here.
    /// </summary>
    type IEquippable<'equipmentType when 'equipmentType: comparison> = 
        abstract EquipSlot : 'equipmentType

    type IEquipment<'armorset, 'equipmentType, 'skill> when Skill<'skill> and ArmorSet<'armorset> and 'equipmentType : comparison = 
        inherit IEquippable<'equipmentType>
        abstract MaybeSlots : Slot list option
        abstract MaybeSkills : ('skill*int) list option
        abstract MaybeSetID: 'armorset option

    type IMaybeHoldsDecorations<'decoration> = 
        abstract MaybeDecorations : (Slot * 'decoration option) list option


    type IActiveEquipment<'armorset, 'decoration, 'equipmentType, 'skill> 
        when Skill<'skill> 
        and ArmorSet<'armorset> 
        and Decoration<'decoration, 'skill> 
        and 'equipmentType : comparison
        = 
        inherit IEquipment<'armorset, 'equipmentType, 'skill>
        inherit IMaybeHoldsDecorations<'decoration>


    // type Charm<'c, 's when Skill<'s> and 'c :> IProvidesSkills<'s>> = 'c
    

    // type IArmor<'armorset, 'equipmentType, 'skill> when Skill<'skill> and ArmorSet<'armorset> and 'equipmentType : comparison =
    //     inherit IProvidesSkills<'skill>
    //     inherit IPartOfArmorSet
    //     inherit IHasSlots
    //     inherit IEquippable<'equipmentType>

    // type Armor<'armor, 'armorset, 'equipmentType, 'skill
    //     when Skill<'skill> 
    //     and ArmorSet<'armorset>
    //     and 'armor :> IArmor<'armorset, 'equipmentType, 'skill>
    //     and 'equipmentType : comparison
    //     > = 'armor

    type IEquipmentLoadout<'armorset, 'decoration, 'equipment, 'equipmentType, 'skill> 
          when IActiveEquipment<'armorset, 'decoration, 'equipmentType, 'skill>
          and ArmorSet<'armorset>
          and Decoration<'decoration, 'skill>
          and 'equipmentType : comparison 
          and Skill<'skill> 
          and 'equipment :> IActiveEquipment<'armorset, 'decoration, 'equipmentType, 'skill> =
        abstract EquipmentTypes: string list
        abstract OpenEquipmentSlots: string list
        abstract Equipment: ('equipmentType * 'equipment option) list
        
        
    // type ISetBonusRank<'s when Skill<'s>> = 
    //     abstract RequiredPieces: int
    //     abstract Skill: 's
    // type SetBonusRank<'sbr, 's when 'sbr :> ISetBonusRank<'s> and Skill<'s>> = 'sbr
    

    // type ISetBonus<'set, 's, 'sbr when SetBonusRank<'sbr, 's> and ArmorSet<'set>> =
    //     abstract Set: 'set
    //     abstract Ranks: 'sbr list

    // type SetBonus<'sb, 'set, 's, 'sbr> 
    //   when 'sb :> ISetBonus<'set, 's, 'sbr> 
    //   and ArmorSet<'set> 
    //   and Skill<'s> 
    //   and SetBonusRank<'sbr, 's>
    //   = 'sb