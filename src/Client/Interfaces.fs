module Interfaces

type IHasName = 
    abstract member Name : string

type IHasID = 
    abstract member Id : int

type IHasIconURI = 
    abstract member IconUri : string option

type IHasRank = 
    abstract member Rank : APIDataTypes.Rank
  
type IHasDefense = 
    abstract member Defense : int

type IHasAttack =
    abstract member Attack : int

type IHasResistances = 
    abstract member Resistances : APIDataTypes.MHWGameData.Armor.Resistances

type Decoration<'d, 's 
    when SetSearchLogic.Interfaces.Decoration<'d, 's> 
    and 'd :> IHasID 
    and 'd :> IHasIconURI 
    and 'd :> IHasName
    > = 'd
type Skill<'s 
    when SetSearchLogic.Interfaces.Skill<'s> 
    and 's :> IHasName
    and 's :> IHasRank
    > = 's
type Armor<'a, 's, 'd, 'set 
    when SetSearchLogic.Interfaces.Armor<'a, 's, 'd, 'set> 
    and 'a :> IHasName 
    and 'a :> IHasID 
    and 'a :> IHasRank 
    and 'a :> IHasDefense 
    and 'a :> IHasResistances
    > = 'a
type Charm<'c, 's 
    when SetSearchLogic.Interfaces.Charm<'c, 's> 
    and 'c :> IHasRank 
    and 'c :> IHasID
    > = 'c
type Weapon<'w, 'd 
    when 'w :> IHasID 
    and 'w :> SetSearchLogic.Interfaces.IHasSlots<'d> 
    and 'w :> SetSearchLogic.Interfaces.IHasActiveSlots<'d> 
    and 'w :> IHasName 
    and 'w :> IHasAttack 
    > = 'w
type SetBonus<'sb, 'set, 's, 'sbr 
    when SetSearchLogic.Interfaces.SetBonus<'sb, 'set, 's, 'sbr> 
    and 'sb :> IHasName
    > = 'sb

type ArmorSet<'set
    when SetSearchLogic.Interfaces.ArmorSet<'set>
    > = 'set

type ISkillData<'s> when Skill<'s> = 
    abstract Skill : 's
    abstract MaxRank : int

type SkillData<'sd, 's
    when Skill<'s>
    and 'sd :> ISkillData<'s>
    > = 'sd