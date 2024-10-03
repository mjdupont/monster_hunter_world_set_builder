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

type Decoration<'d, 's when 'd :> IHasName and 'd :> IHasID and 'd :> IHasIconURI and SetSearchLogic.Interfaces.Decoration<'d, 's>> = 'd
type Skill<'s when 's :> IHasName and SetSearchLogic.Interfaces.Skill<'s>> = 's
type Armor<'a, 's, 'd, 'sb when SetSearchLogic.Interfaces.Armor<'a, 's, 'd, 'sb> and 'a :> IHasName and 'a :> IHasID and 'a :> IHasRank and 'a :> IHasDefense and 'a :> IHasResistances> = 'a
type Charm<'c, 's when 'c :> IHasID and 'c :> IHasRank and SetSearchLogic.Interfaces.Charm<'c, 's>> = 'c
type Weapon<'w, 'd when 'w :> IHasID and 'w :> SetSearchLogic.Interfaces.IHasSlots<'d> and 'w :> SetSearchLogic.Interfaces.IHasActiveSlots<'d> and 'w :> IHasName and 'w :> IHasAttack > = 'w
type SetBonus<'sb when SetSearchLogic.Interfaces.SetBonus<'sb> and 'sb :> IHasName> = 'sb