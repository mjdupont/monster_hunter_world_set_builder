/// This module is intended to reflect the DataTypes used in the Monster Hunter API.
/// Very little logic should be included here.
module APIDataTypes

type Rank =
    | Low
    | High
    | Master

let (|Rank|_|) (rankStr: string) =
    match rankStr.ToLower() with
    | "master" -> Some Master
    | "high" -> Some High
    | "low" -> Some Low
    | _ -> None

type Element =
    | Fire
    | Water
    | Ice
    | Thunder
    | Dragon

type ElementalResist =
    | All
    | Element of Element

type SkillRankModifier =
    | Affinity of float
    | Attack of int
    | ElementalDamage of (Element * int)
    | ElementalResist of (ElementalResist * int)
    | Defense of int
    | Health of int
    | SharpnessBonus of int

let skillRankModifierMatcher (modName: string, value: FSharp.Data.JsonValue) =
    match (modName.ToLower()), value with
    | "affinity", FSharp.Data.JsonValue.Number x -> x |> (float) |> Affinity |> Some
    | "attack", FSharp.Data.JsonValue.Number x -> x |> (int) |> Attack |> Some
    | "defense", FSharp.Data.JsonValue.Number x -> x |> (int) |> Defense |> Some
    | "health", FSharp.Data.JsonValue.Number x -> x |> (int) |> Health |> Some
    | "sharpnessbonus", FSharp.Data.JsonValue.Number x -> x |> (int) |> SharpnessBonus |> Some
    | "damagefire", FSharp.Data.JsonValue.Number x -> (Fire, (int) x) |> ElementalDamage |> Some
    | "damagewater", FSharp.Data.JsonValue.Number x -> (Water, (int) x) |> ElementalDamage |> Some
    | "damageice", FSharp.Data.JsonValue.Number x -> (Ice, (int) x) |> ElementalDamage |> Some
    | "damagethunder", FSharp.Data.JsonValue.Number x -> (Thunder, (int) x) |> ElementalDamage |> Some
    | "damagedragon", FSharp.Data.JsonValue.Number x -> (Dragon, (int) x) |> ElementalDamage |> Some
    | "resistall", FSharp.Data.JsonValue.Number x -> (All, (int) x) |> ElementalResist |> Some
    | "resistfire", FSharp.Data.JsonValue.Number x -> (Element Fire, (int) x) |> ElementalResist |> Some
    | "resistwater", FSharp.Data.JsonValue.Number x -> (Element Water, (int) x) |> ElementalResist |> Some
    | "resistice", FSharp.Data.JsonValue.Number x -> (Element Ice, (int) x) |> ElementalResist |> Some
    | "resistthunder", FSharp.Data.JsonValue.Number x -> (Element Thunder, (int) x) |> ElementalResist |> Some
    | "resistdragon", FSharp.Data.JsonValue.Number x -> (Element Dragon, (int) x) |> ElementalResist |> Some
    | _ -> None


type SkillRank = {
    Id: int
    Level: int
    Description: string
    Skill: int
    SkillName: string
    Modifiers: SkillRankModifier[]
}

type SrSkill = 
  { Id : int } with 
      interface SetSearch.Interfaces.ISkill with 
          member this.SkillId = this.Id


[<StructuredFormatDisplay("{Name}")>]
type Skill = {
    Id: int
    Slug: string
    Name: string
    Description: string
    Ranks: SkillRank list
} with

    override this.ToString() = this.Name
    interface SetSearch.Interfaces.ISkill with
      member this.SkillId = this.Id


[<StructuredFormatDisplay("{Name}")>]
type Decoration = {
    Id: int
    Name: string
    Rarity: int
    Slot: int
    Skills: SkillRank list
    IconUri: string option
} with

    override this.ToString() = this.Name
    interface SetSearch.Interfaces.IDecoration<SrSkill> with
      member this.Skills = this.Skills |> List.map (fun sr -> ({Id = sr.Skill}), sr.Level)
      member this.Slot = SetSearch.Interfaces.Slot this.Slot
                          


type ArmorType =
    | Headgear
    | Chest
    | Gloves
    | Waist
    | Legs

    static member allTypes = [ Headgear; Chest; Gloves; Waist; Legs ]

let (|ArmorType|_|) (armorStr: string) =
    match (armorStr.ToLower()) with
    | "head" -> Some Headgear
    | "chest" -> Some Chest
    | "gloves" -> Some Gloves
    | "waist" -> Some Waist
    | "legs" -> Some Legs
    | _ -> None

type Defense = { Base: int; Max: int; Augmented: int }

type Resistances = {
    Fire: int
    Water: int
    Ice: int
    Thunder: int
    Dragon: int
}

type Slot = Slot of int

[<StructuredFormatDisplay("{Name}")>]
type Armor = {
    Id: int
    Slug: string
    Name: string
    Type: ArmorType
    Rank: Rank
    Rarity: int
    Defense: Defense
    Resistances: Resistances
    Slots: Slot[]
    Skills: SkillRank list
    ArmorSet: int
// ; Assets: ArmorAssets
// ; Crafting: ArmorCraftingInfo
// ; Attributes: ArmorAttributes
} with

    override this.ToString() = this.Name

type CharmRank = {
    Level: int
    Rarity: int
    Skills: SkillRank list
// ; Crafting: CharmRankCrafting
}

[<StructuredFormatDisplay("{Name}")>]
type Charm = {
    Id: int
    Slug: string
    Name: string
    Ranks: CharmRank list
} with

    override this.ToString() = this.Name

type ArmorSetBonusRank = { Pieces: int; Skill: SkillRank }

type ArmorSetBonus = {
    Id: int
    Name: string
    Ranks: ArmorSetBonusRank list
}

type ArmorSet = {
    Id: int
    Name: string
    Rank: Rank
    Pieces: int[]
    Bonus: ArmorSetBonus option
}



type SkillCategory =
    | ArmorSetSkill
    | ArmorUniqueSkill
    | DecorationSkill
    | ArmorSetAndDecorationSkill
    | ArmorSetAndUniqueSkill


[<StructuredFormatDisplay("{Name}")>]
type Weapon = {
    Id: int
    //; Slug: string
    Name: string
    //; Type: WeaponType
    Rarity: int
    Attack: int
    Slots: Slot[]
// Rest to Follow
} with

    override this.ToString() = this.Name



/// This module is intended to describe data types relating to parsing data from excel records from Monster Hunter World
module MHWGameExcelData =

    module Armor =
        type ArmorType =
            | Regular
            | FullSet

        type EquipSlot =
            | Head
            | Chest
            | Arms
            | Waist
            | Legs
            | Charm

        type Gender =
            | Male
            | Female
            | Unisex

        type Resistances = {
            Fire: int
            Water: int
            Ice: int
            Thunder: int
            Dragon: int
        }

        type ArmorSkill = { Id: int; Name: string; Level: int }

    open Armor

    ///
    /// Armor, as cleaned data pulled from the game files
    ///
    type Armor = {
        Name: string
        Index: int
        ArmorType: ArmorType
        EquipSlot: EquipSlot
        Rarity: int
        Cost: int
        Defense: int
        Resistances: Resistances
        //ModelID1 : int
        Slots: Slot list
        Skills: (ArmorSkill) list
        Set_Skill: string
        Gender: Gender
        Set_Group: int
        Description: string
    }

    module Decoration =
        type DecorationSkill = { Id: int; Name: string; Level: int }

    open Decoration

    type Decoration = {
        Index: int
        Id: int
        Name: string
        Size: int
        Skill1: DecorationSkill
        Skill2: DecorationSkill
        Description: string
    }

    module SkillLevel =
        type UnlockSkill = { Id: int; Name: string }

    open SkillLevel

    type SkillLevel = {
        Index: int
        Id: int
        Name: string
        Level: int
        UnlockSkills: UnlockSkill list
        Description: string
    }

    type SetSkillLevel = {
        Name: string
        Id: int
        Index: int
        IsSetBonus: bool
        IconColorID: int
    }

module MHWGameData = 
    type Skill = { Id: int }

    type ArmorSet = 
      { SetId: int }

    type Decoration = 
      { Skills: (Skill * int) list 
        Slot: Slot
      }
    
    type Charm = 
      { Skills: (Skill * int) list 
      }
    
    type Armor = 
      { Skills: (Skill * int) list 
        Slots: Slot list
        Set: ArmorSet
      }

    type SetBonusRank = 
      { RequiredPieces : int
        Skill : Skill
      }

    type SetBonus = 
      { Set: ArmorSet 
        Ranks: SetBonusRank  list
      }