module DataTypes

type Rank = 
  | Low
  | High
  | Master
let (|Rank|_|) (rankStr:string) = 
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

let skillRankModifierMatcher (modName:string, value:FSharp.Data.JsonValue) = 
  match (modName.ToLower()), value with
  | "affinity", FSharp.Data.JsonValue.Number x -> x |> (float) |> Affinity |> Some
  | "attack", FSharp.Data.JsonValue.Number x -> x |> (int) |> Attack |> Some
  | "defense", FSharp.Data.JsonValue.Number x -> x |> (int)  |> Defense |> Some
  | "health", FSharp.Data.JsonValue.Number x -> x |> (int)  |> Health |> Some
  | "sharpnessbonus", FSharp.Data.JsonValue.Number x -> x |> (int)  |> SharpnessBonus |> Some
  | "damagefire", FSharp.Data.JsonValue.Number x ->  (Fire, (int) x) |> ElementalDamage |> Some
  | "damagewater", FSharp.Data.JsonValue.Number x ->  (Water, (int) x) |> ElementalDamage |> Some
  | "damageice", FSharp.Data.JsonValue.Number x ->  (Ice, (int) x) |> ElementalDamage |> Some
  | "damagethunder", FSharp.Data.JsonValue.Number x ->  (Thunder, (int) x) |> ElementalDamage |> Some
  | "damagedragon", FSharp.Data.JsonValue.Number x ->  (Dragon, (int) x) |> ElementalDamage |> Some
  | "resistall", FSharp.Data.JsonValue.Number x -> (All, (int) x) |> ElementalResist |> Some
  | "resistfire", FSharp.Data.JsonValue.Number x -> (Element Fire, (int) x) |> ElementalResist |> Some
  | "resistwater", FSharp.Data.JsonValue.Number x -> (Element Water, (int) x) |> ElementalResist |> Some
  | "resistice", FSharp.Data.JsonValue.Number x -> (Element Ice, (int) x) |> ElementalResist |> Some
  | "resistthunder", FSharp.Data.JsonValue.Number x -> (Element Thunder, (int) x) |> ElementalResist |> Some
  | "resistdragon", FSharp.Data.JsonValue.Number x -> (Element Dragon, (int) x) |> ElementalResist |> Some
  | _ -> None


type SkillRank = 
  { Id: int
  ; Level: int
  ; Description: string
  ; Skill: int
  ; SkillName: string
  ; Modifiers: SkillRankModifier []
  }

type Decoration = 
  { Id: int
  ; Name: string 
  ; Rarity: int
  ; Slot: int
  ; Skills: SkillRank []
  ; IconUri: string option
  }

type ArmorType = 
  | Headgear
  | Chest
  | Gloves
  | Waist
  | Legs

let (|ArmorType|_|) (armorStr:string) =
  match (armorStr.ToLower()) with
  | "head" -> Some Headgear
  | "chest" -> Some Chest
  | "gloves" -> Some Gloves
  | "waist" -> Some Waist
  | "legs" -> Some Legs
  | _ -> None 

type Defense = 
  { Base: int
  ; Max: int
  ; Augmented: int
  }

type Resistances = 
  { Fire: int
  ; Water: int
  ; Ice: int
  ; Thunder: int
  ; Dragon: int
  }

type Slot = Slot of int

type Armor = 
  { Id: int
  ; Slug: string
  ; Name: string
  ; Type: ArmorType
  ; Rank: Rank
  ; Rarity: int
  ; Defense: Defense
  ; Resistances: Resistances
  ; Slots: Slot[]
  ; Skills: SkillRank[]
  ; ArmorSet: int
  // ; Assets: ArmorAssets
  // ; Crafting: ArmorCraftingInfo
  // ; Attributes: ArmorAttributes
  }

type CharmRank = 
  { Level: int
  ; Rarity: int
  ; Skills: SkillRank[]
  // ; Crafting: CharmRankCrafting
  }

type Charm = 
  { Id: int
  ; Slug: string
  ; Name: string
  ; Ranks: CharmRank []
  }

type ArmorSetBonusRank = 
  { Pieces: int
  ; Skill: SkillRank  
  }

type ArmorSetBonus = 
  { Id: int
  ; Name: string
  ; Ranks: ArmorSetBonusRank []
  }

type ArmorSet = 
  { Id: int
  ; Name: string
  ; Rank: Rank
  ; Pieces: int []
  ; Bonus: ArmorSetBonus option
  }

type Skill =
  { Id: int
  ; Slug: string
  ; Name: string
  ; Description: string
  ; Ranks: SkillRank []
  }

type Weapon =
  { Id: int
  //; Slug: string
  ; Name: string
  //; Type: WeaponType
  ; Rarity: int
  ; Attack: int
  ; Slots: Slot[]
  // Rest to Follow
  }
  static member Placeholder slots = 
    { Id = 9999 
    ; Name = "Placeholder Weapon"
    ; Rarity = 0
    ; Attack = 0
    ; Slots = slots
    }

///
/// Compares a SkillRank to a Skill to determine if the SkillRank is of the skill.
/// Used due to SkillRank having Id and Skill, which are different, and confusing, and prone to bugs.
/// 
let skillRankOfSkill (skill:Skill) (sr:SkillRank) =
  sr.Skill = skill.Id
