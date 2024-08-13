/// This module is intended to reflect the DataTypes used in the Monster Hunter API.
/// Very little logic should be included here.
module GameDataTypes

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

[<StructuredFormatDisplay("{Name}")>]
type Decoration = {
    Id: int
    Name: string
    Rarity: int
    Slot: int
    Skills: SkillRank[]
    IconUri: string option
} with
  override this.ToString() = this.Name

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
    Skills: SkillRank[]
    ArmorSet: int
// ; Assets: ArmorAssets
// ; Crafting: ArmorCraftingInfo
// ; Attributes: ArmorAttributes
} with
  override this.ToString() = this.Name

type CharmRank = {
    Level: int
    Rarity: int
    Skills: SkillRank[]
// ; Crafting: CharmRankCrafting
}

[<StructuredFormatDisplay("{Name}")>]
type Charm = {
    Id: int
    Slug: string
    Name: string
    Ranks: CharmRank[]
} with override this.ToString() = this.Name 

type ArmorSetBonusRank = { Pieces: int; Skill: SkillRank }

type ArmorSetBonus = {
    Id: int
    Name: string
    Ranks: ArmorSetBonusRank[]
}

type ArmorSet = {
    Id: int
    Name: string
    Rank: Rank
    Pieces: int[]
    Bonus: ArmorSetBonus option
}

[<StructuredFormatDisplay("{Name}")>]
type Skill = {
    Id: int
    Slug: string
    Name: string
    Description: string
    Ranks: SkillRank[]
} with
  override this.ToString() = this.Name

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


///
/// Compares a SkillRank to a Skill to determine if the SkillRank is of the skill.
/// Used due to SkillRank having Id and Skill, which are different, and confusing, and prone to bugs.
///
let skillRankOfSkill (skill: Skill) (sr: SkillRank) = sr.Skill = skill.Id

///
/// Returns the skills contained in an object that contains skills, along with their levels.
///
let inline containedSkills (skills: Skill list) (skillSource: 'a when 'a: (member Skills: SkillRank array)) =
    skillSource.Skills
    |> Array.choose (fun decoSr ->
        skills
        |> List.filter (fun sk -> skillRankOfSkill sk decoSr)
        |> List.tryExactlyOne
        |> Option.map (fun sk -> sk, decoSr.Level))
    |> List.ofArray

///
/// Check if a given decoration provides a given skill.
///
let decoContainsSkill (skill: Skill) (deco: Decoration) =
    deco.Skills |> Array.filter (skillRankOfSkill skill) |> (not << Array.isEmpty)

///
/// Check if a given decoration has any skill that is not the provided skill.
///
let decoContainsOtherSkill (skill: Skill) (deco: Decoration) =
    deco.Skills
    |> Array.filter (not << (skillRankOfSkill skill))
    |> (not << Array.isEmpty)

///
/// Calculates the level of a particular skill in a decoration. If the skill is not present, returns None.
///
let decoSkillLevel (skill: Skill) (deco: Decoration) =
    deco.Skills
    |> Array.filter (skillRankOfSkill skill)
    |> Array.tryExactlyOne
    |> Option.map (fun sr -> sr.Level)

///
/// Checks if the given decoration is a "singleton" - The decoration is not size 4, contains only 1 skill, and that skill is level 1.
///
let isSingletonDecoration skill decoration =
    (decoContainsSkill skill decoration)
    && (not (decoContainsOtherSkill skill decoration))
    && (decoration |> decoSkillLevel skill) = Some 1

///
/// Finds the singleton decoration for a given skill in a list of decorations, if it exists.
///
let singletonDecoration decorations skill =
    decorations |> List.filter (isSingletonDecoration skill) |> List.tryExactlyOne
///
/// Calclulates the slot size of the singleton decoration of a given skill in a list of decorations.
///
let decorationSlotSize decorations skill =
    singletonDecoration decorations skill |> Option.map (fun deco -> Slot deco.Slot)

///
/// Determines if a hard decoration exists for the given skill.
///
let isHardSkill skills decorations skill =
    decorations
    |> List.filter (decoContainsSkill skill)
    |> List.choose (
        (containedSkills skills)
        >> List.filter (fun (skill', level) -> skill = skill' && level = 3)
        >> List.tryExactlyOne
    )
    |> List.tryExactlyOne
    |> Option.isSome

///
/// Calculates the highest level of each skill.
/// 

let skillCaps skills =
    skills
    |> List.map (fun skill -> skill, (skill.Ranks |> Array.map (fun sr -> sr.Level) |> Array.max))
    |> Map.ofList

let allDecorations skills (decorations:Decoration list) =
    decorations
    |> List.map (fun decoration ->
        decoration,
        (decoration
          |> containedSkills skills
          |> List.map (fun (skill, level) ->
              skillCaps skills
              |> Map.tryFind skill
              |> Option.defaultValue 0
              |> (fun x -> int (ceil (float x) / (float level))))
          |> List.min))



let asCounts (xs: 'a seq) = xs |> Seq.countBy id |> List.ofSeq

let asItems (xs: ('a * int) seq) = [
    for x, count in xs do
        for i in 1..count -> x
]

