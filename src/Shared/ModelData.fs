module ModelData

open GameDataTypes

type DecorationSlot = (Slot * Decoration option) option

module DecorationSlot =

    let skillsFromDecorationSlot (decorationSlot: DecorationSlot) =
        decorationSlot
        |> Option.bind snd
        |> Option.map (fun deco -> deco.Skills)
        |> Option.defaultValue [||]


type DecorationSlotsPosition =
    | First
    | Second
    | Third

type DecorationSlots = {
    First: DecorationSlot
    Second: DecorationSlot
    Third: DecorationSlot
} with

    static member Empty = {
        First = None
        Second = None
        Third = None
    }

    static member FromSlots(slots: Slot array) =
        let (slots: (Slot * Decoration option) option array) =
            slots
            |> Array.map (function
                | Slot n when [ 1; 2; 3; 4 ] |> List.contains n -> Some(Slot n, None)
                | _ -> None)

        match slots with
        | [| first |] -> {
            DecorationSlots.Empty with
                First = first
          }
        | [| first; second |] -> {
            DecorationSlots.Empty with
                First = first
                Second = second
          }
        | [| first; second; third |] -> {
            DecorationSlots.Empty with
                First = first
                Second = second
                Third = third
          }
        | _ -> DecorationSlots.Empty

    member this.SlotFromPosition position =
        match position with
        | First -> this.First
        | Second -> this.Second
        | Third -> this.Third

    static member skillsFromDecorationSlots(decorationSlots: DecorationSlots) =
        [|
            decorationSlots.First |> DecorationSlot.skillsFromDecorationSlot
            decorationSlots.Second |> DecorationSlot.skillsFromDecorationSlot
            decorationSlots.Third |> DecorationSlot.skillsFromDecorationSlot
        |]
        |> Array.concat

    static member asSlots(decorationSlots: DecorationSlots) =
        [ First; Second; Third ]
        |> List.choose (fun pos -> decorationSlots.SlotFromPosition pos)

type Armor with 
  static member skillsFromArmor((armor: Armor), decorationSlots) =
      [| decorationSlots |> DecorationSlots.skillsFromDecorationSlots; armor.Skills |]
      |> Array.concat

type ChosenSet = {
    Weapon: (Weapon * DecorationSlots) option
    Headgear: (Armor * DecorationSlots) option
    Chest: (Armor * DecorationSlots) option
    Gloves: (Armor * DecorationSlots) option
    Waist: (Armor * DecorationSlots) option
    Legs: (Armor * DecorationSlots) option
    //Equipment_1: (Equipment * DecorationSlots)
    //Equipment_2: (Equipment * DecorationSlots)
    Charm: (Charm * CharmRank) option
} with

    static member Default = {
        Weapon = None
        Headgear = None
        Chest = None
        Gloves = None
        Waist = None
        Legs = None
        Charm = None
    }

    static member setArmor armorType armor (chosenSet: ChosenSet) =
        match armorType with
        | ArmorType.Headgear -> { chosenSet with Headgear = armor }
        | ArmorType.Gloves -> { chosenSet with Gloves = armor }
        | ArmorType.Chest -> { chosenSet with Chest = armor }
        | ArmorType.Waist -> { chosenSet with Waist = armor }
        | ArmorType.Legs -> { chosenSet with Legs = armor }

    static member tryGetPiece(armorType, (chosenSet: ChosenSet)) =
        match armorType with
        | Headgear -> chosenSet.Headgear
        | Chest -> chosenSet.Chest
        | Gloves -> chosenSet.Gloves
        | Waist -> chosenSet.Waist
        | Legs -> chosenSet.Legs

    static member getUnassignedPieces chosenSet = [
        for armorType in ArmorType.allTypes do
            match ChosenSet.tryGetPiece (armorType, chosenSet) with
            | Some piece -> ()
            | _ -> armorType
    ]

    static member getAssignedPieces chosenSet = [
        for armorType in ArmorType.allTypes do
            match ChosenSet.tryGetPiece (armorType, chosenSet) with
            | Some piece -> yield piece
            | _ -> ()
    ]

    member this.tryGetPiece armorType = ChosenSet.tryGetPiece (armorType, this)

    static member armorSetBonuses (armorSets: ArmorSet seq) (chosenSet: ChosenSet) =

        let tryFindMatchingArmorSet setId =
            armorSets |> Seq.filter (fun aset -> aset.Id = setId) |> Seq.tryExactlyOne

        let tryFindMatchingArmorSetBonus setId =
            let matchingArmorSet = tryFindMatchingArmorSet setId
            matchingArmorSet |> Option.bind (fun matchingArmorSet -> matchingArmorSet.Bonus)

        let armorSetBonuses =
            [|
                chosenSet.Headgear
                chosenSet.Chest
                chosenSet.Gloves
                chosenSet.Waist
                chosenSet.Legs
            |]
            |> Array.choose id
            |> Array.map fst
            |> Array.choose (fun armor -> armor.ArmorSet |> tryFindMatchingArmorSetBonus)

        let armorSetRanks =
            armorSetBonuses
            |> Array.groupBy id
            |> Array.map (fun (a, b) -> a, b |> Array.length)
            |> Array.map (fun (bonus, count) -> [|
                for rank in bonus.Ranks |> Array.filter (fun r -> r.Pieces <= count) -> bonus, rank
            |])
            |> Array.concat

        armorSetRanks

    static member allSkillRanks(chosenSet: ChosenSet) =
        let skillsFromArmor =
            [|
                chosenSet.Headgear
                chosenSet.Chest
                chosenSet.Gloves
                chosenSet.Waist
                chosenSet.Legs
            |]
            |> Array.choose (Option.map Armor.skillsFromArmor)
            |> Array.concat

        let skillsFromCharm =
            chosenSet.Charm
            |> Option.map (fun (charm, charmRank) -> charmRank.Skills)
            |> Option.defaultValue [||]

        let skillsFromWeapon =
            chosenSet.Weapon
            |> Option.map (
                (fun (weapon, slots) -> [| slots |> DecorationSlots.skillsFromDecorationSlots (*; weapon.Skills*) |])
                >> Array.concat
            )
            |> Option.defaultValue [||]

        [ skillsFromArmor; skillsFromCharm; skillsFromWeapon ] |> Array.concat

let accumulateSkills (skills: SkillRank array) =
    skills
    |> Array.groupBy (fun sr -> sr.Skill)
    |> Array.map (fun (skill, items) ->
        items
        |> Array.reduce (fun skillRankState newSkillRank -> {
            skillRankState with
                Level = skillRankState.Level + newSkillRank.Level
        }))

type ChosenSet with
    static member skillCount (skills: Skill list) (chosenSet: ChosenSet) =
        chosenSet
        |> ChosenSet.allSkillRanks
        |> accumulateSkills
        |> Array.choose (fun decoSr ->
            skills
            |> List.filter (fun sk -> skillRankOfSkill sk decoSr)
            |> List.tryExactlyOne
            |> Option.map (fun sk -> sk, decoSr.Level))
        |> List.ofArray