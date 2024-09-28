module ModelData

open APIDataTypes
open GameData.APIData
open SetSearchLogic.Interfaces

let skillRankAsSkillAndLevel (sr:SkillRank) = {SkillID = sr.Skill; SkillLevel = sr.Level}

type DecorationSlot<'d> when Decoration<'d> = (Slot * 'd option) option 

module DecorationSlot =

    let skillsFromDecorationSlot (decorationSlot: DecorationSlot<'d>) =
        decorationSlot
        |> Option.bind snd
        |> Option.map (fun deco -> deco.Skills)
        |> Option.defaultValue []

    let removeDecoration decorationSlot =
        decorationSlot |> Option.map (fun (slot, _maybeDeco) -> (slot, None))

type DecorationSlotsPosition =
    | First
    | Second
    | Third

type DecorationSlots<'d> when Decoration<'d> = {
    First: DecorationSlot<'d>
    Second: DecorationSlot<'d>
    Third: DecorationSlot<'d>
} with
    interface IProvidesSkills with
      member this.Skills : SkillAndLevel list = 
        [this.First; this.Second; this.Third] 
        |> List.map DecorationSlot.skillsFromDecorationSlot
        |> List.concat

    static member Empty : DecorationSlots<'d> = {
        First = None
        Second = None
        Third = None
    }

    static member removeAllDecorations(decorationSlots: DecorationSlots<'d>) = {
        decorationSlots with
            First = decorationSlots.First |> DecorationSlot.removeDecoration
            Second = decorationSlots.Second |> DecorationSlot.removeDecoration
            Third = decorationSlots.Third |> DecorationSlot.removeDecoration
    }

    static member FromSlots(slots: Slot seq) : DecorationSlots<'d> =
        let slots = slots |> Array.ofSeq
        let (slots: (Slot * 'd option) option array) =
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

    static member skillsFromDecorationSlots(decorationSlots: DecorationSlots<'d>) =
        [
            decorationSlots.First |> DecorationSlot.skillsFromDecorationSlot
            decorationSlots.Second |> DecorationSlot.skillsFromDecorationSlot
            decorationSlots.Third |> DecorationSlot.skillsFromDecorationSlot
        ]
        |> List.concat

    static member asSlots(decorationSlots: DecorationSlots<'d>) =
        [ First; Second; Third ]
        |> List.choose (fun pos -> decorationSlots.SlotFromPosition pos)
    

type Armor with
    static member skillsFromArmor((armor: Armor), decorationSlots) =
        [ decorationSlots |> DecorationSlots.skillsFromDecorationSlots; armor.Skills |> List.map (fun sr -> {SkillID = sr.Skill; SkillLevel = sr.Level}) ]
        |> List.concat

type ChosenSet<'a, 'w, 'c, 'd, 'sb> when Armor<'a, 'd, 'sb> and Charm<'c> and Decoration<'d> = {
    Weapon: (Weapon * DecorationSlots<'d>) option
    Headgear: ('a * DecorationSlots<'d>) option
    Chest: ('a * DecorationSlots<'d>) option
    Gloves: ('a * DecorationSlots<'d>) option
    Waist: ('a * DecorationSlots<'d>) option
    Legs: ('a * DecorationSlots<'d>) option
    //Equipment_1: (Equipment * DecorationSlots)
    //Equipment_2: (Equipment * DecorationSlots)
    Charm: 'c option 
} with

    static member Default : ChosenSet<'a, 'w, 'c, 'd, 'sb> = {
        Weapon = None
        Headgear = None
        Chest = None
        Gloves = None
        Waist = None
        Legs = None
        Charm = None
    }

    static member setArmor armorType armor (chosenSet: ChosenSet<'a, 'w, 'c, 'd, 'sb>) =
        match armorType with
        | ArmorType.Headgear -> { chosenSet with Headgear = armor }
        | ArmorType.Gloves -> { chosenSet with Gloves = armor }
        | ArmorType.Chest -> { chosenSet with Chest = armor }
        | ArmorType.Waist -> { chosenSet with Waist = armor }
        | ArmorType.Legs -> { chosenSet with Legs = armor }

    static member tryGetPiece(armorType, (chosenSet: ChosenSet<'a, 'w, 'c, 'd, 'sb>)) =
        match armorType with
        | Headgear -> chosenSet.Headgear
        | Chest -> chosenSet.Chest
        | Gloves -> chosenSet.Gloves
        | Waist -> chosenSet.Waist
        | Legs -> chosenSet.Legs

    static member getUnassignedPieces (chosenSet: ChosenSet<'a, 'w, 'c, 'd, 'sb>) = [
        for armorType in ArmorType.allTypes do
            match ChosenSet.tryGetPiece (armorType, chosenSet) with
            | Some piece -> ()
            | _ -> armorType
    ]

    static member getAssignedPieces (chosenSet: ChosenSet<'a, 'w, 'c, 'd, 'sb>) = [
        for armorType in ArmorType.allTypes do
            match ChosenSet.tryGetPiece (armorType, chosenSet) with
            | Some piece -> yield piece
            | _ -> ()
    ]

    member this.tryGetPiece armorType = ChosenSet.tryGetPiece (armorType, this)

    static member armorSetBonuses (armorSets: ArmorSet seq) (chosenSet: ChosenSet<'a, 'w, 'c, 'd, 'sb>) =

        let tryFindMatchingArmorSet setId =
            armorSets |> Seq.filter (fun aset -> aset.Id = setId) |> Seq.tryExactlyOne

        let tryFindMatchingArmorSetBonus setId =
            let matchingArmorSet = tryFindMatchingArmorSet setId
            matchingArmorSet |> Option.bind (fun matchingArmorSet -> matchingArmorSet.Bonus)

        let (armorSetBonuses: 'sb list) =
            [
                chosenSet.Headgear
                chosenSet.Chest
                chosenSet.Gloves
                chosenSet.Waist
                chosenSet.Legs
            ]
            |> List.choose id
            |> List.map fst
            |> List.choose (fun armor -> armor.SetBonus)

        let armorSetRanks =
            armorSetBonuses
            |> List.groupBy id
            |> List.map (fun (a, b) -> a, b |> List.length)
            |> List.map (fun ((bonus:'sb), count) -> [
                for rank in bonus.Ranks |> List.filter (fun {RequiredPieces = requiredPieces} -> requiredPieces <= count) -> bonus, rank
            ])
            |> List.concat

        armorSetRanks

    static member allSkillRanks(chosenSet: ChosenSet<'a, 'w, 'c, 'd, 'sb>) =
        let skillsFromArmor : SkillAndLevel list =
            [
                chosenSet.Headgear
                chosenSet.Chest
                chosenSet.Gloves
                chosenSet.Waist
                chosenSet.Legs
            ]
            |> List.choose (Option.map (fun (a, slots) -> a.Skills @ (slots :> IProvidesSkills).Skills))
            |> List.concat

        let skillsFromCharm : SkillAndLevel list =
            chosenSet.Charm
            |> Option.map (fun c -> c.Skills)
            |> Option.defaultValue []

        let skillsFromWeapon : SkillAndLevel list =
            chosenSet.Weapon
            |> Option.map (
                (fun (weapon, slots) -> [| slots |> DecorationSlots.skillsFromDecorationSlots (*; weapon.Skills*) |])
                >> List.concat
            )
            |> Option.defaultValue []

        [ skillsFromArmor; skillsFromCharm; skillsFromWeapon ] |> List.concat

let accumulateSkills (skills: SkillAndLevel list) =
    skills
    |> List.groupBy (fun sr -> sr.SkillID)
    |> List.map (fun (skill, items) ->
        items
        |> List.reduce (fun skillRankState newSkillRank -> {
            skillRankState with
                SkillLevel = skillRankState.SkillLevel + newSkillRank.SkillLevel
        }))

type ChosenSet<'a, 'w, 'c, 'd, 'sb> when Armor<'a, 'd, 'sb> and Charm<'c> and Decoration<'d> with
    static member skillCount (chosenSet: ChosenSet<'a, 'w, 'c, 'd, 'sb>) =
        chosenSet
        |> ChosenSet.allSkillRanks
        |> accumulateSkills