module ModelData

open APIDataTypes
open GameData.APIData
open SetSearchLogic.Interfaces

type DecorationSlot<'d, 's> when Decoration<'d, 's> = (Slot * 'd option) option 

module DecorationSlot =

    let skillsFromDecorationSlot (decorationSlot: DecorationSlot<'d, 's>) =
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

type DecorationSlots<'d, 's> when Decoration<'d, 's> and Skill<'s> = {
    First: DecorationSlot<'d, 's>
    Second: DecorationSlot<'d, 's>
    Third: DecorationSlot<'d, 's>
} with
    interface IProvidesSkills<'s>  with
      member this.Skills : ('s * int) list = 
        [this.First; this.Second; this.Third] 
        |> List.map DecorationSlot.skillsFromDecorationSlot
        |> List.concat

    static member Empty : DecorationSlots<'d, 's> = {
        First = None
        Second = None
        Third = None
    }

    static member removeAllDecorations(decorationSlots: DecorationSlots<'d, 's>) = {
        decorationSlots with
            First = decorationSlots.First |> DecorationSlot.removeDecoration
            Second = decorationSlots.Second |> DecorationSlot.removeDecoration
            Third = decorationSlots.Third |> DecorationSlot.removeDecoration
    }

    static member FromSlots(slots: Slot seq) : DecorationSlots<'d, 's> =
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

    static member skillsFromDecorationSlots(decorationSlots: DecorationSlots<'d, 's>) =
        [
            decorationSlots.First |> DecorationSlot.skillsFromDecorationSlot
            decorationSlots.Second |> DecorationSlot.skillsFromDecorationSlot
            decorationSlots.Third |> DecorationSlot.skillsFromDecorationSlot
        ]
        |> List.concat

    static member asSlots(decorationSlots: DecorationSlots<'d, 's>) =
        [ First; Second; Third ]
        |> List.choose (fun pos -> decorationSlots.SlotFromPosition pos)
    

type Armor with
    static member skillsFromArmor((armor: 'a when Armor<'a, 's, 'd, 'set>), decorationSlots) =
        [ decorationSlots |> DecorationSlots.skillsFromDecorationSlots; armor.Skills ]
        |> List.concat

type ChosenSet<'s, 'a, 'w, 'c, 'd, 'set> when Armor<'a, 's, 'd, 'set> and Charm<'c, 's> and Decoration<'d, 's> and Skill<'s> and ArmorSet<'set>= {
    Weapon: ('w * DecorationSlots<'d, 's>) option
    Headgear: ('a * DecorationSlots<'d, 's>) option
    Chest: ('a * DecorationSlots<'d, 's>) option
    Gloves: ('a * DecorationSlots<'d, 's>) option
    Waist: ('a * DecorationSlots<'d, 's>) option
    Legs: ('a * DecorationSlots<'d, 's>) option
    //Equipment_1: (Equipment * DecorationSlots)
    //Equipment_2: (Equipment * DecorationSlots)
    Charm: 'c option 
} with

    static member Default : ChosenSet<'s, 'a, 'w, 'c, 'd, 'set> = {
        Weapon = None
        Headgear = None
        Chest = None
        Gloves = None
        Waist = None
        Legs = None
        Charm = None
    }

    static member setArmor armorType armor (chosenSet: ChosenSet<'s, 'a, 'w, 'c, 'd, 'set>) =
        match armorType with
        | ArmorType.Headgear -> { chosenSet with Headgear = armor }
        | ArmorType.Gloves -> { chosenSet with Gloves = armor }
        | ArmorType.Chest -> { chosenSet with Chest = armor }
        | ArmorType.Waist -> { chosenSet with Waist = armor }
        | ArmorType.Legs -> { chosenSet with Legs = armor }

    static member tryGetPiece(armorType, (chosenSet: ChosenSet<'s, 'a, 'w, 'c, 'd, 'set>)) =
        match armorType with
        | Headgear -> chosenSet.Headgear
        | Chest -> chosenSet.Chest
        | Gloves -> chosenSet.Gloves
        | Waist -> chosenSet.Waist
        | Legs -> chosenSet.Legs

    static member getUnassignedPieces (chosenSet: ChosenSet<'s, 'a, 'w, 'c, 'd, 'set>) = [
        for armorType in ArmorType.allTypes do
            match ChosenSet.tryGetPiece (armorType, chosenSet) with
            | Some piece -> ()
            | _ -> armorType
    ]

    static member getAssignedPieces (chosenSet: ChosenSet<'s, 'a, 'w, 'c, 'd, 'set>) = [
        for armorType in ArmorType.allTypes do
            match ChosenSet.tryGetPiece (armorType, chosenSet) with
            | Some piece -> yield piece
            | _ -> ()
    ]

    member this.tryGetPiece armorType = ChosenSet.tryGetPiece (armorType, this)

    static member armorSetBonuses (setBonuses: 'sb list) (chosenSet: ChosenSet<'s, 'a, 'w, 'c, 'd, 'set>) 
        : _ 
            when SetBonus<'sb, 'set, 's, 'sbr> 
            and ArmorSet<'set> 
            and Skill<'s> 
            and SetBonusRank<'sbr, 's> =

        let setBonusRanks (setBonuses:'sb list) set = 
            setBonuses 
            |> List.filter (fun sb -> sb.Set = set)
            |> List.tryExactlyOne

        let achievedRanks setBonuses (set, nPieces) =
          setBonusRanks setBonuses set
          |> Option.map (fun (sb:'sb) -> 
              sb.Ranks 
              |> List.filter (fun sbr -> sbr.RequiredPieces <= nPieces) 
              |> List.map (fun sbr -> sbr.Skill, sbr.RequiredPieces))

        let (armorSets: ('set*int) list) =
            [
                chosenSet.Headgear
                chosenSet.Chest
                chosenSet.Gloves
                chosenSet.Waist
                chosenSet.Legs
            ]
            |> List.choose id
            |> List.map fst
            |> List.map (fun armor -> armor.Set)
            |> List.countBy id

        let armorSetRanks =
            armorSets
            |> List.choose (achievedRanks setBonuses) 
            |> List.concat

        armorSetRanks

    static member allSkillRanks(chosenSet: ChosenSet<'s, 'a, 'w, 'c, 'd, 'set>) =
        let skillsFromArmor : ('s * int) list =
            [
                chosenSet.Headgear
                chosenSet.Chest
                chosenSet.Gloves
                chosenSet.Waist
                chosenSet.Legs
            ]
            |> List.choose (Option.map (fun (a, slots) -> a.Skills @ (slots :> IProvidesSkills<'s>).Skills))
            |> List.concat

        let skillsFromCharm : ('s * int) list =
            chosenSet.Charm
            |> Option.map (fun c -> c.Skills)
            |> Option.defaultValue []

        let skillsFromWeapon : ('s * int) list =
            chosenSet.Weapon
            |> Option.map (
                (fun (weapon, slots) -> [| slots |> DecorationSlots.skillsFromDecorationSlots (*; weapon.Skills*) |])
                >> List.concat
            )
            |> Option.defaultValue []

        [ skillsFromArmor; skillsFromCharm; skillsFromWeapon ] |> List.concat

let accumulateSkills (skills: ('s * int) list) =
    skills
    |> List.groupBy (fun (skill, level) -> skill)
    |> List.map (fun (skill, items) ->
        items
        |> List.reduce (fun (skill, accumulatedLevels) (skill', newLevels) -> (skill, accumulatedLevels + newLevels)))

type ChosenSet<'s, 'a, 'w, 'c, 'd, 'set> when Armor<'a, 's, 'd, 'set> and Charm<'c, 's> and Decoration<'d, 's> and Skill<'s> with
    static member skillCount (chosenSet: ChosenSet<'s, 'a, 'w, 'c, 'd, 'set>) =
        chosenSet
        |> ChosenSet.allSkillRanks
        |> accumulateSkills