module MHWGameDataLoader


open FSharp.Interop.Excel
open APIDataTypes.MHWGameData
open Helpers.ResultExpression
open Helpers
open System.Text.RegularExpressions

module Helpers =
    let pairRegex = Regex("(\d): (.*)")

    let inline tryAsInt x =
        try
            int x |> Some
        with _ ->
            None

    let splitPair text =
        match pairRegex.Match(text).Groups |> List.ofSeq with
        | [ id; name ] when id.Success && name.Success -> id.Value |> tryAsInt |> Option.map (fun id -> id, name.Value)
        | _ -> None

open Helpers

module Armor =
    open Armor
    open SetSearchLogic.Interfaces
    type ArmorFromFile = ExcelFile<"../../resources/datamined_data_clean/armor.xlsx">

    // Note that some fields, like Name, don't really have "invalid" values
    // Other fields should only contain a specific set of strings, which are being mapped to a discriminated union, but technically could fail and are handled here
    // Some fields, like Resistances, *might* be "bad" if the value in the row is extreme; a Resistance of 123 would be extremely unlikely to be valid.
    // In thise case

    type ParseFailure =
        | ArmorType of string
        | EquipSlot of string
        | Resistance of string
        | SlotValue of string
        | Gender of string
        | SlotCount of string

    let parseType rowFieldType =
        match rowFieldType with
        | "Regular" -> Ok Regular
        | "Full_Set" -> Ok FullSet
        | _ ->
            Error(
                ArmorType(
                    sprintf
                        "Failed to parse row field \"Type\": Expected (\"Regular\" or \"Full_Set\"), found %A."
                        rowFieldType
                )
            )

    let parseEquipSlot rowEquipSlot =
        match rowEquipSlot with
        | "Head" -> Ok Head
        | "Chest" -> Ok Chest
        | "Arms" -> Ok Arms
        | "Waist" -> Ok Waist
        | "Legs" -> Ok Legs
        | "Charm" -> Ok Charm
        | _ ->
            Error(
                EquipSlot(
                    sprintf
                        "Failed to parse row field \"EquipSlot\": Expected (\"Head\", \"Chest\", \"Arms\", \"Waist\", \"Legs\", or \"Charm\"), found %A."
                        rowEquipSlot
                )
            )

    let parseResistances (dataRow: ArmorFromFile.Row) = result {

        let validate resistance fieldName =
            let res = ((int) resistance)

            if List.contains res [ -5 .. 5 ] then
                Ok res
            else
                Error(
                    Resistance(
                        sprintf
                            "Failed to parse row field \"%A\": Expected an int value between -5 and 5, found %A."
                            fieldName
                            resistance
                    )
                )

        let! fireRes = validate dataRow.``Fire Res`` "Fire Resistance"
        let! waterRes = validate dataRow.``Water Res`` "Water Resistance"
        let! iceRes = validate dataRow.``Ice Res`` "Ice Resistance"
        let! thunderRes = validate dataRow.``Thunder Res`` "Thunder Resistance"
        let! dragonRes = validate dataRow.``Dragon Res`` "Dragon Resistance"

        return {
            Fire = fireRes
            Water = waterRes
            Ice = iceRes
            Thunder = thunderRes
            Dragon = dragonRes
        }
    }

    let parseSlots (dataRow: ArmorFromFile.Row) = result {

        let validate slotValue slotN =
            let sVal = (int) slotValue

            if List.contains sVal [ 0..4 ] then
                Ok(Slot sVal)
            else
                Error(
                    SlotValue(
                        sprintf
                            "Failed to parse row field \"Slot %i Size\": Expected an int value between -5 and 5, found %A."
                            slotN
                            slotValue
                    )
                )

        let! s1 = validate dataRow.``Slot 1 Size`` 1
        let! s2 = validate dataRow.``Slot 2 Size`` 2
        let! s3 = validate dataRow.``Slot 3 Size`` 3

        let slots = [ s1; s2; s3 ] |> List.filter (fun (Slot s) -> s > 0)
        let count = slots |> List.length
        let expectedCount = (int) dataRow.``Slot Count``

        return!
            if count = expectedCount then
                Ok slots
            else
                Error(
                    SlotCount(
                        sprintf
                            "Failed to validate correct number of slots; field \"Slot Count\" lists \"%i\" slots, but parsing found %i."
                            expectedCount
                            count
                    )
                )
    }

    let parseGender rowFieldGender =
        match rowFieldGender with
        | "Male" -> Ok Male
        | "Female" -> Ok Female
        | "Unisex" -> Ok Unisex
        | _ ->
            Error(
                Gender(
                    sprintf
                        "Failed to parse row field \"Gender\": Expected (\"Male\", \"Female\", or \"Full_Set\"), Found %A"
                        rowFieldGender
                )
            )


    let parseArmorRow (i, dataRow: ArmorFromFile.Row) : Result<Armor, ParseFailure> =
        let parsed = result {
            let! armorType = parseType dataRow.``Type``
            let! gender = parseGender dataRow.Gender
            let! equipSlot = parseEquipSlot dataRow.``Equip Slot``
            let! resistances = parseResistances dataRow
            let! slots = parseSlots dataRow

            return {
                Name = dataRow.Name
                Index = (int) dataRow.Index
                ArmorType = armorType
                EquipSlot = equipSlot
                Rarity = (int) dataRow.Rarity
                Cost = (int) dataRow.Cost
                Defense = (int) dataRow.Defense
                Resistances = resistances
                Slots = slots
                Set_Skill = dataRow.``Set Skill 1``
                Gender = gender
                Set_Group = (int) dataRow.``Set Group``
                Description = dataRow.Description
            }
        }

        match parsed with
        | Ok row -> Ok row
        | Error e ->
            let mapped =
                match e with
                | ArmorType errStr -> ArmorType((sprintf "Row %i:\t" i) + errStr)
                | EquipSlot errStr -> ArmorType((sprintf "Row %i:\t" i) + errStr)
                | Resistance errStr -> ArmorType((sprintf "Row %i:\t" i) + errStr)
                | SlotValue errStr -> ArmorType((sprintf "Row %i:\t" i) + errStr)
                | Gender errStr -> ArmorType((sprintf "Row %i:\t" i) + errStr)
                | SlotCount errStr -> ArmorType((sprintf "Row %i:\t" i) + errStr)

            Error mapped

    let loadArmor () =
        let file = new ArmorFromFile()
        let rows = file.Data |> List.ofSeq

        let successfullyParsed, errors =
            rows |> List.indexed |> List.partitionByR (parseArmorRow)

        successfullyParsed


module Decoration =
    open Decoration


    type DecorationFromFile = ExcelFile<"../../resources/datamined_data_clean/decorations.xls">

    type ParseFailure =
        | SkillParse of string
        | IdParse of string
        | InvalidSlotSize of string
        | NonInteger of string

    let parseID (i, dataRow: DecorationFromFile.Row) : Result<(int * string), ParseFailure> =
        splitPair dataRow.Id
        |> Option.toResult (
            IdParse(
                sprintf
                    "Failed to parse row field \"Id\" on row %i: Expected string in the format \"<int>: <string>\", Found %A"
                    i
                    dataRow.Id
            )
        )

    let parseSkill1 (i, dataRow: DecorationFromFile.Row) : Result<(DecorationSkill), ParseFailure> = result {
        let! id, name =
            splitPair dataRow.``Skill 1``
            |> Option.toResult (
                SkillParse(
                    sprintf
                        "Failed to parse row field \"Skill 1\" on row %i: Expected string in the format \"<int>: <string>\", Found %A"
                        i
                        dataRow.``Skill 1``
                )
            )

        let! level =
            dataRow.``Skill 1 Level``
            |> tryAsInt
            |> Option.toResult (
                NonInteger(
                    sprintf
                        "Failed to parse row field \"Skill 1 Level\" on row %i: Expected an int-convertable value, Found %A"
                        i
                        dataRow.``Skill 1 Level``
                )
            )

        return { Id = id; Name = name; Level = level }
    }

    let parseSkill2 (i, dataRow: DecorationFromFile.Row) : Result<(DecorationSkill), ParseFailure> = result {
        let! id, name =
            splitPair dataRow.`` Skill 2``
            |> Option.toResult (
                SkillParse(
                    sprintf
                        "Failed to parse row field \"Skill 2\" on row %i: Expected string in the format \"<int>: <string>\", Found %A"
                        i
                        dataRow.`` Skill 2``
                )
            )

        let! level =
            dataRow.``Skill 2 Level``
            |> tryAsInt
            |> Option.toResult (
                NonInteger(
                    sprintf
                        "Failed to parse row field \"Skill 2 Level\" on row %i: Expected an int-convertable value, Found %A"
                        i
                        dataRow.``Skill 2 Level``
                )
            )

        return { Id = id; Name = name; Level = level }
    }

    let parseDecorationRow (i, dataRow: DecorationFromFile.Row) : Result<Decoration, ParseFailure> = result {
        let! index =
            tryAsInt dataRow.Index
            |> Option.toResult (
                NonInteger(
                    sprintf
                        "Failed to parse row field \"Index\" on row %i: Expected an int-convertable value, Found %A"
                        i
                        dataRow.``Index``
                )
            )

        let! id, name = parseID (i, dataRow)

        let! size =
            tryAsInt dataRow.Size
            |> Option.toResult (
                NonInteger(
                    sprintf
                        "Failed to parse row field \"Size\" on row %i: Expected an int-convertable value, Found %A"
                        i
                        dataRow.``Size``
                )
            )

        let! skill1 = parseSkill1 (i, dataRow)
        let! skill2 = parseSkill2 (i, dataRow)

        return {
            Index = index
            Id = id
            Name = name
            Size = size
            Skill1 = skill1
            Skill2 = skill2
            Description = dataRow.Description
        }
    }

    let loadDecorations () = 
        let file = new DecorationFromFile()
        let rows = file.Data |> List.ofSeq

        let successfullyParsed, errors =
            rows |> List.indexed |> List.partitionByR (parseDecorationRow)

        successfullyParsed


module SkillLevel =
    open SkillLevel

    type SkillLevelFromFile = ExcelFile<"../../resources/datamined_data_clean/skill_levels.xlsx">

    type ParseFailure =
        | IdParse of string
        | NonInteger of string

    let parseID (i, dataRow: SkillLevelFromFile.Row) : Result<(int * string), ParseFailure> =
        splitPair dataRow.Id
        |> Option.toResult (
            IdParse(
                sprintf
                    "Failed to parse row field \"Id\" on row %i: Expected string in the format \"<int>: <string>\", Found %A"
                    i
                    dataRow.Id
            )
        )

    let parseUnlockSkill i (n, unlockSkillStr) : Result<UnlockSkill, ParseFailure> =
        splitPair unlockSkillStr
        |> Option.toResult (
            IdParse(
                sprintf
                    "Failed to parse row field \"Unlock Skill %i\" on row %i: Expected string in the format \"<int>: <string>\", Found %A"
                    n
                    i
                    unlockSkillStr
            )
        )
        |> Result.map (fun (i, name) -> { Id = i; Name = name })
        

    let parseSkillLevelRow (i, dataRow: SkillLevelFromFile.Row) : Result<SkillLevel, ParseFailure> = result {

        let! id, name = parseID (i, dataRow)

        let! index =
            tryAsInt dataRow.Index
            |> Option.toResult (
                NonInteger(
                    sprintf
                        "Failed to parse row field \"Index\": Expected an int-convertable value, Found %A"
                        dataRow.``Index``
                )
            )

        let! level =
            tryAsInt dataRow.Level
            |> Option.toResult (
                NonInteger(
                    sprintf
                        "Failed to parse row field \"Level\": Expected an int-convertable value, Found %A"
                        dataRow.``Level``
                )
            )

        let! unlockSkills =
            [
                dataRow.``Unlock Skill 1``
                dataRow.``Unlock Skill 2``
                dataRow.``Unlock Skill 3``
                dataRow.``Unlock Skill 4``
                dataRow.``Unlock Skill 5``
                dataRow.``Unlock Skill 6``
            ]
            |> List.indexed
            |> List.map (fun (i, x) -> i + 1, x)
            |> Result.traverseList (parseUnlockSkill i)

        return {
            Name = name
            Description = dataRow.Description
            Id = id
            Index = index
            Level = level
            UnlockSkills = unlockSkills
        }
    }

    let loadSkillLevels () = 
        let file = new SkillLevelFromFile()
        let rows = file.Data |> List.ofSeq

        let successfullyParsed, errors =
            rows |> List.indexed |> List.partitionByR (parseSkillLevelRow)

        successfullyParsed

module SetSkillLevel =


    type SetSkillLevelFromFile = ExcelFile<"../../resources/datamined_data_clean/set_skill_levels.xls">

    type ParseFailure =
        | IdParse of string
        | NonInteger of string

    let parseSetSkillLevelRow (i, dataRow: SetSkillLevelFromFile.Row) : Result<SetSkillLevel, ParseFailure> = result {
        return {
            Name = dataRow.Name
            Id = (int) dataRow.Id
            Index = (int) dataRow.Index
            IsSetBonus = dataRow.``Is Set Bonus``
            IconColorID = (int) dataRow.``Icon Color Id``
        }
    }

    let loadSetSkillLevels () = 
        let file = new SetSkillLevelFromFile()
        let rows = file.Data |> List.ofSeq

        let successfullyParsed, errors =
            rows |> List.indexed |> List.partitionByR (parseSetSkillLevelRow)

        successfullyParsed