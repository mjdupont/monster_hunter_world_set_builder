//////////////////////////////////////////////////////////////////
///// Optional Extension Methods for src/Shared/ModelData.fs /////
//////////////////////////////////////////////////////////////////

module ModelData

open APIDataTypes
open ModelData
open Helpers.Option
open HelperFunctions.Deferred
open GameData.APIData
open Helpers.Prelude

type private StoredDecorationSlot = (Slot * int option) option

module DecorationSlot =
    let serializeDecorationSlotToStorage (decorationSlot: DecorationSlot) : StoredDecorationSlot =
        decorationSlot
        |> Option.map (fun (slot, deco) -> (slot, deco |> Option.map (fun deco -> deco.Id)))

    /// Note if a decoration can't be found in the decorations list, this function defaults that decoration to None.
    let deserializeDecorationSlotFromStorage
        (decorations: Decoration list)
        (storedDecorationSlot: StoredDecorationSlot)
        : DecorationSlot =
        match storedDecorationSlot with
        | Some(slot, Some deco) ->
            let matchingDecoration =
                decorations
                |> List.filter (fun listDeco -> listDeco.Id = deco)
                |> List.tryExactlyOne

            Some(slot, matchingDecoration)
        | Some(slot, None) -> Some(slot, None)
        | _ -> None



type internal StoredDecorationSlots = {
    First: StoredDecorationSlot
    Second: StoredDecorationSlot
    Third: StoredDecorationSlot
}

type DecorationSlots with
    static member internal serializeDecorationSlotsToStorage(decorationSlots: DecorationSlots) : StoredDecorationSlots = {
        First = decorationSlots.First |> DecorationSlot.serializeDecorationSlotToStorage
        Second = decorationSlots.Second |> DecorationSlot.serializeDecorationSlotToStorage
        Third = decorationSlots.Third |> DecorationSlot.serializeDecorationSlotToStorage
    }

    static member internal deserializeDecorationSlotsFromStorage
        decorations
        (storage: StoredDecorationSlots)
        : DecorationSlots =
        {
            First =
                storage.First
                |> (DecorationSlot.deserializeDecorationSlotFromStorage decorations)
            Second =
                storage.Second
                |> (DecorationSlot.deserializeDecorationSlotFromStorage decorations)
            Third =
                storage.Third
                |> (DecorationSlot.deserializeDecorationSlotFromStorage decorations)
        }



type StoredWeapon =
    | FromList of int
    | Custom of Weapon



type private StoredChosenSet = {
    Weapon: (StoredWeapon * StoredDecorationSlots) option
    Headgear: (int * StoredDecorationSlots) option
    Chest: (int * StoredDecorationSlots) option
    Gloves: (int * StoredDecorationSlots) option
    Waist: (int * StoredDecorationSlots) option
    Legs: (int * StoredDecorationSlots) option
    //Equipment_1: (Equipment * DecorationSlots)
    //Equipment_2: (Equipment * DecorationSlots)
    Charm: (int * int) option
}



type ChosenSet with
    static member serialize(chosenSet: ChosenSet) : string =
        let serializeCustomWeapon (slots: DecorationSlots) (weapon: Weapon) =
            // Note that Slot 0 would normally not occur in a weapon (or armor) struct
            // It is used here for custom weapons, in the case the user made a custom weapon with
            // DecorationSlots.First of None, but DecorationSlots.Second of Some Slot N. In this case,
            // a direct conversion to a Slot array would drop the None first slot, leaving an array with 1 element.
            // On deserialization, the only element would be interpreted to be the first slot,
            // While the stored decorationslots would have the corresponding decoration in the second slot.
            // This would default to dropping the decoration.
            // Long story short, I should rework "Slots". TODO
            let newSlots =
                [| slots.First; slots.Second; slots.Third |]
                |> Array.map (Option.map fst >> Option.defaultValue (Slot 0))

            { weapon with Slots = newSlots }

        let storedForm: StoredChosenSet = {
            Weapon =
                match chosenSet.Weapon with
                | Some(weapon, slots) when weapon.Id = 0 ->
                    Some(
                        Custom(weapon |> serializeCustomWeapon slots),
                        slots |> DecorationSlots.serializeDecorationSlotsToStorage
                    )
                | Some weapon ->
                    chosenSet.Weapon
                    |> Option.map (fun (weapon, decoslots) ->
                        (FromList weapon.Id, decoslots |> DecorationSlots.serializeDecorationSlotsToStorage))
                | None -> None
            Headgear =
                chosenSet.Headgear
                |> Option.map (fun (headgear, decoslots) ->
                    (headgear.Id, decoslots |> DecorationSlots.serializeDecorationSlotsToStorage))
            Chest =
                chosenSet.Chest
                |> Option.map (fun (chest, decoslots) ->
                    (chest.Id, decoslots |> DecorationSlots.serializeDecorationSlotsToStorage))
            Gloves =
                chosenSet.Gloves
                |> Option.map (fun (gloves, decoslots) ->
                    (gloves.Id, decoslots |> DecorationSlots.serializeDecorationSlotsToStorage))
            Waist =
                chosenSet.Waist
                |> Option.map (fun (waist, decoslots) ->
                    (waist.Id, decoslots |> DecorationSlots.serializeDecorationSlotsToStorage))
            Legs =
                chosenSet.Legs
                |> Option.map (fun (legs, decoslots) ->
                    (legs.Id, decoslots |> DecorationSlots.serializeDecorationSlotsToStorage))
            //Equipment_1: (Equipment * DecorationSlots)
            //Equipment_2: (Equipment * DecorationSlots)
            Charm = chosenSet.Charm |> Option.map (fun (charm, rank) -> (charm.Id, rank.Level))
        }

        storedForm |> Thoth.Json.Encode.Auto.toString<StoredChosenSet>

    static member deserialize
        (decorations: Decoration list)
        (weapons: Weapon list)
        (armor: Armor list)
        (charms: Charm list)
        (storedString: string)
        : ChosenSet option =
        let storedForm: Result<StoredChosenSet, string> =
            storedString |> Thoth.Json.Decode.Auto.fromString

        /// Handle a weird case where decoration slots in data and storage disagree on decoration slots. Possibly would happen if armor is nerfed/patched.
        /// Default to using data over storage
        /// Handle two cases:
        /// - Data having fewer slots than storage
        /// -- Handled by the Option.bind, defaulting the entire expression to None if fromData is None
        /// - Storage having fewer slots than data
        /// -- Handled by the if statement
        let inline mergeDecorationSlot (fromData) (fromStorage) : DecorationSlot =
            fromData
            |> Option.bind (fun _ -> if Option.isSome fromStorage then fromStorage else fromData)


        let mergeDecorationSlots (fromData: DecorationSlots) (fromStorage: DecorationSlots) : DecorationSlots = {
            First = mergeDecorationSlot fromData.First fromStorage.First
            Second = mergeDecorationSlot fromData.Second fromStorage.Second
            Third = mergeDecorationSlot fromData.Third fromStorage.Third
        }

        let bindCharmRank charmLevel (charm: Charm) =
            match charm.Ranks with
            | ranks when ranks |> List.length > 0 ->
                ranks
                |> List.filter (fun rank -> rank.Level = charmLevel)
                |> List.tryExactlyOne
                |> Option.map (fun x -> charm, x)
            | _ -> None

        let lookupWeapon (weapon: StoredWeapon, storedSlots) =
            match weapon with
            | FromList id ->
                weapons
                |> List.filter (fun weapon -> weapon.Id = id)
                |> List.tryExactlyOne
                |> Option.map (fun x ->
                    x,
                    storedSlots
                    |> DecorationSlots.deserializeDecorationSlotsFromStorage decorations
                    |> (mergeDecorationSlots (x.Slots |> DecorationSlots.FromSlots)))
            | Custom weapon ->
                Some(
                    weapon,
                    storedSlots
                    |> DecorationSlots.deserializeDecorationSlotsFromStorage decorations
                    |> (mergeDecorationSlots (weapon.Slots |> DecorationSlots.FromSlots))
                )

        let lookupArmor (armorId, storedSlots) =
            armor
            |> List.filter (fun armor -> armor.Id = armorId)
            |> List.tryExactlyOne
            |> Option.map (fun x ->
                x,
                storedSlots
                |> DecorationSlots.deserializeDecorationSlotsFromStorage decorations
                |> (mergeDecorationSlots (x.Slots |> DecorationSlots.FromSlots)))

        let lookupCharm (charmId, level) =
            charms
            |> List.filter (fun charm -> charm.Id = charmId)
            |> List.tryExactlyOne
            |> Option.bind (bindCharmRank level)

        storedForm
        |> Result.map (fun storedForm ->
            ({
                Weapon = storedForm.Weapon |> Option.bind lookupWeapon
                Headgear = storedForm.Headgear |> Option.bind lookupArmor
                Chest = storedForm.Chest |> Option.bind lookupArmor
                Gloves = storedForm.Gloves |> Option.bind lookupArmor
                Waist = storedForm.Waist |> Option.bind lookupArmor
                Legs = storedForm.Legs |> Option.bind lookupArmor
                Charm = storedForm.Charm |> Option.bind lookupCharm
            }
            : ChosenSet))
        |> (function
        | Ok s -> Some s
        | Error _ -> None)

    static member storeToWebStorage(chosenSet: ChosenSet) : Async<unit> = async {
        let serialized = chosenSet |> ChosenSet.serialize
        Browser.WebStorage.sessionStorage.setItem ("chosenSet", serialized)
    }

    static member readFromWebStorage
        (decorations: Decoration list)
        (weapons: Weapon list)
        (armor: Armor list)
        (charms: Charm list)
        : Async<ChosenSet option> =
        async {
            return
                Browser.WebStorage.sessionStorage.getItem ("chosenSet")
                |> ChosenSet.deserialize decorations weapons armor charms
        }


type RequestedSkills = RequestedSkills of Map<Skill, int>

type StoredRequestedSkills = (int * int) list

type RequestedSkills with
    static member serialize(RequestedSkills skillList) : string =
        [ for KeyValue(skill, count) in skillList -> skill.Id, count ]
        |> Thoth.Json.Encode.Auto.toString<StoredRequestedSkills>

    static member deserialize (skills: Skill list) skillListString : RequestedSkills option =
        let maybeRawList =
            Thoth.Json.Decode.Auto.fromString<StoredRequestedSkills> skillListString

        let maybeTranslatedList =
            maybeRawList
            |> Result.map (fun rawList ->
                RequestedSkills(
                    [
                        for skId, count in rawList do
                            let maybePair =
                                skills
                                |> List.filter (fun skill -> skill.Id = skId)
                                |> List.tryExactlyOne
                                |> Option.map (fun skill -> skill, count)

                            match maybePair with
                            | Some pair -> yield pair
                            | None -> printfn "No Skill was found for skill Id %i" skId
                    ]
                    |> Map.ofList
                ))

        match maybeTranslatedList with
        | Ok list -> Some list
        | Error _ -> None

    static member storeToWebStorage(RequestedSkills skillList) = async {
        let serialized = RequestedSkills.serialize (RequestedSkills skillList)
        Browser.WebStorage.sessionStorage.setItem ("skillList", serialized)
    }

    static member readFromWebStorage(skills: Skill list) = async {
        return
            Browser.WebStorage.sessionStorage.getItem ("skillList")
            |> RequestedSkills.deserialize skills
    }


type SearchStatus =
    | NotAsked
    | Searching
    | Found
    | Failed


let inline asMapBy (keyGen: 'a -> int) (xs: 'a list) =
    xs
    |> List.groupBy (keyGen)
    |> List.map (fun (id, a) -> (id, a |> List.tryExactlyOne |> Option.get))
    |> Map.ofList

let inline asMap (x: 'a list when 'a: (member Id: int)) = x |> asMapBy (fun a -> a.Id)


type UserData = {
    Armor: (Armor * bool) list
    Charms: (Charm * int) list
    Decorations: (Decoration * int) list
} with

    static member Default = {
        Armor = []
        Charms = []
        Decorations = []
    }

    static member allItems skills (armor: Armor list) (charms: Charm List) decorations = {
        Armor = armor |> List.map (fun a -> a, true)
        Charms =
            charms
            |> List.map (fun charm -> charm.Ranks |> List.max |> (fun cr -> charm, cr.Level))
        Decorations = decorations |> allDecorations skills
    }

    member this.ArmorMap =
        (lazy
            ([
                for a, b in this.Armor do
                    if b then
                        yield a
             ]
             |> asMap))
            .Force()

    member this.CharmMap =
        (lazy (this.Charms |> asMapBy (fun ((charm: Charm), highestCharmLevel) -> charm.Id)))
            .Force()

    member this.DecorationMap =
        (lazy
            (this.Decorations
             |> asMapBy (fun ((decoration: Decoration), decorationCount) -> decoration.Id)))
            .Force()

type StoredUserData = {
    Armor: int list
    Charms: (int * int) list
    Decorations: (int * int) list
}

type UserData with
    static member serialize userData : string =
        let storageForm: StoredUserData = {
            Armor = [
                for armorPiece, hasArmorPiece in userData.Armor do
                    if hasArmorPiece then
                        yield armorPiece.Id
            ]
            Charms = [
                for (charm, highestCharmLevel) in userData.Charms -> (charm.Id, highestCharmLevel)
            ]
            Decorations = [ for (decoration, count) in userData.Decorations -> decoration.Id, count ]
        }

        storageForm |> Thoth.Json.Encode.Auto.toString

    static member deserialize
        ((armor: Armor list), (charms: Charm list), (decorations: Decoration list))
        (storedUserDataString: string)
        : UserData option =
        let storedUserData = Thoth.Json.Decode.Auto.fromString storedUserDataString

        match storedUserData with
        | Error _ -> None
        | Ok(storedUserData: StoredUserData) ->
            let foundArmor = [
                for piece in armor ->
                    (piece, (storedUserData.Armor |> List.exists (fun storedId -> storedId = piece.Id)))
            ]

            let findMatchingCharm (charmId, maxCharmRank) =
                charms
                |> List.filter (fun c -> c.Id = charmId)
                |> List.tryExactlyOne
                |> Option.map (fun c -> c, maxCharmRank)

            let foundCharms =
                storedUserData.Charms
                |> List.groupBy fst
                |> List.map (fun (id, idandcounts) -> id, idandcounts |> List.map snd |> List.max)
                |> traverseList findMatchingCharm

            let foundDecorations =
                storedUserData.Decorations
                |> traverseList (fun (decoId, decoCount) ->
                    decorations
                    |> List.filter (fun decoration -> decoration.Id = decoId)
                    |> List.tryExactlyOne
                    |> Option.map (fun deco -> deco, decoCount))

            match foundArmor, foundCharms, foundDecorations with
            | a, Some c, Some d ->
                Some {
                    Armor = a
                    Charms = c
                    Decorations = d
                }
            | _ -> None


    static member storeToWebStorage userData = async {
        let serialized = UserData.serialize (userData)
        Browser.WebStorage.sessionStorage.setItem ("userData", serialized)
    }

    static member readFromWebStorage (armor: Armor list) (charms: Charm list) (decorations: Decoration list) = async {
        let result =
            Browser.WebStorage.sessionStorage.getItem ("userData")
            |> UserData.deserialize (armor, charms, decorations)

        return result
    }


type MHWDataType =
    | Armor of Armor list
    | ArmorSets of ArmorSet list
    | Decorations of Decoration list
    | Skills of Skill list
    | Charms of Charm list
    | Weapons of Weapon list

type MHWData = {
    ArmorMap: Map<ArmorType, Armor list>
    ArmorSets: ArmorSet list
    Decorations: Decoration list
    Skills: Skill list
    Charms: Charm list
    Weapons: Weapon list
    Memoized: Memoized
} with

    member this.Armor = this.ArmorMap |> Map.values |> List.ofSeq |> List.concat

type LoadingMHWData = {
    Armor: Deferred<Armor list, string>
    ArmorSets: Deferred<ArmorSet list, string>
    Decorations: Deferred<Decoration list, string>
    Skills: Deferred<Skill list, string>
    Charms: Deferred<Charm list, string>
    Weapons: Deferred<Weapon list, string>
} with

    static member Default = {
        Armor = Deferred.NotAsked
        ArmorSets = Deferred.NotAsked
        Decorations = Deferred.NotAsked
        Skills = Deferred.NotAsked
        Charms = Deferred.NotAsked
        Weapons = Deferred.NotAsked
    }

    member this.isFullyLoaded =
        this.Armor |> Deferred.isSuccessful
        && this.ArmorSets |> Deferred.isSuccessful
        && this.Decorations |> Deferred.isSuccessful
        && this.Skills |> Deferred.isSuccessful
        && this.Charms |> Deferred.isSuccessful
        && this.Weapons |> Deferred.isSuccessful

    member this.asFullyLoaded: PartialDeferred<LoadingMHWData, MHWData, string> =
        match this.Armor, this.ArmorSets, this.Decorations, this.Skills, this.Charms, this.Weapons with
        | Deferred.Success armor,
          Deferred.Success asets,
          Deferred.Success decorations,
          Deferred.Success skills,
          Deferred.Success charms,
          Deferred.Success weapons ->
            PartialDeferred.Success {
                ArmorMap = armor |> List.groupBy (fun armor -> armor.Type) |> Map.ofList
                ArmorSets = asets
                Decorations = decorations
                Skills = skills
                Charms = charms
                Weapons = weapons
                Memoized = {
                    PartitionedSkills = partitionSkills asets decorations skills
                    DecorationMaxCount =
                        decorations
                        |> List.map (fun deco -> deco, maxSkillLevelOfDecoration skills deco)
                        |> Map.ofList
                }
            }
        | _ -> PartialDeferred.Failure "Tried to treat input data as fully loaded before ready"