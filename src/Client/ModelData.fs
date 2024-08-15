//////////////////////////////////////////////////////////////////
///// Optional Extension Methods for src/Shared/ModelData.fs /////
//////////////////////////////////////////////////////////////////

module ModelData

open APIDataTypes
open ModelData

type private StoredDecorationSlot = (Slot * int option) option

module DecorationSlot =
    let serializeDecorationSlotToStorage (decorationSlot: DecorationSlot) : StoredDecorationSlot =
        decorationSlot
        |> Option.map (fun (slot, deco) -> (slot, deco |> Option.map (fun deco -> deco.Id)))

    /// Note if a decoration can't be found in the decorations list, this function defaults that decoration to None.
    let deserializeDecorationSlotFromStorage
        (decorations: Decoration seq)
        (storedDecorationSlot: StoredDecorationSlot)
        : DecorationSlot =
        match storedDecorationSlot with
        | Some(slot, Some deco) ->
            let matchingDecoration =
                decorations
                |> Seq.filter (fun listDeco -> listDeco.Id = deco)
                |> Seq.tryExactlyOne

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
        (decorations: Decoration seq)
        (weapons: Weapon seq)
        (armor: Armor seq)
        (charms: Charm seq)
        (storedString: string)
        : ChosenSet =
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
            | ranks when ranks |> Array.length > 0 ->
                ranks
                |> Seq.filter (fun rank -> rank.Level = charmLevel)
                |> Seq.tryExactlyOne
                |> Option.map (fun x -> charm, x)
            | _ -> None

        let lookupWeapon (weapon: StoredWeapon, storedSlots) =
            match weapon with
            | FromList id ->
                weapons
                |> Seq.filter (fun weapon -> weapon.Id = id)
                |> Seq.tryExactlyOne
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
            |> Seq.filter (fun armor -> armor.Id = armorId)
            |> Seq.tryExactlyOne
            |> Option.map (fun x ->
                x,
                storedSlots
                |> DecorationSlots.deserializeDecorationSlotsFromStorage decorations
                |> (mergeDecorationSlots (x.Slots |> DecorationSlots.FromSlots)))

        let lookupCharm (charmId, level) =
            charms
            |> Seq.filter (fun charm -> charm.Id = charmId)
            |> Seq.tryExactlyOne
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
        |> Option.defaultValue ChosenSet.Default

    static member storeToWebStorage(chosenSet: ChosenSet) : unit =
        let serialized = chosenSet |> ChosenSet.serialize
        Browser.WebStorage.sessionStorage.setItem ("chosenSet", serialized)

    static member readFromWebStorage
        (decorations: Decoration seq)
        (weapons: Weapon seq)
        (armor: Armor seq)
        (charms: Charm seq)
        : ChosenSet =
        Browser.WebStorage.sessionStorage.getItem ("chosenSet")
        |> ChosenSet.deserialize decorations weapons armor charms


type SkillList = 
  SkillList of (Skill * int) list

type StoredSkillList = 
  (int * int) list

type SkillList with
  static member serialize(SkillList skillList) : string = 
    [ for (skill, count) in skillList ->
      skill.Id, count ] |> Thoth.Json.Encode.Auto.toString<StoredSkillList>

  static member deserialize(skills:Skill list) skillListString : SkillList option =
    let maybeRawList = Thoth.Json.Decode.Auto.fromString<StoredSkillList> skillListString
    let maybeTranslatedList = 
      maybeRawList |> Result.map (fun rawList ->
        SkillList 
          [ for skId, count in rawList do
            let maybePair = skills |> List.filter (fun skill -> skill.Id = skId) |> List.tryExactlyOne |> Option.map (fun skill -> skill, count)
            match maybePair with
            | Some pair -> yield pair
            | None -> 
              printfn "No Skill was found for skill Id %i" skId
          ] 
      )
    
    match maybeTranslatedList with
    | Ok list -> Some list
    | Error _ -> None
    
  static member storeToWebStorage(SkillList skillList) =
    let serialized = SkillList.serialize (SkillList skillList)
    Browser.WebStorage.sessionStorage.setItem ("skillList", serialized)

  static member readFromWebStorage(skills: Skill list) =
    Browser.WebStorage.sessionStorage.getItem ("skillList") 
    |> SkillList.deserialize skills  
    |> Option.defaultValue (SkillList [])



type UserData =
  { Armor: Armor list
    Charms: Charm list
  }