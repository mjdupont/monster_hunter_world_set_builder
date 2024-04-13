module ModelData

  open DataTypes
  

  type DecorationSlot = (Slot * Decoration option) option
  type private StoredDecorationSlot = (Slot * int option) option
  module DecorationSlot = 
    let serializeDecorationSlotToStorage (decorationSlot:DecorationSlot) : StoredDecorationSlot =
      decorationSlot 
      |> Option.map (fun (slot, deco) -> (slot, deco |> Option.map (fun deco -> deco.Id)))
    
    /// Note if a decoration can't be found in the decorations list, this function defaults that decoration to None. 
    let deserializeDecorationSlotFromStorage (decorations:Decoration seq) (storedDecorationSlot : StoredDecorationSlot) : DecorationSlot = 
      match storedDecorationSlot with 
      | Some (slot, Some deco) -> 
        let matchingDecoration = decorations |> Seq.filter (fun listDeco -> listDeco.Id = deco) |> Seq.tryExactlyOne 
        Some (slot, matchingDecoration)
      | Some (slot, None) -> Some (slot, None)
      | _ -> None
    
    let skillsFromDecorationSlot (decorationSlot:DecorationSlot) =
        decorationSlot
        |> Option.bind snd
        |> Option.map (fun deco -> deco.Skills)
        |> Option.defaultValue [||]


  type DecorationSlotsPosition =
    | First
    | Second
    | Third

  type DecorationSlots =
    {
      First: DecorationSlot
      Second: DecorationSlot
      Third: DecorationSlot
    }
    static member Empty = { First = None; Second = None; Third = None}
    static member FromSlots (slots:Slot array) =
      match slots |> List.ofArray with
      | [first] -> { DecorationSlots.Empty with First = Some (first, None) }
      | [first; second] -> { DecorationSlots.Empty with First = Some (first, None); Second = Some (second, None) }
      | [first; second; third] -> { DecorationSlots.Empty with First = Some (first, None); Second = Some (second, None); Third = Some (third, None)}
      | _ -> DecorationSlots.Empty
    member this.SlotFromPosition position =
      match position with
      | First -> this.First
      | Second -> this.Second
      | Third -> this.Third
  
    static member internal serializeDecorationSlotsToStorage decorationSlots : StoredDecorationSlots =
      {
        First = decorationSlots.First |> DecorationSlot.serializeDecorationSlotToStorage
        Second = decorationSlots.Second |> DecorationSlot.serializeDecorationSlotToStorage
        Third = decorationSlots.Third |> DecorationSlot.serializeDecorationSlotToStorage
      } 
    
    static member internal deserializeDecorationSlotsFromStorage decorations (storage:StoredDecorationSlots) : DecorationSlots =
      {
        First = storage.First |> (DecorationSlot.deserializeDecorationSlotFromStorage decorations)
        Second = storage.Second |> (DecorationSlot.deserializeDecorationSlotFromStorage decorations)
        Third = storage.Third |> (DecorationSlot.deserializeDecorationSlotFromStorage decorations)
      }

    static member skillsfromDecorationSlots (decorationSlots:DecorationSlots) =
        [|decorationSlots.First |> DecorationSlot.skillsFromDecorationSlot
        ; decorationSlots.Second |> DecorationSlot.skillsFromDecorationSlot
        ; decorationSlots.Third |> DecorationSlot.skillsFromDecorationSlot
        |]
        |> Array.concat

  and internal StoredDecorationSlots =
    {
      First: StoredDecorationSlot
      Second: StoredDecorationSlot
      Third: StoredDecorationSlot
    }


  type ChosenSet =
    { Weapon: (Weapon * DecorationSlots) option
      Headgear: (Armor * DecorationSlots) option
      Chest: (Armor * DecorationSlots) option
      Gloves: (Armor * DecorationSlots) option
      Waist: (Armor * DecorationSlots) option
      Legs: (Armor * DecorationSlots) option
      //Equipment_1: (Equipment * DecorationSlots)
      //Equipment_2: (Equipment * DecorationSlots)
      Charm: (Charm * CharmRank) option
    }
      static member Default =
        { Weapon = None
          Headgear = None
          Chest = None
          Gloves = None
          Waist = None
          Legs = None
          Charm = None
        }
      static member setArmor armorType armor (chosenSet:ChosenSet) =
        match armorType with
        | ArmorType.Headgear -> { chosenSet with Headgear = armor }
        | ArmorType.Gloves -> { chosenSet with Gloves = armor }
        | ArmorType.Chest -> { chosenSet with Chest = armor }
        | ArmorType.Waist -> { chosenSet with Waist = armor }
        | ArmorType.Legs -> { chosenSet with Legs = armor }

      static member getArmor armorType (chosenSet:ChosenSet) =
        match armorType with
        | Headgear -> chosenSet.Headgear
        | Chest -> chosenSet.Chest
        | Gloves -> chosenSet.Gloves
        | Waist -> chosenSet.Waist
        | Legs -> chosenSet.Legs

      static member serialize (chosenSet:ChosenSet) : string = 
        let storedForm : StoredChosenSet =
          { Weapon = chosenSet.Weapon |> Option.map (fun (weapon, decoslots) -> (weapon.Id, decoslots |> DecorationSlots.serializeDecorationSlotsToStorage)) 
            Headgear = chosenSet.Headgear |> Option.map (fun (headgear, decoslots) -> (headgear.Id, decoslots |> DecorationSlots.serializeDecorationSlotsToStorage)) 
            Chest = chosenSet.Chest |> Option.map (fun (chest, decoslots) -> (chest.Id, decoslots |> DecorationSlots.serializeDecorationSlotsToStorage)) 
            Gloves = chosenSet.Gloves |> Option.map (fun (gloves, decoslots) -> (gloves.Id, decoslots |> DecorationSlots.serializeDecorationSlotsToStorage)) 
            Waist = chosenSet.Waist |> Option.map (fun (waist, decoslots) -> (waist.Id, decoslots |> DecorationSlots.serializeDecorationSlotsToStorage)) 
            Legs = chosenSet.Legs |> Option.map (fun (legs, decoslots) -> (legs.Id, decoslots |> DecorationSlots.serializeDecorationSlotsToStorage)) 
            //Equipment_1: (Equipment * DecorationSlots)
            //Equipment_2: (Equipment * DecorationSlots)
            Charm = chosenSet.Charm |> Option.map (fun (charm, rank) -> (charm.Id, rank.Level)) 
          }
        storedForm |> Thoth.Json.Encode.Auto.toString

      static member deserialize  (decorations: Decoration seq) (weapons: Weapon seq) (armor: Armor seq) (charms: Charm seq) (storedString:string) : ChosenSet =
        let storedForm: Result<StoredChosenSet, string>  = storedString |> Thoth.Json.Decode.Auto.fromString
        
        /// Handle a weird case where decoration slots in data and storage disagree on decoration slots. Possibly would happen if armor is nerfed/patched.
        /// Default to using data over storage
        /// Handle two cases: 
        /// - Data having fewer slots than storage
        /// -- Handled by the Option.bind, defaulting the entire expression to None if fromData is None
        /// - Storage having fewer slots than data
        /// -- Handled by the if statement 
        let inline mergeDecorationSlot (fromData) (fromStorage) : DecorationSlot = 
          fromData |> Option.bind (fun _ -> if Option.isSome fromStorage then fromStorage else fromData)


        let mergeDecorationSlots (fromData : DecorationSlots) (fromStorage : DecorationSlots) : DecorationSlots =
          { 
            First = mergeDecorationSlot fromData.First fromStorage.First
            Second = mergeDecorationSlot fromData.Second fromStorage.Second
            Third = mergeDecorationSlot fromData.Third fromStorage.Third
          }

        let bindCharmRank charmLevel (charm:Charm) =
          match charm.Ranks with
          | ranks when ranks |> Array.length > 0 -> 
            ranks |> Seq.filter (fun rank -> rank.Level = charmLevel)
            |> Seq.tryExactlyOne
            |> Option.map (fun x -> charm, x)
          | _ -> None

        let lookupWeapon (weaponId, storedSlots) = 
          weapons 
          |> Seq.filter (fun weapon -> weapon.Id = weaponId) 
          |> Seq.tryExactlyOne
          |> Option.map (fun x -> x, storedSlots |> DecorationSlots.deserializeDecorationSlotsFromStorage decorations |> (mergeDecorationSlots (x.Slots |> DecorationSlots.FromSlots)))

        let lookupArmor (armorId, storedSlots) = 
          armor 
          |> Seq.filter (fun armor -> armor.Id = armorId) 
          |> Seq.tryExactlyOne
          |> Option.map (fun x -> x, storedSlots |> DecorationSlots.deserializeDecorationSlotsFromStorage decorations|> (mergeDecorationSlots (x.Slots |> DecorationSlots.FromSlots)))

        let lookupCharm (charmId, level) =
          charms
          |> Seq.filter (fun charm -> charm.Id = charmId)
          |> Seq.tryExactlyOne
          |> Option.bind (bindCharmRank level)

        storedForm |> Result.map (fun storedForm ->
          { Weapon = storedForm.Weapon |> Option.bind lookupWeapon
            Headgear = storedForm.Headgear |> Option.bind lookupArmor
            Chest = storedForm.Chest |> Option.bind lookupArmor
            Gloves = storedForm.Gloves |> Option.bind lookupArmor
            Waist = storedForm.Waist |> Option.bind lookupArmor
            Legs = storedForm.Legs |> Option.bind lookupArmor
            Charm = storedForm.Charm |> Option.bind lookupCharm
          }) |> (function | Ok s -> Some s | Error _ -> None) |> Option.defaultValue ChosenSet.Default

      static member storeToWebStorage (chosenSet:ChosenSet) : unit = 
        let serialized = chosenSet |> ChosenSet.serialize
        Browser.WebStorage.sessionStorage.setItem ("chosenSet", serialized)

      static member readFromWebStorage (decorations:Decoration seq) (weapons:Weapon seq) (armor: Armor seq) (charms: Charm seq) : ChosenSet =
        Browser.WebStorage.sessionStorage.getItem ("chosenSet") |> ChosenSet.deserialize  decorations weapons armor charms

      static member skillsFromArmor ((armor:Armor), decorationSlots) =
        [| decorationSlots |> DecorationSlots.skillsfromDecorationSlots; armor.Skills |]
        |> Array.concat

      static member armorSetBonuses (armorSets:ArmorSet seq) (chosenSet:ChosenSet) =

        let tryFindMatchingArmorSet setId = 
          armorSets |> Seq.filter (fun aset -> aset.Id = setId) |> Seq.tryExactlyOne

        let tryFindMatchingArmorSetBonus setId = 
          let matchingArmorSet = tryFindMatchingArmorSet setId
          matchingArmorSet |> Option.bind (fun matchingArmorSet -> matchingArmorSet.Bonus) 

        let armorSetBonuses = 
          [| chosenSet.Headgear; chosenSet.Chest; chosenSet.Gloves; chosenSet.Waist; chosenSet.Legs |] 
          |> Array.choose id
          |> Array.map fst 
          |> Array.choose (fun armor -> armor.ArmorSet |> tryFindMatchingArmorSetBonus)
        
        let armorSetRanks =
          armorSetBonuses 
          |> Array.groupBy id
          |> Array.map (fun (a, b) -> a, b |> Array.length)
          |> Array.map (fun (bonus, count) -> [|for rank in bonus.Ranks |> Array.filter (fun r -> r.Pieces <= count) -> bonus, rank|]) 
          |> Array.concat

        armorSetRanks

      static member totalSkills (chosenSet:ChosenSet) =
        let skillsFromArmor = 
          [|chosenSet.Headgear
          ; chosenSet.Chest
          ; chosenSet.Gloves
          ; chosenSet.Waist
          ; chosenSet.Legs
          |] |> Array.choose (Option.map ChosenSet.skillsFromArmor)
          |> Array.concat
        let skillsFromCharm =
          chosenSet.Charm 
          |> Option.map (fun (charm, charmRank) -> charmRank.Skills)
          |> Option.defaultValue [||]
        let skillsFromWeapon = 
          chosenSet.Weapon
          |> Option.map ((fun (weapon, slots) -> [| slots |> DecorationSlots.skillsfromDecorationSlots (*; weapon.Skills*) |]) >> Array.concat)
          |> Option.defaultValue [||]
        
        [ skillsFromArmor; skillsFromCharm; skillsFromWeapon ] |> Array.concat

      static member accumulateSkills (skills:SkillRank array) =
        skills
        |> Array.groupBy (fun sr -> sr.Skill)
        |> Array.map (fun (skill, items) ->
          items
          |> Array.reduce (fun skillRankState newSkillRank ->
            { skillRankState with Level = skillRankState.Level + newSkillRank.Level }))



  and private StoredChosenSet = 
    { Weapon: (int * StoredDecorationSlots) option
      Headgear: (int * StoredDecorationSlots) option
      Chest: (int * StoredDecorationSlots) option
      Gloves: (int * StoredDecorationSlots) option
      Waist: (int * StoredDecorationSlots) option
      Legs: (int * StoredDecorationSlots) option
      //Equipment_1: (Equipment * DecorationSlots)
      //Equipment_2: (Equipment * DecorationSlots)
      Charm: (int * int) option
    }