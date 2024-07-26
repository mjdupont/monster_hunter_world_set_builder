module SetSearchLogic

open DataTypes
open Helpers

let skillPointsFromSlot =
    (function
    | Slot 4 -> 2
    | _ -> 1)

// type SetSearchPiece =
//   { Piece: (Armor * DecorationSlots) option
//     PieceChoices: Armor list
//     PlaceHolderPieces: Armor list
//   }

// type SetSearchData =
//   { Weapon: Weapon * DecorationSlots
//     Headgear: SetSearchPiece
//     Chest: SetSearchPiece
//     Gloves: SetSearchPiece
//     Waist: SetSearchPiece
//     Legs: SetSearchPiece
//     Charm: (Charm * CharmRank) option * Charm list
//   }
//   member this.AllChosen =
//     [ this.Headgear.Piece.IsSome
//       this.Chest.Piece.IsSome
//       this.Gloves.Piece.IsSome
//       this.Waist.Piece.IsSome
//       this.Legs.Piece.IsSome
//       (this.Charm |> fst).IsSome
//     ] |> List.forall id

//   member this.SkillContribution =
//     [ this.Headgear.Piece |> Option.map (fst >> (fun armor -> armor.Skills))
//       this.Chest.Piece |> Option.map (fst >> (fun armor -> armor.Skills))
//       this.Gloves.Piece |> Option.map (fst >> (fun armor -> armor.Skills))
//       this.Waist.Piece |> Option.map (fst >> (fun armor -> armor.Skills))
//       this.Legs.Piece |> Option.map (fst >> (fun armor -> armor.Skills))
//       this.Charm |> fst |> Option.map (snd >> (fun cr -> cr.Skills))
//     ]
//     |> List.choose (Option.map List.ofArray)
//     |> List.concat

//   member this.DecorationSlots =
//     [ Some (this.Weapon |> fst).Slots
//       this.Headgear.Piece |> Option.map (fst >> (fun armor -> armor.Slots))
//       this.Chest.Piece |> Option.map (fst >> (fun armor -> armor.Slots))
//       this.Gloves.Piece |> Option.map (fst >> (fun armor -> armor.Slots))
//       this.Waist.Piece |> Option.map (fst >> (fun armor -> armor.Slots))
//       this.Legs.Piece |> Option.map (fst >> (fun armor -> armor.Slots))
//     ]
//     |> List.choose (Option.map List.ofArray)
//     |> List.concat

//   member this.Filter armorFilter charmFilter =
//     { this with
//         Headgear = if this.Headgear.Piece.IsNone then { this.Headgear with PieceChoices = this.Headgear.PieceChoices |> armorFilter }  else this.Headgear
//         Chest = if this.Chest.Piece.IsNone then { this.Chest with PieceChoices = this.Chest.PieceChoices |> armorFilter} else this.Chest
//         Gloves = if this.Gloves.Piece.IsNone then { this.Gloves with PieceChoices = this.Gloves.PieceChoices |> armorFilter} else this.Gloves
//         Waist = if this.Waist.Piece.IsNone then { this.Waist with PieceChoices = this.Waist.PieceChoices |> armorFilter} else this.Waist
//         Legs = if this.Legs.Piece.IsNone then { this.Legs with PieceChoices = this.Legs.PieceChoices |> armorFilter} else this.Legs
//         Charm = if (this.Charm |> fst).IsNone then this.Charm |> (fun (charm, list) -> charm, list |> charmFilter) else this.Charm
//     }

let anonymizeArmor armor = {
    armor with
        ArmorSet = -1
        Id = -1
        Skills = [||]
        Name = "Any Piece With These Slots"
        Slug = "Placeholder"
}

// Calculates how much a piece of armor can contribute to the set.
let armorSkillContribution (selectedSkillIds: int list) (armor: Armor) : int =
    let matchingArmorSkills =
        armor.Skills
        |> Array.filter (fun armorSkillRank -> selectedSkillIds |> List.contains armorSkillRank.Id)

    matchingArmorSkills
    |> Array.map (fun armorSkillRank -> armorSkillRank.Level)
    |> Array.sum

let anyPiece (armorType: ArmorType) : Armor = {
    ArmorSet = -1
    Defense = { Base = 0; Augmented = 0; Max = 0 }
    Id = -1
    Name = "Any Piece"
    Rank = Low
    Rarity = -1
    Resistances = {
        Fire = 0
        Ice = 0
        Water = 0
        Dragon = 0
        Thunder = 0
    }
    Skills = [||]
    Slug = "Any Piece"
    Slots = [||]
    Type = armorType
}

let anyCharm =
    {
        Id = -1
        Name = "Any Charm"
        Ranks = [||]
        Slug = "Any Charm"
    },
    { Level = 0; Rarity = 0; Skills = [||] }


// let skillDecoStats (skills : Skill list) (decorations : Decoration list) =

//   let hasPlus skill =
//     decorations
//       |> List.filter (fun deco ->
//         (deco |> decoContainsSkill skill)
//         && (deco |> (not << decoContainsOtherSkill skill))
//       )
//       |> List.exists (fun deco -> deco.Skills |> Array.exists (fun sr -> sr.Id = skill.Id && sr.Level = 2))

//   let hasHard skill =
//     decorations
//       |> List.filter (fun deco ->
//         (deco |> decoContainsSkill skill)
//         && (deco |> (not << decoContainsOtherSkill skill))
//       )
//       |> List.exists (fun deco -> deco.Skills |> Array.exists (fun sr -> sr.Id = skill.Id && sr.Level = 2))

//   let inSize4Deco skill =
//     decorations
//       |> List.filter (fun deco -> deco.Slot = 4)
//       |> List.filter (decoContainsSkill skill)
//       |> (not << List.isEmpty)


//   skills |> List.map (fun skill -> skill, (skill |> decorationSlotSize decorations, skill |> hasPlus, skill |> hasHard, skill |> inSize4Deco))



// let assignDecorations (skillDecoStats: (Skill * (Slot option * bool * bool * bool)) list) (wantedSkills:(Skill*int) list) (decorations:(Decoration*int) list) (slotsToFill:Slot list) : Decoration list =
//   let joinedSkills, _, _ = wantedSkills |> List.join (fun ((skillD:Skill), _) (skillL, _) -> skillD.Id = skillL.Id) skillDecoStats
//   let joinedSkills = joinedSkills |> List.map (fun ((skill, skillD),(skill, needed)) -> skill, skillD, needed )
//   let slotSpaceNeeded =
//     joinedSkills
//     |> List.groupBy (fun (skill, skill_data, needed) -> skill_data |> (fun (single_skill_Slot_size, _, _, _) -> single_skill_Slot_size))
//     |> List.map (fun (slot, skills) -> slot, skills |> List.map (fun (skill, skillD, needed) -> needed) |> List.sum)
//     |> List.choose (fun (slot, neededSkills) -> slot |> Option.map (fun slot -> slot, neededSkills))

//   let slotSpaceAvailable =
//     slotsToFill |> List.countBy id

//   let unMetSingleSkillNeeds, _, _ = List.join (fun a b -> fst a = fst b) slotSpaceNeeded slotSpaceAvailable
//   let unMetSingleSkillNeeds = unMetSingleSkillNeeds |> List.map (fun ((slot, needed),(_, available)) -> slot, (needed - available) )

//   []

// let findSets
//   (armor:Armor list)
//   (decorations:Decoration list)
//   (charms: Charm list)
//   (weapon:Weapon * DecorationSlots)
//   (skills:(Skill*int) list)
//   : ChosenSet list =

//   let selectedSkillIds =
//     skills |> List.map (fun (skill, rank) -> skill.Id)

//   let skillRanksNeeded =
//     skills |> List.choose (fun (skill, rank) -> skill.Ranks |> Array.filter (fun sr -> sr.Level = rank) |> Array.tryExactlyOne)

//   // Note being here; we don't want to consider decorations with an unwanted skill
//   // We know that for all two-skill decorations, there exists a single skill decoration of a smaller size.
//   // As such, double skill decorations with only one relevant skill could simply be replaced with the smaller decoration.
//   let relevantDecorations =
//     decorations
//     |> List.filter (fun decoration -> decoration.Skills |> Array.forall (fun sr -> selectedSkillIds |> List.contains sr.Id ))

//   let startingSetSearchData =
//     { Weapon = weapon
//       Headgear = { Piece = None; PieceChoices = armor |> List.filter (fun piece -> piece.Type = Headgear); PlaceHolderPieces = []}
//       Chest    = { Piece = None; PieceChoices = armor |> List.filter (fun piece -> piece.Type = Chest); PlaceHolderPieces = []}
//       Gloves   = { Piece = None; PieceChoices = armor |> List.filter (fun piece -> piece.Type = Gloves); PlaceHolderPieces = []}
//       Waist    = { Piece = None; PieceChoices = armor |> List.filter (fun piece -> piece.Type = Waist); PlaceHolderPieces = []}
//       Legs     = { Piece = None; PieceChoices = armor |> List.filter (fun piece -> piece.Type = Legs); PlaceHolderPieces = []}
//       Charm = None, charms
//     }

//   let rec buildSet' (setSearchData:SetSearchData) : SetSearchData =
//     let skillsAchieved = setSearchData.SkillContribution |> Array.ofList |> accumulateSkills
//     let skillsYetNeeded = set skillRanksNeeded - set skillsAchieved |> List.ofSeq
//     let skillPointsYetNeeded = skillsYetNeeded |> List.map (fun sr -> sr.Level) |> List.sum
//     let decorationSkillPointsPossible =
//       setSearchData.DecorationSlots |> List.map skillPointsFromSlot |> List.sum

//     match setSearchData with
//     | setSearchData when setSearchData.AllChosen -> setSearchData
//     | setSearchData when skillPointsYetNeeded <= decorationSkillPointsPossible ->
//         { setSearchData with
//             Headgear = if setSearchData.Headgear.Piece.IsNone then { setSearchData.Headgear with Piece = Some <| (anyPiece Headgear, DecorationSlots.FromSlots [||]) } else setSearchData.Headgear
//             Chest = if setSearchData.Chest.Piece.IsNone then { setSearchData.Chest with Piece = Some <| (anyPiece Chest, DecorationSlots.FromSlots [||]) } else setSearchData.Chest
//             Gloves = if setSearchData.Gloves.Piece.IsNone then { setSearchData.Gloves with Piece = Some <| (anyPiece Gloves, DecorationSlots.FromSlots [||]) } else setSearchData.Gloves
//             Waist = if setSearchData.Waist.Piece.IsNone then { setSearchData.Waist with Piece = Some <| (anyPiece Waist, DecorationSlots.FromSlots [||]) } else setSearchData.Waist
//             Legs = if setSearchData.Legs.Piece.IsNone then { setSearchData.Legs with Piece = Some <| (anyPiece Legs, DecorationSlots.FromSlots [||]) } else setSearchData.Legs
//             Charm = if (setSearchData.Charm |> fst).IsNone then (Some anyCharm, setSearchData.Charm |> snd) else setSearchData.Charm
//         }
//     | _ ->

//       let test =
//         if decorationSkillPointsPossible >= skillPointsYetNeeded
//           then
//             { setSearchData with
//                 Headgear = if setSearchData.Headgear.Piece.IsNone then { setSearchData.Headgear with Piece = Some <| (anyPiece Headgear, DecorationSlots.FromSlots [||]) } else setSearchData.Headgear
//                 Chest = if setSearchData.Chest.Piece.IsNone then { setSearchData.Chest with Piece = Some <| (anyPiece Chest, DecorationSlots.FromSlots [||]) } else setSearchData.Chest
//                 Gloves = if setSearchData.Gloves.Piece.IsNone then { setSearchData.Gloves with Piece = Some <| (anyPiece Gloves, DecorationSlots.FromSlots [||]) } else setSearchData.Gloves
//                 Waist = if setSearchData.Waist.Piece.IsNone then { setSearchData.Waist with Piece = Some <| (anyPiece Waist, DecorationSlots.FromSlots [||]) } else setSearchData.Waist
//                 Legs = if setSearchData.Legs.Piece.IsNone then { setSearchData.Legs with Piece = Some <| (anyPiece Legs, DecorationSlots.FromSlots [||]) } else setSearchData.Legs
//                 Charm = if (setSearchData.Charm |> fst).IsNone then (Some anyCharm, setSearchData.Charm |> snd) else setSearchData.Charm
//             }
//           else setSearchData

//       let armorFilter (armor:Armor list) : Armor list =
//         let armorWithSkills, armorWithout =
//           armor |> List.partition (fun piece -> (piece |> armorSkillContribution selectedSkillIds) > 0)
//         let distinctBySlots = armorWithout |> List.distinctBy (fun armor -> armor.Slots) |> List.map anonymizeArmor
//         [armorWithSkills; distinctBySlots] |> List.concat

//       let charmFilter charms =
//         charms
//         |> List.filter (fun (charm:Charm) ->
//           charm.Ranks
//           |> Array.exists (fun cr ->
//             cr.Skills
//             |> Array.exists (fun sr ->
//               skillsYetNeeded
//               |> List.map (fun neededSR -> neededSR.Id )
//               |> List.contains sr.Id
//             )
//           )
//         )

//       let updatedSetSearchData = setSearchData.Filter armorFilter charmFilter

//       // Calculate contribution from remaining pieces
//       // Omit pieces with no contribution, replace with "Any piece with N decoration slots"
//       setSearchData
//   []