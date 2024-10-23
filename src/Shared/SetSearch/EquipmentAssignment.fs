namespace SetSearch

module EquipmentAssignment = 

  open Helpers
  open DecorationAssignment
  open SetSearch.Interfaces

  let aggregateSkills (skills: ('skill*int) list) = 
      skills 
      |> List.groupBy fst
      |> List.map (fun (skill, counts) -> skill, counts |> List.sumBy snd)
  
  let getDecorations (equipment: 'equipment when 'equipment :> IActiveEquipment<'armorset, 'decoration, 'equipmentType, 'skill>) =
      option {
          let! decorations = equipment.MaybeDecorations
          return decorations |> List.choose snd
      }

  let allProvidedSkills (equipment: 'equipment list when 'equipment :> IActiveEquipment<'armorset, 'decoration, 'equipmentType, 'skill>) = 
      let skillsFromEquipment =
          equipment 
          |> List.choose (fun e -> e.MaybeSkills)
          |> List.concat 
          |> aggregateSkills

      let skillsFromDecorations = 
          equipment 
          |> List.choose getDecorations
          |> List.concat
          |> List.map (fun decoration -> decoration.Skills)
          |> List.concat
      
      skillsFromEquipment @ skillsFromDecorations 
      |> aggregateSkills
  
  let unusedDecorations (equipment: 'equipment when 'equipment :> IActiveEquipment<'armorset, 'decoration, 'equipmentType, 'skill>) = 
      option {
          let! decorations = equipment.MaybeDecorations
          return decorations |> List.filter (snd >> Option.isNone) |> List.map fst
      }
      

  let remainingSkillNeed (equipment: 'equipment list when 'equipment:> IEquipment<'armorset, 'equipmentType, 'skill>) (requestedSkills: ('skill * int) list when Skill<'skill>) =
      let achievedSkills = equipment |> allProvidedSkills

      let (matchedSkills: (('skill * int) * ('skill * int)) list), _, _ = 
          List.join (fun (requestedSkill, _) (achievedSkill, _) -> areSameSkill requestedSkill achievedSkill) requestedSkills achievedSkills
      
      [ 
        for ((requestedSkill, requestedCount), (achievedSkill, achievedCount)) in matchedSkills do
            let newNeed = requestedCount - achievedCount
            if newNeed > 0 then requestedSkill, newNeed
      ]


  let getBestPiece (availableEquipment: Map<string, 'equipment list>) equipType = 
      Map.tryFind equipType availableEquipment |> Option.bind List.tryHead
      

  type SearchState<'decoration, 'equipment, 'equipmentType, 'key> = 
      {
          RemainingEquipment: ('equipmentType * ('key * ('equipment list)) list) list
          ExaminedEquipment: ('equipmentType * ('key * ('equipment list)) list) list
          SearchPath: 'equipmentType list
          Decorations: 'decoration list
      }

  // let organizeEquipmentSearch 
  //     (availableEquipment:'equipment list) 
  //     availableDecorations 
  //     (keyGen: 'equipment -> 'key)
  //     requestedSkills 
  //     requestedSetSkills 
  //     : SearchState<'decoration, 'equipment, 'equipmentType, 'key> 
  //     when 'equipment :> IActiveEquipment<'armorset, 'decoration, 'equipmentType, 'skill>
  //     = 
  //     let equipmentByType = 
  //         availableEquipment |> List.groupBy (fun equipment -> equipment.EquipSlot)
  //     let sortedEquipmentByType = 
  //         equipmentByType |> List.map (fun (et, equipment) -> et, equipment |> List.groupBy keyGen)
  //     {
  //         RemainingEquipment = sortedEquipmentByType
  //         ExaminedEquipment = []
  //         SearchPath = []
  //         Decorations = availableDecorations
  //     }


  // let rec findEquipmentSet (searchState:SearchState<'equipment, 'equipmentType, 'key>) requestedSkills requestedSetSkills
  //     match searchState with
  //     | { SetBonusesAchieved = false } -> 
  //         let withSetBonuses

  /// Splitting this out:
  ///   Take:
  ///       What the user wants: Requested skills, requested armor skills, requested defense, requested elemental defense
  ///       What the user has: The user's armor, the user's charms, the user's decorations, the user's weapons
  ///       The shell of an equipment loadout; either empty if none provided, or with parts set from a preset
  ///   Incrementally add pieces to this equipment loadout, starting with armor/weapons/charms, in this order:
  ///   - Achieve set skills
  ///   - Achieve armor-unique skills
  ///   - Achieve other skills
  ///   - Achieve highest defense/elemental defense
  ///   
  ///   - To search these pieces efficiently, first sort them by a key generator depending on preferences:
  ///   - Achieve X
  ///   - Maximize Y
  ///   - Transform these requirements into a key-generator, producing a tuple key with scores for each element in order.
  ///   - Example: I want to achieve, in order, attack 7 and crit eye 7; then, maximize fire defense
  ///   - Becomes a function for contribution to attack 7, contribution to crit eye 7, total fire defense.
  ///   - Allow grouping of "achieves"
  /// 
  ///   - Only sort at the start to generate a structure of equipment ordered by slot, then key.
  ///   - When grabbing new equipment, re-score the key with updated values for what has been achieved.
  ///   - If the score has changed, re-shuffle a piece into the appropriate spot and check the next.
  /// 
  ///   - We want to be able to generate N sets from requirements, a current loadout, and a search state
  ///   - The search state should maintain two copies of the sorted equipment
  ///   - One has all equipment not yet searched
  ///   - One has all equipment that had been searched.
  /// 
  ///   - Choose a piece by iterating over each equipment type and returing the key/score of the head item
  ///   - On retrieving the head item, re-evaluate it's score
  ///       - If it has changed, re-calculate the score, and re-add the item to the available items under the appropriate key
  ///       - Then grab the next piece from the best key.
  /// 
  ///   - From the best pieces for each slot, select the piece with the highest score
  ///   - When a piece has been selected:
  ///       - Add the piece to the equipment loadout
  ///       - Add it's equipment type to a stack containing the unique equipment types.
  /// 
  ///   - Repeat choosing another piece until:
  ///       - All requirements have been met
  ///       - No more pieces can be allocated and not all requirements have been met
  /// 
  ///   - To check requirements, estimate distance in skill points until all "must achieve" criteria are met
  ///   - Estimate reach to this set when filling decorations
  ///   - When requirements seem to be met, attempt decoration assignment. If successful, good. If not, continue building.
  /// 
  /// 
  ///   - When a set is found, add it to the results
  ///   - After finding a complete set:
  ///       - Add the loadout to the results
  ///       - Move the equipment of the type on the head of the equipment type stack from the loadout to the examined pieces
  ///       - Continue the search
  ///   
  ///   - If you have enough equipment, return the found equipment loadouts, and the state of the search.
  ///   
  ///   - When continuing, if you have no more available equipment:
  ///       - Remove the equipment from the equipment type on the top of the equipment type stack from the loadout.
  ///       - Add the removed equipment to the list of examined equipment
  ///       - Once no equipment for that type is available, remove that type from the stack
  ///       - Move the examined equipment back to the available equipment structure, re-sorting them.
  /// 
  /// 
  ///   - When no valid solutions for further loadouts exist, back up in the depth first search
  ///   - Pop a type identifier off of the stack; remove the corresponding piece from the loadout
  ///   - Move the searched pieces for that type off of the 