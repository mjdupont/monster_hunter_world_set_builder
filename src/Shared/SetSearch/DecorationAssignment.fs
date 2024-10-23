namespace SetSearch

module DecorationAssignment = 
    open Helpers
    open Interfaces

    let inline skillContribution 
        (remainingSkillNeed: ('skill * int) list) 
        (skillSource: 'a) 
        : int when 'a :> IProvidesSkills<'skill2> and Skill<'skill> and Skill<'skill2> 
        =

        let bySkillId (requestedSkill, _) (containedSkill, _) = 
            areSameSkill requestedSkill containedSkill

        let joined, _, _ =
            List.join bySkillId remainingSkillNeed skillSource.Skills

        joined
        |> List.map (fun ((_, rLevel), (_, cLevel)) -> min rLevel cLevel)
        |> List.sum



    let contributes
        (remainingSkillNeed: ('skill * int) list)
        (skillSource: 'a)
        : bool when Skill<'skill> and 'a :> IProvidesSkills<'skill2> and Skill<'skill2>
        =
        skillContribution remainingSkillNeed skillSource > 0



    let potentiallyUsefulDecoration
        (remainingSkillNeed: ('skill * int) list)
        ((Slot size) as maxSizeSlot)
        (decoration: 'decoration)
        : bool when Skill<'skill> and Decoration<'decoration, 'skill>
        =
        decoration |> contributes remainingSkillNeed 
        && decoration.Slot <= maxSizeSlot



    let isHardDecoration 
        (remainingSkillNeed: ('skill * int) list) 
        (decoration: 'decoration) 
        : bool 
        when Skill<'skill>
        and Decoration<'decoration, 'skill2>
        and Skill<'skill2>
        =
        (decoration |> skillContribution remainingSkillNeed) > 2
    


    let addToRequestedSkills 
        (skillNeed: ('skill * int) list) 
        (newSkills: ('skill2 * int) list) 
        : ('skill * int) list when Skill<'skill> and Skill<'skill2>
        =
        let folder requestedSkillNeed ((skillToRemove:'skill2), valueToRemove) =
            requestedSkillNeed
            |> List.map (fun ((requestedSkill:'skill), requestedValue) ->
                if areSameSkill requestedSkill skillToRemove then
                    requestedSkill, requestedValue - valueToRemove
                else
                    requestedSkill, requestedValue)

        newSkills
        |> List.fold folder skillNeed
        |> List.filter (fun (skill, need) -> need > 0)



    let addDecorationToSkillNeed skillNeed (decoration: 'decoration when Decoration<'decoration, 'skill>) = 
        decoration.Skills |> addToRequestedSkills skillNeed


    let distance (requestedSkills: (_ * int) list) =
        requestedSkills |> List.map snd |> List.sum

    ///
    /// <summary> 
    /// Calculates how many additional points hard decorations could add to a set of requested skills. 
    /// If a decoration is a hard decoration, it will provide more than the 2 skill points expected of a size 4 decoration.
    /// </summary>
    /// 
    ///
    let potentialHardDecorationContribution
        (requestedSkills: ('skill * int) list)
        (decorations: ('decoration * int) list)
        (slotCounts: (Slot * int) list)
        : int when Skill<'skill> and Decoration<'decoration, 'skill2> and Skill<'skill2>
        =
        let hardDecorations =
            decorations
            |> List.filter (fst >> isHardDecoration requestedSkills)

        let possibleHardContributions = [
            for requestedSkill, level in requestedSkills do
                for hardDecoration, count in hardDecorations do
                    if
                        hardDecoration.Skills
                        |> List.tryExactlyOne
                        |> Option.map (fun (decorationSkill, _) -> areSameSkill requestedSkill decorationSkill)
                        |> Option.defaultValue false
                    //Note this relies on integer division truncating any fractional component
                    then
                        yield min (level / 3) count
        ]

        let nSize4Slots =
            slotCounts
            |> List.filter (fun ((Slot s), count) -> s = 4)
            |> List.tryExactlyOne
            |> Option.map (snd)
            |> Option.defaultValue 0

        (min (possibleHardContributions |> List.sum) nSize4Slots)



    let slotReachHeuristic =
        function
        | Slot 4 -> 2
        | _ -> 1



    let simplisticReachHeuristic (slots: (Slot * int) seq) =
        [ for Slot s, count in slots -> count * (slotReachHeuristic (Slot s)) ]
        |> List.sum



    let actualReachHeuristic requestedSkills (groupedDecorations: ((int * int * Slot) * ('decoration * int) list) list when Decoration<'decoration, 'skill>) slots =
      let maxContribution =
          groupedDecorations 
          |> List.map (fst >> (fun (a,b,c) -> b))
          |> List.tryMax
          |> Option.defaultValue 0

      slots
      |> List.map (function
          | (Slot 4, n) -> maxContribution * n
          | (_, n) -> n)
      |> List.sum



    let bestCaseReach requestedSkills decorations slots = 
        let reach = simplisticReachHeuristic slots 
        let hardContribution = potentialHardDecorationContribution requestedSkills decorations slots
        reach + hardContribution



    let sortOrder 
        (requestedSkills: ('skill * int) list) 
        (decoration: 'decoration) 
        : (int * int * Slot) when Skill<'skill> and Decoration<'decoration, 'skill2> and Skill<'skill2> 
        = 
          
        let contribution = decoration |> skillContribution requestedSkills
        // Unused space is only used to assign size 4 decorations that contribute only one skill as the last decoration assigned.
        let unusedSpace = contribution - slotReachHeuristic decoration.Slot
        (unusedSpace, contribution, decoration.Slot)

    let reSortKey newKey decoration groupedDecorations =
        groupedDecorations 
        |> List.map 
          (function 
          | (key, decorations) when key = newKey -> (key, decorations @ [decoration])
          | x -> x
          )

    let smallestSlotToFit (Slot slotToFit) (decorationSlots: (Slot * int) list) : (Slot * (Slot * int) list) option = 
        let largeEnoughSlots, notLargeEnoughSlots = decorationSlots |> List.partition (fun (Slot slotSize, count) -> slotSize >= slotToFit)
        match largeEnoughSlots with 
        | (slot, count) :: rest ->
          let newCount = count - 1
          let restLargeEnough = if newCount = 0 then rest else (slot, newCount) :: rest
          Some (slot, restLargeEnough @ notLargeEnoughSlots)
        | [] -> None


    /// "Re-zips" groupedDecorations - reconstructs a groupedDecorations list without a selected decoration
    /// Wraps up any field being empty; removes decorations whose counts are zero, keys whose decorations are zero.
    let reWrapGroupedDecorations key firstDecoration count restDecorations restGroupedDecorations = 
        let newDecorationCount = count - 1
        let newRestDecorations = 
          if newDecorationCount = 0 
          then restDecorations 
          else (firstDecoration, newDecorationCount) :: restDecorations
        let newRestGroupedDecorations = 
          if newRestDecorations = [] 
          then restGroupedDecorations 
          else (key, newRestDecorations) :: restGroupedDecorations
        newRestGroupedDecorations


    // Tries to assign the best decoration from the groupedDecorations. Note, the groupedDecorations are assumed to be ordered correctly.
    let rec tryAssignDecorations
        (assignedDecorations: (Slot * 'decoration option) list) 
        (requestedSkills: ('skill * int) list)
        (decorationSlots: (Slot * int) list)
        (groupedDecorations: ((int * int * Slot) * ('decoration * int) list) list) 
        : ((Slot * 'decoration option) list) option
          when Skill<'skill> and Decoration<'decoration, 'skill2> 
        =

        let tryAssignWithNewGroupedDecorations = tryAssignDecorations assignedDecorations requestedSkills decorationSlots
        
        let actualReachEstimate =
            actualReachHeuristic requestedSkills groupedDecorations decorationSlots

        let distance = requestedSkills |> distance

        if actualReachEstimate < distance then
            None
        else

            let slottableDecorations = 
                groupedDecorations 
                |> List.skipWhile (fun ((_,_,Slot decorationSize), _decorations) -> not (decorationSlots |> List.exists (fun (Slot availableSlot, count) -> availableSlot >= decorationSize)))

            match requestedSkills, slottableDecorations with
            | [], _ -> Some (assignedDecorations @ (decorationSlots |> asItems |> List.map (fun s -> s, None)))
            | _, [] -> None

            | _, (key, []) :: restGroupedDecorations ->
              tryAssignWithNewGroupedDecorations restGroupedDecorations
            | _, (key, ((_, 0) :: restDecorations)) :: restGroupedDecorations ->
              let newGroupedDecorations = (key, restDecorations) :: restGroupedDecorations
              tryAssignWithNewGroupedDecorations newGroupedDecorations

            | _, (key, ((decoration, count) as decoCount) :: restDecorations) :: restGroupedDecorations ->
              let newKey = decoration |> sortOrder requestedSkills
              if key <> newKey 
              then 
                  let groupedDecorationsWithResortedDecoration = restGroupedDecorations |> reSortKey newKey decoCount
                  let newGroupedDecorations = 
                      if restDecorations = [] 
                      then groupedDecorationsWithResortedDecoration 
                      else (key, restDecorations) :: groupedDecorationsWithResortedDecoration
                  tryAssignWithNewGroupedDecorations newGroupedDecorations
              else
                  option {
                      let! slot, remainingSlots = decorationSlots |> smallestSlotToFit decoration.Slot
                      let newRestGroupedDecorations = reWrapGroupedDecorations key decoration count restDecorations restGroupedDecorations
                      let newRequestedSkills = decoration.Skills |> addToRequestedSkills requestedSkills
                      let newAssignedDecorations = (slot, Some decoration) :: assignedDecorations

                      return! 
                        match tryAssignDecorations newAssignedDecorations newRequestedSkills remainingSlots newRestGroupedDecorations with
                        | Some assignment -> Some assignment
                        | None ->
                            let newGroupedDecorations = 
                                if restDecorations = [] 
                                then restGroupedDecorations 
                                else (key, restDecorations) :: restGroupedDecorations
                            tryAssignWithNewGroupedDecorations newGroupedDecorations
                    }



    /// <summary>
    /// Attempts to increase the available slots for assignment by swapping in larger decorations 
    /// or adding new decorations from reserved decorations.
    /// </summary>
    /// 
    /// <remarks> 
    /// Used in the context of being given more than enough slots to fit a set of requested skills.
    /// 
    /// To maximize free decoration slots, a decoration assignment will be solved by setting aside 
    /// available slots until a minimal set of slots is available. If solving the assignment with 
    /// minimal slots fails (i.e. the estimate on the minimal set of slots was too low), 
    /// more slots are made available from those set aside and the assignment is attempted again.
    /// </remarks>
    let allocateReservedSlot ((availableSlots: (Slot*int) list), (reservedSlots: (Slot*int) list)) =
        let availableSlots, reservedSlots = availableSlots |> asItems, reservedSlots |> asItems
        match availableSlots |> List.sort, reservedSlots |> List.sort with
        // No more slots to allocate; cannot allocate additional slots
        | _, [] -> None
        // No slots have been allocated; allocate the smallest slot
        | [], smallestReserved :: rest -> Some([ smallestReserved ], rest)
        // 
        | smallestAvailable :: restAvailable as minimalSlots, reserved ->
            match reserved |> List.partition (fun slot -> slot > smallestAvailable) with
            // Swap the smallest of the available slots for the smallest reserved slot larger than it
            | largerReservedSlot :: restLargerReservedSlots, _restSmallerR ->
                Some((largerReservedSlot :: restAvailable), (smallestAvailable :: restLargerReservedSlots))
            // If there are no larger reserved slots, add the smallest reserved slot
            | [], smallestSmallerR :: restSmallerR -> Some((smallestSmallerR :: minimalSlots), restSmallerR)
            // Note reserved is already checked to be non-empty above; this case should never be hit. None is still the correct answer.
            | [], [] -> None 
        |> Option.map (fun (newAvailableSlots, newReservedSlots) -> newAvailableSlots |> asCounts, newReservedSlots |> asCounts)



    let rec tryFindDecorationAssignment      
        (requestedSkills: ('skill * int) list)
        (((availableSlots: (Slot * int) list), (reservedSlots: (Slot * int) list)) as slots)
        (groupedDecorations: ((int * int * Slot) * ('decoration * int) list) list)
        : (Slot * 'decoration option) list option when Skill<'skill> and Decoration<'decoration, 'skill2> and Skill<'skill2> =
        
        let assignment = tryAssignDecorations [] requestedSkills availableSlots groupedDecorations
        match reservedSlots, assignment with
        | _, Some assignment -> Some (assignment @ (reservedSlots |> asItems |> List.map (fun s -> s, None)))
        | [], None -> None
        | _, None -> 
            match allocateReservedSlot slots with
            | None -> None
            | Some newSlots ->
                tryFindDecorationAssignment requestedSkills newSlots groupedDecorations
        

    let reserveUnneededSlots excessSpace decorationSlots = 
        let excessSpace = max excessSpace 0

        // Intentionally truncating decimal component
        let largeSlotsToAssign = excessSpace / 2
        
        // Take as many Size 4 slots as possible, equal to remaining space/2.
        let size4DecorationSlots, size1to3DecorationSlots = decorationSlots |> List.partition (fun (Slot s) -> s = 4)
        let size4SlotsToChoose = min largeSlotsToAssign (size4DecorationSlots |> List.length)
        
        let reservedSize4Slots, assignableSize4Slots = size4DecorationSlots |> List.splitAt size4SlotsToChoose
        
        // Take as many other slots, starting with the largest, as needed, starting with the largest
        let remainingSpace = excessSpace - size4SlotsToChoose * 2
        let reservedSmallerSlots, assignableSmallerSlots = size1to3DecorationSlots |> List.sortByDescending (fun (Slot s) -> s) |> List.splitAt remainingSpace

        (assignableSize4Slots @ assignableSmallerSlots), (reservedSize4Slots @ reservedSmallerSlots)


    
    let findDecorationsSatisfyingSkills'
        (requestedSkills: ('skill * int) list)
        (decorationSlots: (Slot * int) list)
        (decorations: ('decoration * int) list)
        : (Slot * 'decoration option) list option when Skill<'skill> and Decoration<'decoration, 'skill2> and Skill<'skill2> =

        let bestPossibleReach = bestCaseReach requestedSkills decorations decorationSlots
        let expectedExcessSpace = bestPossibleReach - (distance requestedSkills)
        let assignableSlots, reservedSlots = reserveUnneededSlots expectedExcessSpace (decorationSlots |> asItems)
        let groupedDecorations = 
            decorations 
            |> List.filter (fst >> contributes requestedSkills)
            |> List.groupBy (fst >> sortOrder requestedSkills)
            |> List.sortByDescending (fun (key, decorations) -> key)

        tryFindDecorationAssignment requestedSkills (assignableSlots |> asCounts, reservedSlots |> asCounts) groupedDecorations
