# Decoration Assignment Considerations

  Our general goal here is, given a set of requested skill levels, to assign to a set of available decoration slots decorations to satisfy those skill levels.

  This won't aim to influence how the gear providing those decoration slots is chosen; it simply will try to assign decorations to available slots, terminating early in failure if possible.

### Input Data:
  - (Skill * int) list
  - Slot list
  - (Decoration * int) list

  We will assume that the lists are valid data; all skills and decorations are real, obtainable, and accurate to the game; Our slots list will be accurate as well, with a maximum of 18 slots (potentially 20 if we later consider mantle slots). We'll also assume the provided skills are limited to those that decorations can provide (although early termination will likely handle unavailable skills in any circumstance)

## General Problem Breakdown:
  This problem is much easier broken down by considering the simpler case, where only slots and decorations of sizes three and less are accounted for, and then adjusting this case to handle 4* decorations.

### "High Rank" Case
  This case is much simpler because of the 1:1 ratio of decorations and skills. In this case, we can quickly detect if a solution is feasible if two constraints are satisfied:
  - For each skill, we have decorations >= requested skill.
  - We have enough slots for all the needed decorations.
    - The only hangup here is smaller decorations fitting in larger slots.
    - Practically this is true when all of the below are true:
      - #Size3 Decos <= #Size3 slots
      - #Size3 + #Size2 Decos <= #Size3 + #Size2 slots
      - #Size3 + #Size2 + #Size1 Decos <= #Size3 + #Size2 + #Size1 slots
  
  At this point, assignment is as simple as assigning the largest decorations first and moving down the list.

### "Master Rank" Case
  Adding 4* decorations adds complications.
  
  We can do some early elimination with a simple heuristic of "Enough possible skill points" considering 4* decorations. In this case, we can compare the number of needed skills to the number of decoration slots, where each slot provides one skill point, except 4* slots, which count for 2 points (and technically, a certain number of these counting for 3 depending on requested skills). However, 4* decorations can have multiple skils, but it is not always possible for 4* decoration slots to contribute 2 skill points to a solution; cases exist where despite having more than enough 4* decorations to fill all slots, and even enough 4* decorations all of whose skills are requested to fill all slots, that no solution can take advantage of all these decorations, due to the choice of one decoration locking out many others. 

  Our goal for 4* decorations is to identify cases where by assigning 4* decorations, we can satisfy the constraints of the High Rank case. This will involve two separate steps:
    - Relaxing the constraints on the High Rank case to support the existence of 4* slots, and 4* decorations that have exactly one matching skill. 
    - Identifying and exploring cases where these relaxed constraints aren't met, but an assignment of dual skill 4* decorations could let us reach a point where those constraints could be satisfied.

  #### Expanded "High Rank" constraints:
  - Modify having enough decorations per requested skill, to ensuring unmet decorations per skill can be met by 4*
  - Slot constraints same as before, but use unmet skills must be <= 4* slots
  -Assignment as before

  #### Multi-skill 4* constraints:
  - `Unmet requested skills<= ((2*(4*Slots)) + number of feasible hard decorations)`
  - Attempt assignment:
    - Calculate "Skills Needed": again, difference bewteen requested skills, and skills available via singleton decorations.
    - Calculate "Assignment Reach" - This is `min(4*Slots |> List.length, (#AllDecorationsWithSkill + PlusDecorationsWithThisSkill))` - Effectively, how many skill points could we assign to a given skill, if we assigned all our decorations solely focusing on this skill.

    - If a skill's reach is less than its requested amount, we can't solve the decoration assignment.

    - Assign decorations by calculating Reach - Needed, choosing the skill with the lowest Reach, filtering decorations for those with that skill, repeating for the second skill, and assigning that decoration.