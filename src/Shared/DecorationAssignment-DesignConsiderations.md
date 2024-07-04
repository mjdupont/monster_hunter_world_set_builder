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


  ## Extra: copied comments of problem worked out elsewhere:
      // Elimination of impossibles: 
      // - We don't have enough of a skill in our decorations to meet the requested skills
      // - The sum of requested skills is greater than the max capacity of the slot sizes
    
    // If the above is true, we can maybe make an assignment.
    // How to deal with 4* 
    // - Find distance between current needs and "solvable with singleton only" needs
    // - -  Current needs: 
    // - - - May not have enough singleton decorations for a skill, that could be solved with 4*
    // - - - May not have enough total decoration capacity for skills if assigned as singletons
    // - - Singleton only needs:
    // - - - Enough singleton decorations to meet each skill need
    // - - - Enough slot capacity to fill all skills of a size

    // - Need to augment Singleton only algorithm to allow for 4*, to solve first current need limitation
    // - - 4* slots can be assigned to, 
    // - - 4* singleton decorations are available and can be assigned
    // - - - 4* singletons must be assigned first, and only until we can assign with true singletons
    // - - - - Calculate difference between true singleton and 4* singleton decorations available
    // - - - - Effectively, first assign only the 4* singletons needed, then we can do normal assignment as above

    // - Then, need to solve getting from current needs to augmented singleton only needs
    // - - Calculate gap in:
    // - - - Slot capacity vs skills needed - If skills needed exceed slots, we need this many decorations "double dipping"
    // - - - Problems example 1: 
    // - - - - For skills A and B:
    // - - - - I have an A+ decoration, and two AB decorations. I need 2 A and 2 B skills, across 2 4* slots. Assigning the A+ is incorrect because A+ and AB overcap A and don't meet B.
    // - - - Problem example 2:
    // - - - - For skills A, B, C:
    // - - - - I have 3 4* slots. I need 2 points for each of A, B, and C. I have 2 of each AB, AC, and BC decorations. 
    // - - - - An assignment of AB, AB will not work, because I do not have a C+ decoration.
    // - - - Both problems seem to stem from when the needed amount for a skill hits 0, as this reduces the number of available decorations.
    // - - - 
    // - - - Problem example 3:
    // - - - - I have skills A, B, C, D: I need 4 A, 2B, 1C, 1D.
    // - - - - I have 4 4* slots. I have 1 A+, 1 AB, 1 AC, 1 AD, 1 BD
    // - - - - If I assign from the skill with the highest amount needed, 
    // - - - - I might assign (4, 2, 1, 1) -> AB (3, 1, 1, 1) -> AC (2, 1, 0, 1) -> AD (1, 1, 0, 0). I don't have a second AB, so I can't solve this.
    // - - - This contrasts with the first case, where assigning A+ last would be better.
    // - - - Problem example 4:
    // - - - - I need skills 4A, 1B, 1C, 1D, 1E - I have A+, AB, AC, AD, AE. I would need AB/AC/AD/AE; can't choose A+ first
    
    // Things that won't work:
    // - Choose decoration that satisfies highest skill(s)? - Fails Examples 3
    // - Assign decoration+ first? - Fails Example 1, 4
    // - Assign decorations by skill with fewest decorations? - Fails Example 2

    // Thoughts:
    // Look at minimizing decoration loss? As in, if I satisfy a skill, which decoration options do I lose?
    // Skills needed vs skills available in decorations?
    // - 1: A: (2, 4) -> 2, B: (2, 2) -> 0; choose decoration with B -> choose decoration with B
    // - 2: (2,4) (2,4) (2,4) -> AB -> (1,3) (1,3) (2,4) -> 0,2 0,2 2,4
    // - 3: 4,5 2,2 1,1 1,2 -> Choose B, AB vs BD are equivalent -> AB -> 3,4 1,1 1,1 1,2 -> BD -> 3,4 0,0 1,1 0,1 -> eliminate AD -> 3,3 0,0 1,1 0,0 -> AC -> AA
    // - 4: 4,6 1,1 1,1 1,1 1,1 -> AB -> 3,5 0,0 1,1 1,1 1,1 -> AC -> 2,4 0,0 0,0 1,1 1,1 -> AD -> 1,3, 0,0 0,0, 0,0, 1,1 -> AE -> done

    // Something like "reach"? "reach" - distance?
    // Remaining slots - max from decos?

    // max (needed-reach)
    // - 1: 2,3 2,2 -> AB -> 1,2 1,1 -> AB -> done
    // - 2: 2,3 2,3 2,3 -> AB -> 1,2 1,2 2,2 -> AC -> 0,1 1,1 1,1 -> BC
    // - 3: 4,5 2,2 1,1 1,2 -> BA -> 3,4 1,1 1,1 1,2 -> BD -> 2,3 0,0 1,1 0,1 -> AC -> 1,2, 0,0 0,0 1,1 -> DA -> 0,0 0,0 0,0 0,0
    // - 4: 4,5 1,1 1,1 1,1 1,1 -> BA -> 3,4 0,0 1,1 1,1 1,1 -> CA -> 2,3 0,0 0,0 1,1 1,1 -> DA -> 1,2 0,0 0,0 0,0 1,1 -> EA -> 0,0 0,0 0,0 0,0 0,0
    
    // - 4, but with BC as well
    // 4,5 1,2 1,2 1,1 1,1 -> DA -> 3,4 1,2 1,2 0,0 1,1 -> EA -> 2,3 1,2 1,2 0,0 0,0 -> A+ -> 0,1 1,1 1,1 0,0 0,0 -> BC -> it works?
    // 2, but minus BC?
    // 2,3 2,3 2,3 -> AB -> 1,2 1,2 2,2 -> AC -> 0,1 1,1 1,1 -> BC
    // 2,3 2,3 2,3 -> AC -> 1,2 2,2 1,2 -> BA -> 0,1 1,1 1,1 -> BC
    // 2,3 2,3 2,3 -> BC -> 2,2 1,2 1,2 -> AB -> 1,1 0,1 1,1 -> AC

    // 2, but minus an AB and a BC?
    // 2,3 2,2 2,3 -> AB -> 1,2 1,1 2,2 -> BC -> 1,1, 0,0 1,1 -> AC

    // 2, but only one of each?
    // 2,2 2,2 2,2 -> AB -> 1,1 1,1 2,2 -> AC -> 0,0 1,1 1,1 -> BC

    // 3, but with BC available?  1 A+, 1 AB, 1 AC, 1 AD, 1 BD, 1 BC
    // A+ AB AC AD
    // AB BC BD
    // AC BC
    // AD BD

    // 4,5 2,3 1,2 1,2 -> A -> 3,4 2,3 1,3 1,2 -> A+ ->
    // 2,3 2,3 1,2 1,2 -> A -> 1,2 2,2 1,2 1,2 -> AB ->
    // 1,2 1,2 1,2 1,2 -> A -> 0,1 1,1 1,1 1,1 -> AC -> 
    // 0,0 1,1 0,0 1,1 -> B -> 0,0 0,0 0,0 1,0 -> BD ->

    // 3, but with BC and another AB available? A+ AB AB AC AD BD BC
    // A+ AB AB AC AD
    // AB AB BC
    // AC BC
    // AD BD

    // 4,5 2,3 1,2 1,2 -> A -> 3,4 2,3 1,2 1,2 -> A+ ->      AB AB AC AD BD BC
    // 2,3 2,3 1,2 1,2 -> A -> 1,2 2,2 1,2 1,2 -> AB ->      AB AC AD BD BC
    // 1,2 1,2 1,2 1,2 -> A -> 0,1 1,1 1,1 1,1 -> AB -> 
    // 0,0 0,0 1,0 1,0 -> FAIL

    // In simpler terms, how do we solve needing 1A 1B, 1C, 1D with 1AB, 1CD, and 1BC? How do we avoid picking 1BC first?

    // Consider special:
    // After choosing first skill, if first skill would be final, list all decorations of all other skills
    // All other decorations with first skill, look at all other options
    // Eliminate from seconds
    // For all other skills, if count of remainig decos = need, eliminate other skill from seconds
    //
    // If none left 
    // 1,2 1,2 1,2 1,2 -> Choose A -> SPECIAL 
    //   Check Others -> 0,1 1,1 (AB, BC, BD) 1,1 (AC, BC) 1,1 (AD, BD)     Seconds: B, C, D
    //   Eliminate A  -> 0,1 1,1 (BC, BD) 1,1 (BC) 1,1 (BD)             Seconds: B, C, D
    //     B -> 2 other choices, no elimination
    //     C -> 1 other choice - eliminate B
    //     D -> 1 other choice - eliminate B 
    // -> AC
    // 0,0 1,1 0,0 1,1  - BC


    // Consider case 3, with only antoher AB -                      A+ AB AB AC AD BD
    // 4:  4,5 2,3 1,1 1,2 -> C -> SPECIAL
    //   Check Others: 4,4 (A+ AB AB AC AD) 2,3 (AB BD) 0,0 () 1,2 (AD BD)
    //   Look at: None
    // Continue
    //  -> (A) -> AC                                                A+ AB AB AD BD
    // 3:  3,4 2,3 0,0 1,2 -> A -> 2,3 2,2 0,0 1,2 -> (B) -> AB ->  A+ AB AD BD
    // 2:  2,3 1,2 0,0 1,2 -> A -> 1,2 1,1 0,0 1,1 -> (B, D) -> 
    //   SPECIAL:
    //     Check Others: 1,2 (A+, AB, AD) 1,1 (AB) 0,0 () 1,1 (AD BD)
    //     Eliminate B -> 1,2 (A+, AD) 1,1 (B) 0,0 () 1,1 (AD)
    // 
    // 1:  0,0 0,0 1,0 1,0 -> FAIL

    // General ideas: A decoration will never cause problems if it doesn't restrict another decoration
    // A decoration restricts another decoration if it:
    // -- Reduces the skill need such that another decoration can't be used: For example, if the only valid set of decorations needed 3 Offensive Guard/Attack decorations, and I chose an Attack/Evasion decoration when I only needed 3 attack, I've now reduced the attack need to reduce the number of decorations
    // -- Reduces the number of decoration slots so I couldn't fit in the total number of decorations of a type that I needed.
    // -- So in the minimum, I can safely add a decoration if after adding it:
    //   -- My number of decoration slots is still greater than any individual skill need
    //   -- I haven't reduced my skill need for the two skills below the highest number of decorations containing either skill

    // -- Can I add a decoration if these conditions apply to the skills and their neighbors?