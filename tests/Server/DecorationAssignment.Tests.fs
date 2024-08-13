module DecorationAssignment.Tests

open Shared
open SetSearchLogic
open DataTypes
open Helpers
open Server
open DataAccess
#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let unsafeUnwrapResult d (r: Result<'a, 'err>) =
    match r with
    | Ok r -> r
    | _ -> d

let stringMatch elements (name: 'a -> string) strings =
    let matchingElements =
        strings
        |> List.map (fun decoName -> elements |> List.filter (fun elem -> name elem = decoName) |> List.tryExactlyOne)

    Expect.all matchingElements Option.isSome "All matching elements were found"
    matchingElements |> List.choose id

let stringTraverse elements name strings =
    let matchingElements =
        strings
        |> List.map (
            Option.traverseList (fun elemName ->
                elements |> List.filter (fun elem -> name elem = elemName) |> List.tryExactlyOne)
        )

    Expect.all matchingElements Option.isSome "All matching elements were found"
    matchingElements |> List.choose id

let threeSeconds () =
    Async.Sleep(3000) |> Async.RunSynchronously


let decorationAssignment =
    let skillNames, skills = InferredTypes.Skill.loadSkills |> Async.RunSynchronously

    let decorations =
        InferredTypes.Decoration.loadDecorations skillNames |> Async.RunSynchronously

    testList "DecorationAssignment" [
        testCase "Matches SkillRanks to Skills"
        <| fun _ ->
            let allSkillRanksFromDecos =
                decorations |> List.map (fun deco -> deco.Skills |> List.ofArray) |> List.concat

            for skill in skills do
                let matchingSkills =
                    allSkillRanksFromDecos |> List.filter (fun sr -> skillRankOfSkill skill sr)

                Expect.isTrue
                    (Set.isSubset (set matchingSkills) (set skill.Ranks))
                    $"Matching SkillRanks for {skill.Name} must be in the set of SkillRanks in {skill.Name}"

            for decoration in decorations do
                for skillRank in decoration.Skills do
                    let matchingSkill =
                        skills
                        |> List.filter (fun skill -> skillRankOfSkill skill skillRank)
                        |> List.tryExactlyOne

                    Expect.isSome matchingSkill $"All SkillRanks in {decoration.Name} have a corresponding skill."



        testCase "Can detect if a decoration contains, or does not contain, a given skill"
        <| fun _ ->
            let decorationsWithoutOtherSkills =
                [
                    "Botany Jewel 1" // One Skill
                    "Attack Jewel+ 4" // One Skill, Rank 2
                    "Hard Defense Jewel 4" // One Skill, Rank 3
                ]
                |> stringMatch decorations (fun deco -> deco.Name)

            let decorationsWithOtherSkills =
                [
                    "Guardian/Expert Jewel 4" // Two Skills
                ]
                |> stringMatch decorations (fun deco -> deco.Name)

            let decorationsUnderTest =
                decorationsWithoutOtherSkills @ decorationsWithOtherSkills

            let expectedSkills =
                [
                    [ "Botanist" ]
                    [ "Attack Boost" ]
                    [ "Defense Boost" ]
                    [ "Offensive Guard"; "Critical Eye" ]
                ]
                |> stringTraverse skills (fun skill -> skill.Name)

            let expectedOtherSkills = [ false; false; false; true ]

            let notExpectedSkills =
                [ "Clutch Claw Boost"; "Honey Hunter" ]
                |> List.map (fun skillName ->
                    skills
                    |> List.filter (fun skill -> skill.Name = skillName)
                    |> List.tryExactlyOne)

            Expect.all notExpectedSkills Option.isSome "All specified not expected skill results were found"
            let notExpectedSkills = notExpectedSkills |> List.choose id

            for deco, skillList, expectOthers in (List.zip3 decorationsUnderTest expectedSkills expectedOtherSkills) do
                for skill in skillList do
                    Expect.isTrue (deco |> decoContainsSkill skill) $"{deco.Name} should contain {skill.Name}"

                    Expect.equal
                        (deco |> decoContainsOtherSkill skill)
                        expectOthers
                        $"{deco.Name} having other skills should be {expectOthers}"

                for skill in notExpectedSkills do
                    Expect.isFalse (deco |> decoContainsSkill skill) $"{deco.Name} should not contain {skill.Name}"

                    Expect.isTrue
                        (deco |> decoContainsOtherSkill skill)
                        $"{deco.Name} should have a different skill from {skill.Name}"



        testCase "Can identify level of skill in decoration"
        <| fun _ ->

            let decorationsUnderTest =
                [
                    "Botany Jewel 1" // One Skill
                    "Attack Jewel+ 4" // One Skill, Rank 2
                    "Hard Defense Jewel 4" // One Skill, Rank 3
                    "Guardian/Expert Jewel 4"
                ]
                |> stringMatch decorations (fun deco -> deco.Name)

            let expectedSkills =
                [
                    [ "Botanist" ]
                    [ "Attack Boost" ]
                    [ "Defense Boost" ]
                    [ "Offensive Guard"; "Critical Eye" ]
                ]
                |> stringTraverse skills (fun skill -> skill.Name)

            let expectedSkillLevels = [ [ 1 ]; [ 2 ]; [ 3 ]; [ 1; 1 ] ]

            let notExpectedSkill =
                [ "Clutch Claw Boost" ]
                |> stringMatch skills (fun skill -> skill.Name)
                |> List.head

            for deco, skillList, expectedLevel in (List.zip3 decorationsUnderTest expectedSkills expectedSkillLevels) do
                for skill, level in List.zip skillList expectedLevel do
                    Expect.equal (decoSkillLevel skill deco) (Some level) $"{deco.Name} should have skill level {level}"

                    Expect.isNone
                        (decoSkillLevel notExpectedSkill deco)
                        $"{deco.Name} should have no skill level for unexpected skill {notExpectedSkill.Name}"



        testCase "Can Identify and select singleton decorations"
        <| fun _ ->
            let skillsToTest =
                [
                    "Health Boost" // Only Size 1
                    "Palico Rally" // Size 1 and +
                    "Divine Blessing" // Size 1 and Mixed
                    "Defense Boost" // Size 1, +, and Hard
                    "Attack Boost" // Size 1, +, and Mixed
                    "Guard Up" // Only Size 2
                    "Weakness Exploit" // Size 2 and Mixed
                    "Agitator" // Size 2, +, and Mixed
                    "Clutch Claw Boost" // Only Size 3
                    "Handicraft" // Size 3, +, and Mixed
                ]
                |> stringMatch skills (fun skill -> skill.Name)

            let expectedSingletons =
                [
                    "Vitality Jewel 1"
                    "Meowster Jewel 1"
                    "Protection Jewel 1"
                    "Defense Jewel 1"
                    "Attack Jewel 1"
                    "Shield Jewel 2"
                    "Tenderizer Jewel 2"
                    "Challenger Jewel 2"
                    "Shaver Jewel 3"
                    "Handicraft Jewel 3"
                ]
                |> stringMatch decorations (fun decoration -> decoration.Name)

            for skill, decoration in List.zip skillsToTest expectedSingletons do
                Expect.equal
                    (singletonDecoration decorations skill)
                    (Some decoration)
                    $"The singleton decoration for {skill.Name} should be {decoration.Name}"

            let skillsToTest =
                [
                    "Health Boost" // Size 1
                    "Defense Boost" // Size 1, +, and Hard
                    "Attack Boost" // Size 1, +, and Mixed
                    "Agitator" // Size 2, +, and Mixed
                    "Handicraft" // Size 3, +, and Mixed
                ]
                |> stringMatch skills (fun skill -> skill.Name)

            let notSingletons =
                [
                    [ "Meowster Jewel 1" ]
                    [ "Defense Jewel+ 4"; "Hard Defense Jewel 4" ]
                    [ "Attack Jewel+ 4"; "Guardian/Attack Jewel 4"; "Guardian Jewel 2" ]
                    [ "Challenger Jewel+ 4"; "Challenger/Protection Jewel 4" ]
                    [ "Handicraft Jewel+ 4"; "Handicraft/Evasion Jewel 4" ]
                ]
                |> stringTraverse decorations (fun deco -> deco.Name)

            for skill, notSingleton in List.zip skillsToTest notSingletons do
                for decoration in notSingleton do
                    Expect.isFalse
                        (isSingletonDecoration skill decoration)
                        $"{decoration.Name} should not be the singleton for {skill.Name}"


        testCase "Can Identify slot sizes"
        <| fun _ ->

            let skillsToTest =
                [
                    "Effluvial Expert"
                    "Honey Hunter" // No slot
                    "Attack Boost"
                    "Defense Boost"
                    "Paralysis Resistance" // Size 1
                    "Weakness Exploit"
                    "Constitution"
                    "Guard Up" // Size 2
                    "Clutch Claw Boost"
                    "Handicraft"
                    "Free Elem/Ammo Up" // Size 3
                    "Inheritance"
                    "Joy's Gift" // Set skills (No slot)
                ]
                |> stringMatch skills (fun skill -> skill.Name)

            let results = [ for skill in skillsToTest -> skill |> decorationSlotSize decorations ]

            let expectedResults =
                [
                    None
                    None
                    Some 1
                    Some 1
                    Some 1
                    Some 2
                    Some 2
                    Some 2
                    Some 3
                    Some 3
                    Some 3
                    None
                    None
                ]
                |> List.map (Option.map Slot)

            Expect.equal
                results
                expectedResults
                "Skills should correctly identify the slot size of their singleton decoration if it exists"
    ]

let decorationAssignmentFull =
    let skillNames, skills = InferredTypes.Skill.loadSkills |> Async.RunSynchronously

    let decorations =
        InferredTypes.Decoration.loadDecorations skillNames |> Async.RunSynchronously

    let skillCaps =
        skills
        |> List.map (fun skill -> skill, (skill.Ranks |> Array.map (fun sr -> sr.Level) |> Array.max))
        |> Map.ofList


    testList "DecorationAssignmentFull" [
        testCase "Can find a correct mapping from skills to decorations, 4* Only"
        <| fun _ ->

            let expectedSkills =
                [ "Attack Boost"; "Offensive Guard"; "Critical Eye" ]
                |> stringMatch skills (fun skill -> skill.Name)

            let requestedSkills = List.zip expectedSkills [ 2; 2; 2 ]

            let decorations =
                [
                    "Attack Jewel+ 4"
                    "Expert Jewel+ 4"
                    "Guardian/Attack Jewel 4"
                    "Guardian/Expert Jewel 4"
                ]
                |> stringMatch decorations (fun deco -> deco.Name)

            let availableDecorations = List.zip decorations [ 1; 1; 1; 2 ]

            let slots = [ Slot 4; Slot 4; Slot 4 ] |> List.countBy id

            let assignment = assignDecorations skills requestedSkills slots availableDecorations

            let assignedDecorations =
                assignment
                |> Option.defaultValue []
                |> List.choose (fun (slot, maybeDeco) -> maybeDeco)

            let expectedAssignment =
                [ "Attack Jewel+ 4"; "Guardian/Expert Jewel 4"; "Guardian/Expert Jewel 4" ]
                |> stringMatch decorations (fun deco -> deco.Name)

            Expect.equal
                (assignedDecorations |> List.sort)
                (expectedAssignment |> List.sort)
                "Should have found a successful assignment on a small request with only 4* slots"





        testCase "Can find a correct mapping from skills to decorations, with smaller decorations"
        <| fun _ ->

            let expectedSkills =
                [ "Attack Boost"; "Offensive Guard"; "Critical Eye" ]
                |> stringMatch skills (fun skill -> skill.Name)

            let requestedSkills = List.zip expectedSkills [ 4; 3; 4 ]

            let decorations =
                [
                    "Attack Jewel+ 4"
                    "Expert Jewel+ 4"
                    "Guardian/Attack Jewel 4"
                    "Guardian/Expert Jewel 4"
                    "Guardian Jewel 2"
                    "Attack Jewel 1"
                    "Expert Jewel 1"
                ]
                |> stringMatch decorations (fun deco -> deco.Name)

            let availableDecorations = List.zip decorations [ 1; 1; 1; 2; 1; 2; 1 ]

            let slots =
                [ Slot 4; Slot 4; Slot 4; Slot 4; Slot 2; Slot 1; Slot 1 ] |> List.countBy id

            let assignment = assignDecorations skills requestedSkills slots availableDecorations

            let assignedDecorations =
                assignment
                |> Option.defaultValue []
                |> List.choose (fun (slot, maybeDeco) -> maybeDeco)

            let expectedAssignment =
                [
                    "Guardian Jewel 2"
                    "Attack Jewel+ 4"
                    "Expert Jewel+ 4"
                    "Guardian/Expert Jewel 4"
                    "Guardian/Attack Jewel 4"
                    "Attack Jewel 1"
                    "Expert Jewel 1"
                ]
                |> stringMatch decorations (fun deco -> deco.Name)

            Expect.equal
                (assignedDecorations |> List.sort)
                (expectedAssignment |> List.sort)
                "Should be able to assign when given smaller decorations along with 4*"





        testCase "Can identify max flow in the case where requested skills exceed what decoration slots can support"
        <| fun _ ->

            let expectedSkills =
                [ "Attack Boost"; "Offensive Guard"; "Critical Eye" ]
                |> stringMatch skills (fun skill -> skill.Name)

            let requestedSkills = List.zip expectedSkills [ 4; 3; 4 ]

            let decorations =
                [
                    "Attack Jewel+ 4"
                    "Expert Jewel+ 4"
                    "Guardian/Attack Jewel 4"
                    "Guardian/Expert Jewel 4"
                    "Guardian Jewel 2"
                    "Attack Jewel 1"
                    "Expert Jewel 1"
                ]
                |> stringMatch decorations (fun deco -> deco.Name)

            let availableDecorations = List.zip decorations [ 1; 1; 1; 2; 1; 2; 1 ]

            let slots = [ Slot 4; Slot 4; Slot 4; Slot 1; Slot 1 ] |> List.countBy id

            let assignment = assignDecorations skills requestedSkills slots availableDecorations

            let expectedAssignment =
                [
                    "Guardian Jewel 2"
                    "Attack Jewel+ 4"
                    "Expert Jewel+ 4"
                    "Guardian/Expert Jewel 4"
                    "Guardian/Attack Jewel 4"
                    "Attack Jewel 1"
                    "Expert Jewel 1"
                ]
                |> stringMatch decorations (fun deco -> deco.Name)

            let assignedDecorations =
                assignment
                |> Option.defaultValue []
                |> List.choose (fun (slot, maybeDeco) -> maybeDeco)

            Expect.equal
                assignment
                None
                "(Somehow) allocated decorations when requested skills exceed decoration slot capacity"





        testCase "Can slot smaller decorations in larger slots when needed"
        <| fun _ ->

            let expectedSkills =
                [ "Attack Boost"; "Offensive Guard"; "Critical Eye" ]
                |> stringMatch skills (fun skill -> skill.Name)

            let requestedSkills = List.zip expectedSkills [ 3; 2; 3 ]

            let decorations =
                [
                    "Attack Jewel+ 4"
                    "Expert Jewel+ 4"
                    "Guardian/Attack Jewel 4"
                    "Guardian Jewel 2"
                    "Attack Jewel 1"
                    "Expert Jewel 1"
                ]
                |> stringMatch decorations (fun deco -> deco.Name)

            let availableDecorations = List.zip decorations [ 1; 1; 1; 1; 1; 1 ]

            let slots = [ Slot 4; Slot 4; Slot 4; Slot 4; Slot 1 ] |> List.countBy id

            let assignment = assignDecorations skills requestedSkills slots availableDecorations

            let expectedAssignment =
                [
                    "Guardian Jewel 2"
                    "Attack Jewel+ 4"
                    "Expert Jewel+ 4"
                    "Guardian/Attack Jewel 4"
                    "Expert Jewel 1"
                ]
                |> stringMatch decorations (fun deco -> deco.Name)

            let assignedDecorations =
                assignment
                |> Option.defaultValue []
                |> List.choose (fun (slot, maybeDeco) -> maybeDeco)

            Expect.equal (assignedDecorations |> List.sort) (expectedAssignment |> List.sort) "Upcast failed"






        testCase "Will assign Hard Decorations when needed"
        <| fun _ ->

            let expectedSkills =
                [ "Attack Boost"; "Offensive Guard"; "Critical Eye"; "Defense Boost" ]
                |> stringMatch skills (fun skill -> skill.Name)

            let requestedSkills = List.zip expectedSkills [ 3; 2; 3; 5 ]

            let decorations =
                [
                    "Hard Defense Jewel 4"
                    "Attack Jewel+ 4"
                    "Expert Jewel+ 4"
                    "Guardian/Attack Jewel 4"
                    "Guardian Jewel 2"
                    "Attack Jewel 1"
                    "Expert Jewel 1"
                ]
                |> stringMatch decorations (fun deco -> deco.Name)

            let availableDecorations = List.zip decorations [ 2; 1; 1; 1; 1; 1; 1 ]

            let slots =
                [ Slot 4; Slot 4; Slot 4; Slot 4; Slot 4; Slot 4; Slot 1 ] |> List.countBy id

            let assignment = assignDecorations skills requestedSkills slots availableDecorations

            let expectedAssignment =
                [
                    "Guardian Jewel 2"
                    "Attack Jewel+ 4"
                    "Expert Jewel+ 4"
                    "Guardian/Attack Jewel 4"
                    "Expert Jewel 1"
                    "Hard Defense Jewel 4"
                    "Hard Defense Jewel 4"
                ]
                |> stringMatch decorations (fun deco -> deco.Name)

            let assignedDecorations =
                assignment
                |> Option.defaultValue []
                |> List.choose (fun (slot, maybeDeco) -> maybeDeco)

            Expect.equal
                (assignedDecorations |> List.sort)
                (expectedAssignment |> List.sort)
                "Failed to correctly assign Hard decorations"





        testCase "Time is OK with full decoration collection"
        <| fun _ ->

            let expectedSkills =
                [
                    "Agitator"
                    "Offensive Guard"
                    "Critical Eye"
                    "Critical Boost"
                    "Guard"
                    "Guard Up"
                    "Flinch Free"
                    "Botanist"
                    "Divine Blessing"
                    "Coalescence"
                    "Mind's Eye/Ballistics"
                    "Provoker"
                ]
                |> stringMatch skills (fun skill -> skill.Name)

            let requestedSkills = List.zip expectedSkills [ 2; 3; 4; 3; 2; 1; 1; 3; 5; 1; 1; 1 ]

            let allDecorations =
                decorations
                |> List.map (fun decoration ->
                    decoration,
                    (decoration
                     |> containedSkills skills
                     |> List.map (fun (skill, level) ->
                         skillCaps
                         |> Map.tryFind skill
                         |> Option.defaultValue 0
                         |> (fun x -> int (ceil (float x) / (float level))))
                     |> List.min))

            let slots =
                [
                    Slot 4
                    Slot 4
                    Slot 3
                    Slot 2
                    Slot 2
                    Slot 4
                    Slot 4
                    Slot 4
                    Slot 4
                    Slot 4
                    Slot 4
                    Slot 4
                    Slot 3
                    Slot 3
                    Slot 4
                    Slot 2
                ]
                |> List.countBy id

            let runAssignment () =
                let _ = assignDecorations skills requestedSkills slots allDecorations
                ()

            let threeSeconds () =
                Async.Sleep(3000) |> Async.RunSynchronously

            let assignment = assignDecorations skills requestedSkills slots allDecorations


            Expect.isFasterThan runAssignment threeSeconds "Took longer than three seconds to assign decorations"
            Expect.isSome assignment "Failed to find a decoration assignment"





        testCase "Reserves Slots when requirements are fewer than slots"
        <| fun _ ->

            let expectedSkills =
                [
                    "Agitator"
                    "Offensive Guard"
                    "Critical Eye"
                    "Critical Boost"
                    "Guard"
                    "Guard Up"
                    "Flinch Free"
                    "Botanist"
                    "Divine Blessing"
                    "Coalescence"
                    "Mind's Eye/Ballistics"
                    "Provoker"
                ]
                |> stringMatch skills (fun skill -> skill.Name)

            let requestedSkills = List.zip expectedSkills [ 2; 2; 2; 3; 2; 1; 1; 3; 5; 0; 1; 1 ]

            let allDecorations =
                decorations
                |> List.map (fun decoration ->
                    decoration,
                    (decoration
                     |> containedSkills skills
                     |> List.map (fun (skill, level) ->
                         skillCaps
                         |> Map.tryFind skill
                         |> Option.defaultValue 0
                         |> (fun x -> int (ceil (float x) / (float level))))
                     |> List.min))

            let slots =
                [
                    Slot 4
                    Slot 4
                    Slot 3
                    Slot 2
                    Slot 2
                    Slot 4
                    Slot 4
                    Slot 4
                    Slot 4
                    Slot 4
                    Slot 4
                    Slot 4
                    Slot 3
                    Slot 3
                    Slot 4
                    Slot 2
                ]
                |> List.countBy id

            let runAssignment () =
                let _ = assignDecorations skills requestedSkills slots allDecorations
                ()

            let assignment = assignDecorations skills requestedSkills slots allDecorations


            Expect.isFasterThan runAssignment threeSeconds "Took longer than three seconds to assign decorations"
            Expect.isSome assignment "Failed to find a decoration assignment"

            Expect.hasCountOf
                (assignment |> Option.defaultValue [])
                2u
                (fun (slot, deco) -> deco.IsNone)
                "reserved at least 2 decorations"





        testCase "Can Un-reserve slots if needed to meet requirements"
        <| fun _ ->

            // We expect one 4* with a hard Botanist, 3 4* with pairs, 4 4* with Divine Blessing pairs. All 2* and 3* slots must fill with the exclusive skills.
            // After these skills, we have 2 4* slots, and 2 skills left. One of those skills is Divine Blessing
            // By design, Divine Blessing cannot pair with any other than 4 skills
            // Normally we would expect to fill 2 skills with one 4*
            // In this case, we must assign two single skill decos to the remaining 2 4* slots.
            let expectedSkills = [
                3, "Botanist" // Hard decoration

                // All these can pair with each other
                2, "Offensive Guard"
                3, "Critical Eye"
                2, "Guard"

                // Can pair with Divine Blessing
                3, "Critical Boost"
                1, "Flinch Free"

                // 2* exclusive
                1, "Mind's Eye/Ballistics"
                1, "Guard Up"
                1, "Heat Guard"

                // 3* exclusive
                1, "Provoker"
                1, "Clutch Claw Boost"
                1, "Normal Shots"

                // Under test; Note we expect 5 of this skill, but only have 4 possible pairs.
                5, "Divine Blessing"
            ]

            let counts, names = expectedSkills |> List.unzip
            let matchedSkills = names |> stringMatch skills (fun s -> s.Name)

            let requestedSkills = List.zip matchedSkills counts

            let allDecorations =
                decorations
                |> List.map (fun decoration ->
                    decoration,
                    (decoration
                     |> containedSkills skills
                     |> List.map (fun (skill, level) ->
                         skillCaps
                         |> Map.tryFind skill
                         |> Option.defaultValue 0
                         |> (fun x -> int (ceil (float x) / (float level))))
                     |> List.min))

            let slots =
                [
                    Slot 4
                    Slot 4
                    Slot 3
                    Slot 2
                    Slot 2
                    Slot 4
                    Slot 4
                    Slot 4
                    Slot 4
                    Slot 4
                    Slot 4
                    Slot 4
                    Slot 3
                    Slot 3
                    Slot 4
                    Slot 2
                ]
                |> List.countBy id

            let runAssignment () =
                let _ = assignDecorations skills requestedSkills slots allDecorations
                ()

            let assignment = assignDecorations skills requestedSkills slots allDecorations

            Expect.isFasterThan runAssignment threeSeconds "Took longer than three seconds to assign decorations"
            Expect.isSome assignment "Failed to find a decoration assignment"
    ]