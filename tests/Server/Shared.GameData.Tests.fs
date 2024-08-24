module APIData.Tests

open Shared
open SetSearchLogic
open APIDataTypes
open GameData.APIData
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


let gameDataTypes =
    let skillNames, skills = InferredTypes.Skill.loadSkills |> Async.RunSynchronously

    let decorations =
        InferredTypes.Decoration.loadDecorations skillNames |> Async.RunSynchronously

    testList "GameDataTypes" [
        testCase "skillRankOfSkill: Matches SkillRanks to Skills"
        <| fun _ ->
            let allSkillRanksFromDecos =
                decorations |> List.map (fun deco -> deco.Skills) |> List.concat

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



        testCase
            "decoContainsSkill/decoContainsOtherSkill: Can detect if a decoration contains, or does not contain, a given skill"
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

        let dataForHardDecorations =
            let requestedSkills = [
                "Attack Boost", 4
                "Defense Boost", 7 // 2 Contribution
                "Weakness Exploit", 3
                "Constitution", 3
                "Clutch Claw Boost", 1
                "Geologist", 3 // 1 Contribution

            ]

            let skillNames, levels = requestedSkills |> List.unzip
            let requestedSkillNames = skillNames |> stringMatch skills (fun skill -> skill.Name)
            let requestedSkills = List.zip requestedSkillNames levels

            let decorations = allDecorations skills decorations

            let slots = [ (Slot 4, 3); (Slot 3, 2); (Slot 2, 2); (Slot 1, 4) ]
            (requestedSkills, decorations, slots)


        testCase "Can calculate hard decoration skill contribution"
        <| fun _ ->

            let results = hardSkillContribution <||| dataForHardDecorations

            Expect.equal results 3 "Failed to calculate excess reach achieved with hard decorations"

        testCase "Reduces count when fewer hard decorations"
        <| fun _ ->

            let requestedSkills, decorations, slots = dataForHardDecorations

            let decorations =
                decorations
                |> List.map (fun (hardDeco, count) ->
                    match
                        hardDeco.Skills
                        |> List.tryExactlyOne
                        |> Option.filter (fun sr -> sr.SkillName = "Defense Boost" && sr.Level = 3)
                    with
                    | Some _ -> (hardDeco, 1)
                    | _ -> (hardDeco, count))

            let results = hardSkillContribution requestedSkills decorations slots

            Expect.equal
                results
                2
                "Did not reduce skill contribution when only a single hard defense decoration existed"

        testCase "Reduces count when fewer size 4 slots"
        <| fun _ ->

            let requestedSkills, decorations, slots = dataForHardDecorations

            let slots =
                slots
                |> List.map (fun ((Slot s), count) -> if s = 4 then (Slot 4), 1 else (Slot s), count)

            let results = hardSkillContribution requestedSkills decorations slots

            Expect.equal results 1 "Did not reduce skill contribution when only one slots was available"

        testCase "Reduces count with less skillNeed"
        <| fun _ ->

            let requestedSkills, decorations, slots = dataForHardDecorations

            let requestedSkills =
                requestedSkills
                |> List.map (fun (skill, req) ->
                    if skill.Name = "Defense Boost" then
                        (skill, 4)
                    else
                        skill, req)

            let results = hardSkillContribution requestedSkills decorations slots

            Expect.equal
                results
                2
                "Did not reduce skill contribution when requesting a lower level of skills than hard decos could provide"
    ]