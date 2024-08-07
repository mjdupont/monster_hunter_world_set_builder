module DecorationAssignment.Tests

open Shared
open SetSearchLogic
open GameDataTypes
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

            let assignment = findDecorationsSatisfyingSkills skills requestedSkills slots availableDecorations

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

            let assignment = findDecorationsSatisfyingSkills skills requestedSkills slots availableDecorations

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

            let assignment = findDecorationsSatisfyingSkills skills requestedSkills slots availableDecorations

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

            let assignment = findDecorationsSatisfyingSkills skills requestedSkills slots availableDecorations

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

            let assignment = findDecorationsSatisfyingSkills skills requestedSkills slots availableDecorations

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
                         skillCaps skills
                         |> Map.tryFind skill
                         |> Option.defaultValue 0
                         |> (fun x -> int (ceil (float x) / (float level))))
                     |> List.min))

            let slots =
                [
                    [ Slot 4; Slot 4 ]
                    [ Slot 3; Slot 2; Slot 2 ]
                    [ Slot 4; Slot 4; Slot 4 ]
                    [ Slot 4; Slot 4; Slot 4 ]
                    [ Slot 4; Slot 3; Slot 3 ]
                    [ Slot 4; Slot 2 ]
                ]
                |> List.concat
                |> List.countBy id

            let runAssignment () =
                let _ = findDecorationsSatisfyingSkills skills requestedSkills slots allDecorations
                ()

            let threeSeconds () =
                Async.Sleep(3000) |> Async.RunSynchronously

            let assignment = findDecorationsSatisfyingSkills skills requestedSkills slots allDecorations


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
                         skillCaps skills
                         |> Map.tryFind skill
                         |> Option.defaultValue 0
                         |> (fun x -> int (ceil (float x) / (float level))))
                     |> List.min))

            let slots =
                [
                    [ Slot 4; Slot 4 ]
                    [ Slot 3; Slot 2; Slot 2 ]
                    [ Slot 4; Slot 4; Slot 4 ]
                    [ Slot 4; Slot 4; Slot 4 ]
                    [ Slot 4; Slot 3; Slot 3 ]
                    [ Slot 4; Slot 2 ]
                ]
                |> List.concat
                |> List.countBy id

            let runAssignment () =
                let _ = findDecorationsSatisfyingSkills skills requestedSkills slots allDecorations
                ()

            let assignment = findDecorationsSatisfyingSkills skills requestedSkills slots allDecorations


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
                         skillCaps skills
                         |> Map.tryFind skill
                         |> Option.defaultValue 0
                         |> (fun x -> int (ceil (float x) / (float level))))
                     |> List.min))

            let slots =
                [
                    [ Slot 4; Slot 4 ]
                    [ Slot 3; Slot 2; Slot 2 ]
                    [ Slot 4; Slot 4; Slot 4 ]
                    [ Slot 4; Slot 4; Slot 4 ]
                    [ Slot 4; Slot 3; Slot 3 ]
                    [ Slot 4; Slot 2 ]
                ]
                |> List.concat
                |> List.countBy id

            let runAssignment () =
                let _ = findDecorationsSatisfyingSkills skills requestedSkills slots allDecorations
                ()

            let assignment = findDecorationsSatisfyingSkills skills requestedSkills slots allDecorations

            Expect.isFasterThan runAssignment threeSeconds "Took longer than three seconds to assign decorations"
            Expect.isSome assignment "Failed to find a decoration assignment"


        testCase "Regression test from example"
        <| fun _ ->

            let expectedSkills = [

                3, "Critical Eye"
                3, "Guard"
                7, "Agitator"
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
                         skillCaps skills
                         |> Map.tryFind skill
                         |> Option.defaultValue 0
                         |> (fun x -> int (ceil (float x) / (float level))))
                     |> List.min))

            let slots =
                [
                  Slot 4, 5;
                  Slot 1, 2;
                  Slot 2, 2
                ]


            let runAssignment () =
                let _ = findDecorationsSatisfyingSkills skills requestedSkills slots allDecorations
                ()

            let assignment = findDecorationsSatisfyingSkills skills requestedSkills slots allDecorations

            Expect.isFasterThan runAssignment threeSeconds "Took longer than three seconds to assign decorations"
            Expect.isSome assignment "Failed to find a decoration assignment"
    ]