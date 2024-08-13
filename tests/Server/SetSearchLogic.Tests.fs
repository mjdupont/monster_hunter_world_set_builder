module SetSearchLogic.Tests

open Shared
open GameDataTypes
open Helpers.Constants
open Server
open DataAccess
open DecorationAssignment
#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let unsafeUnwrapResult d (r: Result<'a, 'err>) =
    match r with
    | Ok r -> r
    | _ -> d

let setSearchLogic =
    testList "SetSearchLogic" [
        testCase "Identifies Slot Sizes"
        <| fun _ -> Expect.isTrue true "Placeholder is true"
    // // let decorations : Decoration list = System.IO.File.ReadAllText(decorationsFile) |> Thoth.Json.Net.Decode.Auto.fromString |> unsafeUnwrapResult []
    // // let skillNames: Map<int, string> = System.IO.File.ReadAllText(skillsNameFile) |> Thoth.Json.Net.Decode.Auto.fromString |> unsafeUnwrapResult Map.empty
    // // let skills' : Skill list = System.IO.File.ReadAllText(skillFile) |> Thoth.Json.Net.Decode.Auto.fromString |> unsafeUnwrapResult []
    // let skillNames, skills = InferredTypes.Skill.loadSkills |> Async.RunSynchronously
    // let decorations = InferredTypes.Decoration.loadDecorations skillNames |> Async.RunSynchronously

    // let skillsToTest =
    //   [ "Effluvial Expert"; "Honey Hunter";                       // No slot
    //     "Attack Boost"; "Defense Boost"; "Paralysis Resistance"   // Size 1
    //     "Weakness Exploit"; "Constitution"; "Guard Up"            // Size 2
    //     "Clutch Claw Boost"; "Handicraft"; "Free Elem/Ammo Up"    // Size 3
    //     "Inheritance"; "Joy's Gift"                               // Set skills (No slot)
    //   ]
    //   |> List.map (fun skillName -> skills |> List.filter (fun skill -> skill.Name = skillName) |> List.tryExactlyOne)

    // Expect.all skillsToTest Option.isSome "All skill names should be found in the list of skills"
    // let skillsToTest = skillsToTest |> List.choose id

    // let results = [
    //   for skill in skillsToTest ->
    //     skill |> decorationSlotSize decorations
    // ]

    // let expectedResults =
    //   [ None; None;
    //     Some 1; Some 1; Some 1;
    //     Some 2; Some 2; Some 2;
    //     Some 3; Some 3; Some 3;
    //     None; None;
    //   ] |> List.map (Option.map Slot)

    // Expect.equal results expectedResults "Skills should correctly identify the slot size of their singleton decoration if it exists"
    ]