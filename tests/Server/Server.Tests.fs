module Server.Tests

open Expecto

open Shared
open Server
open DataAccess
open Thoth
open System.IO
open Helpers.Constants

let all =
    testList "All" [
        Shared.Tests.shared
        APIData.Tests.gameDataTypes
        DecorationAssignment.Tests.decorationAssignment
        SetSearchLogic.Tests.setSearchLogic

    ]

let writeData () =
    let skillNames, skills = InferredTypes.Skill.loadSkills |> Async.RunSynchronously

    let decorations =
        InferredTypes.Decoration.loadDecorations skillNames |> Async.RunSynchronously

    do
        skillNames
        |> Thoth.Json.Net.Encode.Auto.toString
        |> (fun str -> File.WriteAllText(skillsNameFile, str))

    do
        skills
        |> Thoth.Json.Net.Encode.Auto.toString
        |> (fun str -> File.WriteAllText(skillFile, str))

    do
        decorations
        |> Thoth.Json.Net.Encode.Auto.toString
        |> (fun str -> File.WriteAllText(decorationsFile, str))


[<EntryPoint>]
let main _ = runTestsWithCLIArgs [] [||] all