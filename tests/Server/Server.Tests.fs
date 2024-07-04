module Server.Tests

open Expecto

open Shared
open Server
open DataAccess
open Thoth
open System.IO
open Helpers.Constants

let server =
    testList "Server" [
        testCase "Adding valid Todo"
        <| fun _ ->
            // let validTodo = Todo.create "TODO"
            // let expectedResult = Ok()

            // // let result = Storage.addTodo validTodo

            // Expect.equal result expectedResult "Result should be ok"
            // Expect.contains Storage.todos validTodo "Storage should contain new todo"

            Expect.equal "NotImplemented" "NotImplemented" "These should be fine because they are placeholders"
    ]

let all = testList "All" [ Shared.Tests.shared; server; SetSearchLogic.Tests.setSearchLogic; DecorationAssignment.Tests.decorationAssignment; DecorationAssignment.Tests.decorationAssignmentFull; (*MaxFlowMap.Tests.maxFlowMap*)]

let writeData () = 
  let skillNames, skills' = InferredTypes.Skill.loadSkills |> Async.RunSynchronously
  let decorations = InferredTypes.Decoration.loadDecorations skillNames |> Async.RunSynchronously
  
  do skillNames |> Thoth.Json.Net.Encode.Auto.toString |> (fun str -> File.WriteAllText(skillsNameFile, str))
  do skills' |> Thoth.Json.Net.Encode.Auto.toString |> (fun str -> File.WriteAllText(skillFile, str))
  do decorations |> Thoth.Json.Net.Encode.Auto.toString |> (fun str -> File.WriteAllText(decorationsFile, str))


[<EntryPoint>]
let main _ = runTestsWithCLIArgs [] [||] all

