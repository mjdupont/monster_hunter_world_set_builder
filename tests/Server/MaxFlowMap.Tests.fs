module MaxFlowMap.Tests

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
open MaxFlowMap
#endif

let unsafeUnwrapResult d (r:Result<'a, 'err>) =
  match r with
  | Ok r -> r
  | _ -> d

let stringMatch elements (name: 'a -> string) strings =
  let matchingElements = strings |> List.map (fun decoName -> elements |> List.filter (fun elem -> name elem = decoName) |> List.tryExactlyOne)
  Expect.all matchingElements Option.isSome "All matching elements were found"
  matchingElements |> List.choose id

let stringTraverse elements name strings =
  let matchingElements = strings |> List.map (Option.traverseList (fun elemName -> elements |> List.filter (fun elem -> name elem = elemName) |> List.tryExactlyOne))
  Expect.all matchingElements Option.isSome "All matching elements were found"
  matchingElements |> List.choose id



let maxFlowMap =
    let skillNames, skills = InferredTypes.Skill.loadSkills |> Async.RunSynchronously
    let decorations = InferredTypes.Decoration.loadDecorations skillNames |> Async.RunSynchronously

    testList "MaxFlowMap" [
      // testCase "Can find a correct mapping from skills to decorations, 4* Only"
      //   <| fun _ ->

      //     let expectedSkills =
      //       [ "Attack Boost"
      //         "Offensive Guard"
      //         "Critical Eye"
      //       ] |> stringMatch skills (fun skill -> skill.Name)

      //     let requestedSkills = List.zip expectedSkills [2;3;2]

      //     let decorations =
      //      [ "Attack Jewel+ 4";
      //        "Expert Jewel+ 4";
      //        "Guardian/Attack Jewel 4"
      //        "Guardian/Expert Jewel 4"
      //      ] |> stringMatch decorations (fun deco -> deco.Name)
      //     let availableDecorations =
      //       List.zip decorations [1;1;1;2]
      //     let availableDecorations =
      //       [ for (decoration, count) in availableDecorations do
      //           for skillRank in decoration.Skills do
      //             let skill =
      //               skills |> List.filter (fun skill -> skillRankOfSkill skill skillRank) |> List.tryExactlyOne
      //             match skill with
      //             | Some skill -> yield skill, (decoration, count)
      //             | _ -> ()
      //       ]
      //       |> List.groupBy fst
      //       |> List.map (fun (skill, elements) -> skill, elements |> List.map (fun (skill', (deco , count)) -> (deco, count)) |> Map.ofList) |> Map.ofList

      //     let slots = [Slot 4; Slot 4; Slot 4]

      //     let graph = assembleMaxFlowByMap requestedSkills availableDecorations slots
      //     let maxFlowGraph = (ford_fulkerson graph)

      //     let assignment =
      //       maxFlowGraph
      //       |> decorationAssignmentEdges
      //       |> List.filter (fun edge -> (maxFlowGraph.Flow |> Map.tryFind edge |> Option.defaultValue 0) > 0)
      //       |> List.map (fun (deco, slot) -> deco |> (function | Decoration d -> Some d | _ -> None) |> Option.get |> (fun deco -> deco.Decoration))

      //     let expectedAssignment =
      //       [ "Attack Jewel+ 4";
      //         "Guardian/Expert Jewel 4"
      //         "Guardian/Expert Jewel 4"
      //       ] |> stringMatch decorations (fun deco -> deco.Name)

      //     Expect.equal (assignment |> List.sort) (expectedAssignment |> List.sort) "Should have found a successful assignment on a small request with only 4* slots"



      // testCase "Can find a correct mapping from skills to decorations, with smaller decorations"
      //   <| fun _ ->

      //     let expectedSkills =
      //       [ "Attack Boost"
      //         "Offensive Guard"
      //         "Critical Eye"
      //       ] |> stringMatch skills (fun skill -> skill.Name)

      //     let requestedSkills = List.zip expectedSkills [4;3;4]

      //     let decorations =
      //      [ "Attack Jewel+ 4";
      //        "Expert Jewel+ 4";
      //        "Guardian/Attack Jewel 4"
      //        "Guardian/Expert Jewel 4"
      //        "Guardian Jewel 2"
      //        "Attack Jewel 1"
      //        "Expert Jewel 1"
      //      ] |> stringMatch decorations (fun deco -> deco.Name)
      //     let availableDecorations =
      //       List.zip decorations [1;1;1;2;1;2;1]
      //     let availableDecorations =
      //       [ for (decoration, count) in availableDecorations do
      //           for skillRank in decoration.Skills do
      //             let skill =
      //               skills |> List.filter (fun skill -> skillRankOfSkill skill skillRank) |> List.tryExactlyOne
      //             match skill with
      //             | Some skill -> yield skill, (decoration, count)
      //             | _ -> ()
      //       ]
      //       |> List.groupBy fst
      //       |> List.map (fun (skill, elements) -> skill, elements |> List.map (fun (skill', (deco , count)) -> (deco, count)) |> Map.ofList) |> Map.ofList

      //     let slots = [ Slot 4; Slot 4; Slot 4; Slot 4; Slot 2; Slot 1; Slot 1 ]

      //     let graph = assembleMaxFlowByMap requestedSkills availableDecorations slots
      //     let maxFlowGraph = (ford_fulkerson graph)

      //     let assignment =
      //       maxFlowGraph
      //       |> decorationAssignmentEdges
      //       |> List.filter (fun edge -> (maxFlowGraph.Flow |> Map.tryFind edge |> Option.defaultValue 0) > 0)
      //       |> List.map (fun (deco, slot) -> deco |> (function | Decoration d -> Some d | _ -> None) |> Option.get |> (fun deco -> deco.Decoration))

      //     let expectedAssignment =
      //       [ "Guardian Jewel 2"
      //         "Attack Jewel+ 4"
      //         "Expert Jewel+ 4"
      //         "Guardian/Expert Jewel 4"
      //         "Guardian/Attack Jewel 4"
      //         "Attack Jewel 1"
      //         "Expert Jewel 1"
      //       ] |> stringMatch decorations (fun deco -> deco.Name)

      //     Expect.equal (assignment |> List.sort) (expectedAssignment |> List.sort) "Assignment should be correct"

      testCase "Can identify max flow in the case where requested skills exceed what decoration slots can support"
        <| fun _ ->

          let expectedSkills =
            [ "Attack Boost"
              "Offensive Guard"
              "Critical Eye"
            ] |> stringMatch skills (fun skill -> skill.Name)

          let requestedSkills = List.zip expectedSkills [4;3;4]

          let decorations =
           [ "Attack Jewel+ 4";
             "Expert Jewel+ 4";
             "Guardian/Attack Jewel 4"
             "Guardian/Expert Jewel 4"
             "Guardian Jewel 2"
             "Attack Jewel 1"
             "Expert Jewel 1"
           ] |> stringMatch decorations (fun deco -> deco.Name)
          let availableDecorations =
            List.zip decorations [1;1;1;2;1;2;1]
          let availableDecorations =
            [ for (decoration, count) in availableDecorations do
                for skillRank in decoration.Skills do
                  let skill =
                    skills |> List.filter (fun skill -> skillRankOfSkill skill skillRank) |> List.tryExactlyOne
                  match skill with
                  | Some skill -> yield skill, (decoration, count)
                  | _ -> ()
            ]
            |> List.groupBy fst
            |> List.map (fun (skill, elements) -> skill, elements |> List.map (fun (skill', (deco , count)) -> (deco, count)) |> Map.ofList) |> Map.ofList

          let slots = [ Slot 4; Slot 4; Slot 4; Slot 1; Slot 1 ]

          let graph = assembleMaxFlowByMap requestedSkills availableDecorations slots
          let maxFlowGraph = (ford_fulkerson graph)

          let assignment =
            maxFlowGraph
            |> decorationAssignmentEdges
            |> List.filter (fun edge -> (maxFlowGraph.Flow |> Map.tryFind edge |> Option.defaultValue 0) > 0)
            |> List.map (fun (deco, slot) -> deco |> (function | Decoration d -> Some d | _ -> None) |> Option.get |> (fun deco -> deco.Decoration))

          let expectedAssignment =
            [ "Guardian Jewel 2"
              "Attack Jewel+ 4"
              "Expert Jewel+ 4"
              "Guardian/Expert Jewel 4"
              "Guardian/Attack Jewel 4"
              "Attack Jewel 1"
              "Expert Jewel 1"
            ] |> stringMatch decorations (fun deco -> deco.Name)

          Expect.equal (assignment |> List.sort) (expectedAssignment |> List.sort) "Identifies a correct assignment on a larger graph with smaller sized decorations"
    ]