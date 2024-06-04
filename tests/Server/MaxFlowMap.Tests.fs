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
      testCase "Can find a correct mapping from skills to decorations, 4* Only"
        <| fun _ ->
          let allSkillRanksFromDecos = decorations |> List.map (fun deco -> deco.Skills |> List.ofArray) |> List.concat 
          
          let expectedSkills = 
            [ "Attack Boost"
              "Offensive Guard"
              "Critical Eye"
            ] |> stringMatch skills (fun skill -> skill.Name)
          
          let requestedSkills = List.zip expectedSkills [2;2;2]
            
          let decorations = 
           [ "Attack Jewel+ 4";
             "Expert Jewel+ 4";
             "Guardian/Attack Jewel 4"
             "Guardian/Expert Jewel 4"
           ] |> stringMatch decorations (fun deco -> deco.Name)
          let availableDecorations = 
            List.zip decorations [1;1;1;2]
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

          let slots = [Slot 4; Slot 4; Slot 4]

          let graph = assembleMaxFlowByMap requestedSkills availableDecorations slots
          let maxFlowGraph = (ford_fulkerson graph) 

          let assignment = 
            maxFlowGraph 
            |> decorationAssignmentEdges 
            |> List.filter (fun edge -> (maxFlowGraph.Flow |> Map.tryFind edge |> Option.defaultValue 0) > 0)
            |> List.map (fun (deco, slot) -> deco |> (function | Decoration d -> Some d | _ -> None) |> Option.get |> (fun deco -> deco.Decoration))

          let expectedAssignment = 
            [ "Attack Jewel+ 4";
              "Guardian/Expert Jewel 4"
              "Guardian/Expert Jewel 4"
            ] |> stringMatch decorations (fun deco -> deco.Name)

          printfn ""
          printfn ""
          printfn ""
          Expect.equal (assignment |> List.sort) (expectedAssignment |> List.sort) "Assignment should be correct"



      testCase "Can find a correct mapping from skills to decorations, with smaller decorations"
        <| fun _ ->
          let allSkillRanksFromDecos = decorations |> List.map (fun deco -> deco.Skills |> List.ofArray) |> List.concat 
          
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

          let slots = [ Slot 4; Slot 4; Slot 4; Slot 4; Slot 2; Slot 1; Slot 1 ]

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

          Expect.equal (assignment |> List.sort) (expectedAssignment |> List.sort) "Assignment should be correct"



    //   testCase "Can detect if a decoration contains, or does not contain, a given skill"
    //    <| fun _ ->
    //       let decorationsWithoutOtherSkills = 
    //         [ "Botany Jewel 1";   // One Skill 
    //           "Attack Jewel+ 4";   // One Skill, Rank 2
    //           "Hard Defense Jewel 4"; // One Skill, Rank 3
    //         ] |> stringMatch decorations (fun deco -> deco.Name)

    //       let decorationsWithOtherSkills = 
    //         [
    //           "Guardian/Expert Jewel 4" // Two Skills
    //         ] |> stringMatch decorations (fun deco -> deco.Name)

    //       let decorationsUnderTest = decorationsWithoutOtherSkills @ decorationsWithOtherSkills

    //       let expectedSkills = 
    //         [ ["Botanist"]
    //           ["Attack Boost"]
    //           ["Defense Boost"]
    //           ["Offensive Guard"; "Critical Eye"]
    //         ] |> stringTraverse skills (fun skill -> skill.Name)

    //       let expectedOtherSkills = 
    //         [ false
    //           false
    //           false
    //           true
    //         ]

    //       let notExpectedSkills = 
    //         [ "Clutch Claw Boost" 
    //           "Honey Hunter"
    //         ] |> List.map (fun skillName -> skills |> List.filter (fun skill -> skill.Name = skillName) |> List.tryExactlyOne)
    //       Expect.all notExpectedSkills Option.isSome "All specified not expected skill results were found"
    //       let notExpectedSkills = notExpectedSkills |> List.choose id

    //       for deco, skillList, expectOthers in (List.zip3 decorationsUnderTest expectedSkills expectedOtherSkills) do
    //         for skill in skillList do
    //           Expect.isTrue (deco |> decoContainsSkill skill) $"{deco.Name} should contain {skill.Name}"
    //           Expect.equal (deco |> decoContainsOtherSkill skill) expectOthers $"{deco.Name} having other skills should be {expectOthers}"
    //         for skill in notExpectedSkills do
    //           Expect.isFalse (deco |> decoContainsSkill skill) $"{deco.Name} should not contain {skill.Name}"
    //           Expect.isTrue (deco |> decoContainsOtherSkill skill) $"{deco.Name} should have a different skill from {skill.Name}"



    //   testCase "Can identify level of skill in decoration"
    //     <| fun _ -> 
          
    //       let decorationsUnderTest = 
    //         [ "Botany Jewel 1";   // One Skill 
    //           "Attack Jewel+ 4";   // One Skill, Rank 2
    //           "Hard Defense Jewel 4"; // One Skill, Rank 3
    //           "Guardian/Expert Jewel 4"
    //         ] |> stringMatch decorations (fun deco -> deco.Name)

    //       let expectedSkills = 
    //         [ ["Botanist"]
    //           ["Attack Boost"]
    //           ["Defense Boost"]
    //           ["Offensive Guard"; "Critical Eye"]
    //         ] |> stringTraverse skills (fun skill -> skill.Name)

    //       let expectedSkillLevels = 
    //         [ [1]
    //           [2]
    //           [3]
    //           [1;1]
    //         ]

    //       let notExpectedSkill = 
    //         [ "Clutch Claw Boost" 
    //         ] |> stringMatch skills (fun skill -> skill.Name)
    //         |> List.head
          
    //       for deco, skillList, expectedLevel in (List.zip3 decorationsUnderTest expectedSkills expectedSkillLevels) do
    //         for skill, level in List.zip skillList expectedLevel do
    //           Expect.equal (decoSkillLevel skill deco) (Some level) $"{deco.Name} should have skill level {level}"
    //           Expect.isNone (decoSkillLevel notExpectedSkill deco) $"{deco.Name} should have no skill level for unexpected skill {notExpectedSkill.Name}"



    //   testCase "Can Identify and select singleton decorations"
    //     <| fun _ -> 
    //       let skillsToTest =
    //         [ "Health Boost"         // Only Size 1
    //           "Palico Rally"         // Size 1 and +
    //           "Divine Blessing"      // Size 1 and Mixed
    //           "Defense Boost"        // Size 1, +, and Hard
    //           "Attack Boost"         // Size 1, +, and Mixed
    //           "Guard Up"             // Only Size 2
    //           "Weakness Exploit"     // Size 2 and Mixed
    //           "Agitator"             // Size 2, +, and Mixed
    //           "Clutch Claw Boost"    // Only Size 3
    //           "Handicraft"           // Size 3, +, and Mixed
    //         ] |> stringMatch skills (fun skill -> skill.Name)

    //       let expectedSingletons =
    //         [ "Vitality Jewel 1"
    //           "Meowster Jewel 1"
    //           "Protection Jewel 1"
    //           "Defense Jewel 1"
    //           "Attack Jewel 1"
    //           "Shield Jewel 2"
    //           "Tenderizer Jewel 2"
    //           "Challenger Jewel 2"
    //           "Shaver Jewel 3"
    //           "Handicraft Jewel 3"
    //         ] |> stringMatch decorations (fun decoration -> decoration.Name)

    //       for skill, decoration in List.zip skillsToTest expectedSingletons do
    //         Expect.equal (singletonDecoration decorations skill) (Some decoration) $"The singleton decoration for {skill.Name} should be {decoration.Name}"

    //       let skillsToTest = 
    //         [ "Health Boost"         // Size 1
    //           "Defense Boost"        // Size 1, +, and Hard
    //           "Attack Boost"         // Size 1, +, and Mixed
    //           "Agitator"             // Size 2, +, and Mixed
    //           "Handicraft"           // Size 3, +, and Mixed
    //         ] |> stringMatch skills (fun skill -> skill.Name)

    //       let notSingletons = 
    //         [ [ "Meowster Jewel 1"]
    //           [ "Defense Jewel+ 4"; "Hard Defense Jewel 4" ]
    //           [ "Attack Jewel+ 4"; "Guardian/Attack Jewel 4" ; "Guardian Jewel 2"]
    //           [ "Challenger Jewel+ 4"; "Challenger/Protection Jewel 4" ]
    //           [ "Handicraft Jewel+ 4"; "Handicraft/Evasion Jewel 4" ]
    //         ] |> stringTraverse decorations (fun deco -> deco.Name)

    //       for skill, notSingleton in List.zip skillsToTest notSingletons do
    //         for decoration in notSingleton do
    //           Expect.isFalse (isSingletonDecoration skill decoration) $"{decoration.Name} should not be the singleton for {skill.Name}"


    //   testCase "Can Identify slot sizes"
    //     <| fun _ -> 

    //       let skillsToTest = 
    //         [ "Effluvial Expert"; "Honey Hunter";                       // No slot
    //           "Attack Boost"; "Defense Boost"; "Paralysis Resistance"   // Size 1
    //           "Weakness Exploit"; "Constitution"; "Guard Up"            // Size 2
    //           "Clutch Claw Boost"; "Handicraft"; "Free Elem/Ammo Up"    // Size 3
    //           "Inheritance"; "Joy's Gift"                               // Set skills (No slot)
    //         ] |> stringMatch skills (fun skill -> skill.Name)

    //       let results = [
    //         for skill in skillsToTest ->
    //           skill |> decorationSlotSize decorations
    //       ]

    //       let expectedResults = 
    //         [ None; None;
    //           Some 1; Some 1; Some 1;
    //           Some 2; Some 2; Some 2;
    //           Some 3; Some 3; Some 3;
    //           None; None;
    //         ] |> List.map (Option.map Slot)

    //       Expect.equal results expectedResults "Skills should correctly identify the slot size of their singleton decoration if it exists"
    ]