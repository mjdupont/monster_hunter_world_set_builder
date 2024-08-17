namespace Components

module ArmorSetSkillsDisplay =
  open Feliz
  open ModelData
  open APIDataTypes


  [<ReactComponent>]
  let Component (props: {|GameData: MHWData; ChosenSet : ChosenSet|}) =

      let armorSetBonuses = props.ChosenSet |> ChosenSet.armorSetBonuses props.GameData.ArmorSets

      let totalSkills = (props.ChosenSet |> ChosenSet.allSkillRanks |> accumulateSkills)

      let totalSkillsElement = [
          for skill in totalSkills do
              let skillFromData =
                  props.GameData.Skills
                  |> Seq.filter (fun skillData -> skillData.Id = skill.Skill)
                  |> Seq.tryExactlyOne

              let skillColor =
                  match skillFromData with
                  | None -> "black"
                  | Some skillData ->
                      let maxLevel =
                          skillData.Ranks
                          |> Seq.sortByDescending (fun sr -> sr.Level)
                          |> Seq.head
                          |> (fun sr -> sr.Level)

                      match skill with
                      | s when s.Level = maxLevel -> "green"
                      | s when s.Level > maxLevel -> "red"
                      | _ -> "black"

              yield
                  Html.div [
                      prop.className ""
                      prop.children [
                          Html.h3 [
                              prop.style [ style.color skillColor ]
                              prop.text (sprintf "%s: %i" skill.SkillName skill.Level)
                          ]
                      ]
                  ]
      ]

      let armorSetSkillsElement = [
          for bonus, rank in armorSetBonuses ->
              Html.div [
                  prop.className ""
                  prop.children [
                      Html.h2 [
                          prop.style [ style.color "black" ]
                          prop.text (sprintf "%s - %s" bonus.Name rank.Skill.SkillName)
                      ]
                  ]
              ]
      ]
    
      Html.div [
          prop.className "armor-summary m-auto bg-white/80 rounded-md shadow-md p-4"
          prop.children ([ armorSetSkillsElement; totalSkillsElement ] |> List.concat)
      ]