namespace Components

  module SelectedSkill = 
    open Feliz

    open DataTypes
    
    [<RequireQualifiedAccess>]
    type Properties =
      { Skill: Skill
        Rank: int
        RemoveSkillCallBack: Skill -> unit
      }

    [<ReactComponent>]
    let Component (props:Properties) =
      printfn "%A" (props.Skill, props.Rank)

      Html.div [
        prop.className "flex flex-row gap-8"
        prop.children [
          Html.text (sprintf "%s %i" props.Skill.Name props.Rank)
          Html.button [
            prop.type' "button"
            prop.onClick (fun _me -> props.RemoveSkillCallBack props.Skill)
            prop.children [
              Html.text "Remove Skill"
            ]
          ]
        ]
      ]
      
