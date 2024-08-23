namespace Components

module SelectedSkill =
    open Feliz

    open APIDataTypes



    [<ReactComponent>]
    let Component
        (props:
            {|
                Skill: Skill
                Rank: int
                RemoveSkillCallBack: Skill -> unit
                SetLevel: int -> unit
            |})
        =

        Html.div [
            prop.className "flex flex-row gap-8 justify-between"
            prop.children [
                Html.text (sprintf "%s" props.Skill.Name)
                Html.div [
                    prop.className "flex flex-row gap-.5"
                    prop.children [
                        Html.input [
                            //prop.id (printfn "chosen-skill-%s" props.Skill.Name)
                            prop.type' "number"
                            prop.min 0
                            prop.className "shrink 0"
                            prop.max (props.Skill.Ranks |> Array.map (fun sr -> sr.Level) |> Array.max)
                            prop.onChange props.SetLevel

                            prop.value props.Rank
                            prop.style [ style.color "black"; style.flexShrink 0; style.width 32 ]
                        ]
                        Html.button [
                            prop.type' "button"
                            prop.className "shrink-0 px-2 font-bold bg-transparent"
                            prop.onClick (fun _me -> props.RemoveSkillCallBack props.Skill)
                            prop.children [ Html.text "X" ]
                        ]
                    ]
                ]
            ]
        ]