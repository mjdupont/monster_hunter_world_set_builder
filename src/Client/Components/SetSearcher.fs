namespace Components

module SetSearcher =
    open Feliz

    open APIDataTypes
    open ModelData



    [<ReactComponent>]
    let Component
        (props:
            {|
                Skills: Skill list
                RequestedSkills: RequestedSkills
                UpdateRequestedSkills: RequestedSkills -> unit
                SubmitSkills: (Skill * int) list -> unit
            |})
        =

        let (RequestedSkills requestedSkills) = props.RequestedSkills

        let unselectedSkills =
            let skillsInRequestedSkills = requestedSkills |> Map.keys |> List.ofSeq

            (props.Skills
             |> List.filter (fun skill ->
                 not (
                     skillsInRequestedSkills
                     |> List.map (fun sk -> sk.Name)
                     |> List.contains skill.Name
                 )))


        let addSkill (skill: (Skill * int) option) =
            match skill with
            | Some(skill, level) -> props.UpdateRequestedSkills(RequestedSkills(requestedSkills |> Map.add skill level))
            | None -> ()

        let removeSkill (skill: Skill) =
            props.UpdateRequestedSkills(RequestedSkills(requestedSkills |> Map.remove skill))

        let setSkillLevel (skill: Skill) newLevel =
            props.UpdateRequestedSkills(RequestedSkills(requestedSkills |> Map.add skill newLevel))

        let clearSkills () =
            props.UpdateRequestedSkills(RequestedSkills Map.empty)

        Html.div [
            prop.className "SetSearcher flex flex-col gap-1 p-1"
            prop.children [
                Html.div [
                    prop.className "selected-skills-list flex-item flex flex-col gap-1"
                    prop.children [
                        for KeyValue(skill, rank) in requestedSkills ->
                            SelectedSkill.Component {|
                                Skill = skill
                                Rank = rank
                                RemoveSkillCallBack = removeSkill
                                SetLevel = (setSkillLevel skill)
                            |}
                    ]
                ]
                SkillSelector.Component {|
                    Skills = unselectedSkills
                    AddSkill = addSkill
                |}
                Html.div [
                    prop.className "flex flex-row justify-evenly"
                    prop.children [
                        Html.button [
                            prop.onClick (fun _me -> props.SubmitSkills(requestedSkills |> Map.toList))
                            prop.children [ Html.text "Find Set" ]
                            prop.style [ style.margin.auto ]
                        ]

                        Html.button [
                            prop.type' "button"
                            prop.disabled (props.Skills |> List.isEmpty)
                            prop.onClick (fun _me -> clearSkills ())
                            prop.children [ Html.text "Clear All" ]
                            prop.style [ style.margin.auto ]
                        ]
                    ]
                ]

            ]
        ]