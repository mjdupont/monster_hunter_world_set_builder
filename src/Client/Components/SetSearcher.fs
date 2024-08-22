namespace Components

module SetSearcher =
    open Feliz

    open APIDataTypes
    open ModelData



    [<ReactComponent>]
    let Component
        (props:
            {|
                Skills: Skill seq
                SkillList: SkillList
                UpdateSkillList: SkillList -> unit
                SubmitSkills: (Skill * int) list -> unit
            |})
        =

        let (SkillList skillList) = props.SkillList

        let unselectedSkills =
            (props.Skills
             |> Seq.filter (fun skill ->
                 not (skillList |> List.map (fst >> (fun sk -> sk.Name)) |> List.contains skill.Name)))

        let addSkill (skill: (Skill * int) option) =
            props.UpdateSkillList(SkillList(skillList |> List.append ([ skill ] |> List.choose id)))

        let removeSkill (skill: Skill) =
            props.UpdateSkillList(SkillList(skillList |> List.filter (fun (sk, r) -> not (sk.Name = skill.Name))))

        let setSkillLevel (skill : Skill) newLevel = 
            props.UpdateSkillList(SkillList(skillList |> List.map (fun (sk, l) -> if (sk.Name = skill.Name) then (sk, newLevel) else (sk, l))))

        let clearSkills () = props.UpdateSkillList(SkillList [])

        Html.div [
            prop.className "SetSearcher flex flex-col gap-1 p-1"
            prop.children [
                Html.div [
                    prop.className "selected-skills-list flex-item flex flex-col gap-1"
                    prop.children [
                        for (skill, rank) in skillList ->
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
                        prop.onClick (fun _me -> props.SubmitSkills skillList)
                        prop.children [ Html.text "Find Set" ]
                        prop.style [ style.margin.auto ]
                    ]

                    Html.button [
                        prop.type' "button"
                        prop.disabled (props.Skills |> Seq.isEmpty)
                        prop.onClick (fun _me -> clearSkills ())
                        prop.children [ Html.text "Clear All" ]
                        prop.style [ style.margin.auto ]
                    ]
                  ]
                ]
                
            ]
        ]