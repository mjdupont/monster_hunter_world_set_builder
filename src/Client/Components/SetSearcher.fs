namespace Components

module SetSearcher =
    open Feliz

    open DataTypes



    [<ReactComponent>]
    let Component
        (props:
            {|
                Skills: Skill seq
                SubmitSkills: (Skill * int) list -> unit
            |})
        =

        let (selectedSkills: (Skill * int) list), updateSelectedSkills = React.useState []

        let unselectedSkills =
            (props.Skills
             |> Seq.filter (fun skill ->
                 not (
                     selectedSkills
                     |> List.map (fst >> (fun sk -> sk.Name))
                     |> List.contains skill.Name
                 )))

        let addSkill (skill: (Skill * int) option) =
            updateSelectedSkills (selectedSkills |> List.append ([ skill ] |> List.choose id))

        let removeSkill (skill: Skill) =
            updateSelectedSkills (selectedSkills |> List.filter (fun (sk, r) -> not (sk.Name = skill.Name)))

        Html.div [
            prop.className "SetSearcher flex flex-col gap-1"
            prop.children [
                Html.div [
                    prop.className "selected-skills-list flex-item flex flex-col gap-1"
                    prop.children [
                        for x in selectedSkills ->
                            let (skill, rank) = x

                            SelectedSkill.Component {|
                                Skill = skill
                                Rank = rank
                                RemoveSkillCallBack = removeSkill
                            |}
                    ]
                ]
                SkillSelector.Component {|
                    Skills = unselectedSkills
                    AddSkill = addSkill
                |}
            ]
        ]