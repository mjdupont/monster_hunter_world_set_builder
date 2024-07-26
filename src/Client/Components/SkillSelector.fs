namespace Components

module SkillSelector =
    open Feliz
    open Feliz.SelectSearch

    open DataTypes

    [<RequireQualifiedAccess>]
    type Properties = {
        Skills: Skill seq
        AddSkill: (Skill * int) option -> unit
    }

    [<ReactComponent>]
    let Component (props: Properties) =
        let placeholder = "Select a Skill"

        let (skillAndRank: (Skill * int) option), updateSkill = React.useState (None)

        let findSkillFromId (id: string) =
            let matchingSkill =
                props.Skills
                |> Seq.filter (fun skill -> skill.Id |> sprintf "%i" = id)
                |> Seq.tryExactlyOne

            let matchingSkill =
                if matchingSkill = (skillAndRank |> Option.map fst) then
                    None
                else
                    matchingSkill

            matchingSkill
            |> Option.map (fun ms -> ms, ms.Ranks |> Seq.map (fun r -> r.Level) |> Seq.max)

        let addSkillAndClear skill =
            props.AddSkill skill
            updateSkill None


        Html.div [
            prop.className "flex flex-row gap-8"
            prop.children [
                SelectSearch.selectSearch [
                    selectSearch.search true
                    selectSearch.autoComplete.on
                    selectSearch.placeholder placeholder


                    selectSearch.value (
                        (skillAndRank
                         |> Option.map fst
                         |> Option.map (fun skill -> skill.Id |> sprintf "%i"))
                        |> Option.defaultValue ""
                    )
                    selectSearch.onChange (findSkillFromId >> updateSkill)
                    selectSearch.options [
                        for skill in props.Skills ->
                            {
                                value = skill.Id |> sprintf "%i"
                                name = skill.Name
                                disabled = false
                            }
                    ]
                ]
                match skillAndRank with
                | Some(skill, rank) ->
                    Html.div [
                        prop.style [ style.width 70; style.fontSize 8 ]
                        prop.children [
                            SelectSearch.selectSearch [
                                selectSearch.search true
                                selectSearch.value (rank |> sprintf "%i")
                                selectSearch.options [
                                    for skillRank in skill.Ranks ->
                                        {
                                            value = skillRank.Level |> sprintf "%i"
                                            name = skillRank.Level |> sprintf "%i"
                                            disabled = false
                                        }
                                ]
                                selectSearch.onChange (fun id -> (updateSkill (Some(skill, (int) id))))
                            ]
                        ]
                    ]
                | None -> ()
                Html.button [
                    prop.type' "button"
                    prop.disabled skillAndRank.IsNone
                    prop.onClick (fun _me -> addSkillAndClear skillAndRank)
                    prop.children [ Html.text "Add Skill" ]
                ]
            ]
        ]