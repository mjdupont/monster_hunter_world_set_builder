namespace Components

module SkillSelector =
    open Feliz
    open Feliz.SelectSearch

    open APIDataTypes

    [<ReactComponent>]
    let Component
        (props:
            {|
                Skills: Skill list
                AddSkill: (Skill * int) option -> unit
            |})
        =
        let placeholder = "Select a Skill"

        let (skillAndRank: (Skill * int) option), updateSkill = React.useState (None)

        let findSkillFromId (id: string) =
            let matchingSkill =
                props.Skills
                |> List.filter (fun skill -> skill.Id |> sprintf "%i" = id)
                |> List.tryExactlyOne

            let matchingSkill =
                if matchingSkill = (skillAndRank |> Option.map fst) then
                    None
                else
                    matchingSkill

            matchingSkill
            |> Option.map (fun ms -> ms, ms.Ranks |> List.map (fun r -> r.Level) |> List.max)

        let addSkillAndClear skill =
            props.AddSkill skill
            updateSkill None


        Html.div [
            prop.className "flex flex-row gap-8 justify-center"
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
                    selectSearch.onChange (findSkillFromId >> addSkillAndClear)
                    selectSearch.options [
                        for skill in props.Skills ->
                            {
                                value = skill.Id |> sprintf "%i"
                                name = skill.Name
                                disabled = false
                            }
                    ]
                ]
            ]
        ]