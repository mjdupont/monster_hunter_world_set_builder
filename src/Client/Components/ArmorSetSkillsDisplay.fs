namespace Components

module ArmorSetSkillsDisplay =
    open Feliz
    open ModelData
    open APIDataTypes
    open GameData.APIData
    open Interfaces


    [<ReactComponent>]
    let Component
        (props:
            {|
                SetBonuses: SetBonus<'sb, 'set, 's, 'sbr> list
                SkillData: SkillData<'sd, 's> list
                ChosenSet: ChosenSet<'s, 'a, 'w, 'c, 'd, 'set>
            |})
            : ReactElement when Skill<'s> and ArmorSet<'set>
        =

        let armorSetBonuses =
            props.ChosenSet |> ChosenSet.armorSetBonuses props.SetBonuses

        let totalSkills = (props.ChosenSet |> ChosenSet.allSkillRanks |> accumulateSkills)

        let totalSkillsElement = [
            for achievedSkill, achievedLevel in totalSkills do
                let skillFromData =
                    props.SkillData
                    |> List.filter (fun skillData -> skillData.Skill = achievedSkill)
                    |> List.tryExactlyOne

                let skillColor =
                    match skillFromData with
                    | None -> "black"
                    | Some skillData ->
                        let maxLevel =
                            skillData.MaxRank

                        match achievedLevel with
                        | s when s = maxLevel -> "green"
                        | s when s > maxLevel -> "red"
                        | _ -> "black"

                yield
                    Html.div [
                        prop.className ""
                        prop.children [
                            Html.h3 [
                                prop.style [ style.color skillColor ]
                                prop.text (sprintf "%s: %i" achievedSkill.Name achievedLevel)
                            ]
                        ]
                    ]
        ]

        let armorSetSkillsElement = [
            for setBonus, level in armorSetBonuses ->
                Html.div [
                    prop.className ""
                    prop.children [
                        Html.h2 [
                            prop.style [ style.color "black" ]
                            prop.text (sprintf "%s - %i" setBonus.Name level)
                        ]
                    ]
                ]
        ]

        Html.div [
            prop.className "armor-summary m-auto bg-white/80 rounded-md shadow-md p-4"
            prop.children ([ armorSetSkillsElement; totalSkillsElement ] |> List.concat)
        ]