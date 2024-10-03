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
                GameData: MHWData
                ChosenSet: ChosenSet<'s, 'a, 'w, 'c, 'd, 'sb>
            |})
            : ReactElement when Skill<'s> and SetBonus<'sb>
        =

        let armorSetBonuses =
            props.ChosenSet |> ChosenSet.armorSetBonuses props.GameData.ArmorSets

        let totalSkills = (props.ChosenSet |> ChosenSet.allSkillRanks |> accumulateSkills)

        let totalSkillsElement = [
            for achievedSkill, achievedLevel in totalSkills do
                let skillFromData =
                    props.GameData.Skills
                    |> List.filter (fun skill -> skill.Id = achievedSkill.SkillId)
                    |> List.tryExactlyOne

                let skillColor =
                    match skillFromData with
                    | None -> "black"
                    | Some skillData ->
                        let maxLevel =
                            skillData.Ranks
                            |> List.sortByDescending (fun sr -> sr.Level)
                            |> List.head
                            |> (fun sr -> sr.Level)

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