namespace Components

module UserCharmList =
    open Feliz
    open APIDataTypes
    open HelperFunctions


    [<ReactComponent>]
    let Component
        (props:
            {|
                Charms: (Charm * int) list
                SetCharms: ((Charm * int) list -> unit)
            |})
        =
        let searchWord, setSearchWord = React.useState ""

        let updateCharms charmToChange newMaxRank =
            props.Charms
            |> List.map (fun (charm, maxRank) ->
                if charm = charmToChange then
                    charm, newMaxRank
                else
                    charm, maxRank)
            |> props.SetCharms

        Html.div [
            prop.className [ "h-full" ]
            prop.style [ style.overflowY.scroll; style.paddingRight 14 ]
            prop.children (
                let filteredCharms =
                    props.Charms
                    |> List.filter (fun (charm, maxRank) ->
                        matchesByNameOrSkillsByPredicate
                            (fun (charm: Charm) -> charm.Name)
                            (fun charm ->
                                charm.Ranks
                                |> List.map (fun cr -> cr.Skills |> List.map (fun sr -> sr.SkillName))
                                |> List.concat)
                            searchWord
                            charm)

                [
                    Html.input [
                        prop.className "text-black w-full sticky top-0"
                        prop.type' "text"
                        prop.onTextChange setSearchWord
                        prop.value searchWord
                        prop.placeholder "Charm/Skill name"
                    ]
                ]
                @ [
                    for (charm, maxOwnedRank) in filteredCharms ->
                        Html.div [
                            prop.className "user-charm-selector flex justify-between"
                            prop.children [
                                Html.label [ prop.className "user-charm-selector-label truncate"; prop.text charm.Name ]
                                Html.input [
                                    prop.id charm.Name
                                    prop.type' "number"
                                    prop.min 0
                                    prop.max (charm.Ranks |> List.map (fun cr -> cr.Level) |> List.max)
                                    prop.onChange (updateCharms charm)
                                    prop.value maxOwnedRank
                                    prop.style [ style.color "black"; style.flexShrink 0; style.width 32 ]
                                ]
                            ]
                        ]
                ]
            )
        ]