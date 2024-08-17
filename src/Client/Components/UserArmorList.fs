namespace Components

module UserArmorList =
    open Feliz
    open APIDataTypes
    open HelperFunctions


    [<ReactComponent>]
    let Component
        (props:
            {|
                Armor: (Armor * bool) list
                SetArmor: ((Armor * bool) list -> unit)
            |})
        =
        let searchWord, setSearchWord = React.useState ""

        let updateArmor armorToChange newStatus =
            props.Armor
            |> List.map (fun (armor, status) ->
                if armor = armorToChange then
                    armor, newStatus
                else
                    armor, status)
            |> props.SetArmor

        Html.div [
            prop.className [ "h-full" ]
            prop.style [ style.overflowY.scroll; style.paddingRight 14 ]
            prop.children (
                [
                    Html.input [
                        prop.className "text-black w-full"
                        prop.type' "text"
                        prop.onTextChange setSearchWord
                        prop.value searchWord
                        prop.placeholder "Armor/Skill name"
                    ]
                ]
                @ [
                    for (armor, doesUserHaveArmor) in
                        (props.Armor
                         |> List.filter (fun (armor, _) -> matchesByNameOrSkills searchWord armor)) ->
                        Html.div [
                            prop.className "user-armor-selector flex justify-between"
                            prop.children [
                                Html.label [ prop.className "user-armor-selector-label truncate"; prop.text armor.Name ]
                                Html.input [
                                    prop.id armor.Name
                                    prop.type' "checkbox"
                                    prop.onChange (updateArmor armor)
                                    prop.isChecked doesUserHaveArmor
                                    prop.style [ style.color "black"; style.flexShrink 0 ]
                                ]
                            ]
                        ]
                ]
            )
        ]