namespace Components

module UserEquipmentSelector =
    open Feliz
    open ModelData
    open Components.UIElements
    open APIDataTypes


    [<ReactComponent>]
    let Component
        (props:
            {|
                UserData: UserData
                GameData: MHWData
                UpdateUserData: UserData -> unit
            |})
        =
        let decorations, setDecorations =
            props.UserData.Decorations,
            (fun newDecorations ->
                props.UpdateUserData {
                    props.UserData with
                        Decorations = newDecorations
                })

        let armor, setArmor =
            props.UserData.Armor, (fun newArmor -> props.UpdateUserData { props.UserData with Armor = newArmor })

        let charms, setCharms =
            props.UserData.Charms,
            (fun newCharms ->
                props.UpdateUserData {
                    props.UserData with
                        Charms = newCharms
                })

        let decorationsWithMax =
            decorations
            |> List.map (fun (decoration, count) ->
                (decoration, count), decoration |> maxSkillLevelOfDecoration props.GameData.Skills)

        Sidebar.Component "My Gear" {|
            Content =
                Accordion.Component {|
                    Tabs = [
                        {
                            Title = "Armor"
                            Content = UserArmorList.Component {| Armor = armor; SetArmor = setArmor |}
                        }
                        {
                            Title = "Charms"
                            Content =
                                UserCharmList.Component {|
                                    Charms = charms
                                    SetCharms = setCharms
                                |}
                        }
                        {
                            Title = "Decorations"
                            Content =
                                UserDecorationList.Component {|
                                    Decorations = decorationsWithMax
                                    SetDecorations = setDecorations
                                |}
                        }
                    ]
                |}
        |}