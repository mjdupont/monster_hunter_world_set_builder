namespace Components

module WeaponBuilder =
    open Feliz

    open DataTypes
    open ModelData
    open HelperFunctions

    [<ReactComponent>]
    let Component
        (props:
            {|
                Decorations: Decoration seq
                ChosenWeapon: PropDrill<(Weapon * DecorationSlots) option>
            |})
        =
        let updateDecorationSlots decorationSlots =
            props.ChosenWeapon.Update(
                props.ChosenWeapon.Value
                |> Option.map (fun (weapon, _) -> weapon, decorationSlots)
            )


        Html.div [
            prop.className "weapon-builder flex flex-row gap-8 p-4 w-full"
            prop.children [
                match props.ChosenWeapon.Value with
                | None -> Html.text "No Weapon Selected"
                | Some(weapon, slots) ->
                    Html.div [
                        prop.className "flex flex-row"
                        prop.children [
                            DecorationSizeSelector.Component {|
                                Position = 1
                                ChosenDecoSlot = {
                                    Value = slots.First
                                    Update =
                                        (fun (decoSlot: DecorationSlot) ->
                                            Some(weapon, { slots with First = decoSlot }) |> props.ChosenWeapon.Update)
                                }
                            |}
                            DecorationSizeSelector.Component {|
                                Position = 2
                                ChosenDecoSlot = {
                                    Value = slots.Second
                                    Update =
                                        (fun (decoSlot: DecorationSlot) ->
                                            Some(weapon, { slots with Second = decoSlot }) |> props.ChosenWeapon.Update)
                                }
                            |}
                            DecorationSizeSelector.Component {|
                                Position = 3
                                ChosenDecoSlot = {
                                    Value = slots.Third
                                    Update =
                                        (fun (decoSlot: DecorationSlot) ->
                                            Some(weapon, { slots with Third = decoSlot }) |> props.ChosenWeapon.Update)
                                }
                            |}
                        ]
                    ]

                    DecorationSlots.Component {|
                        Decorations = props.Decorations
                        ChosenDecoSlots = {
                            Value = slots
                            Update = updateDecorationSlots
                        }
                    |}
            ]
        ]