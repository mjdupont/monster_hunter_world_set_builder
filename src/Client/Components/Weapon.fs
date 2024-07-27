namespace Components

module Weapon =
    open Feliz
    open Feliz.SelectSearch

    open GameDataTypes
    open ModelData
    open HelperFunctions



    [<ReactComponent>]
    let Component
        (props:
            {|
                Decorations: Decoration seq
                Weapons: Weapon seq
                ChosenWeapon: PropDrill<(Weapon * DecorationSlots) option>
            |})
        =

        let findWeaponFromId (id: string) =
            let matchingPieces =
                props.Weapons |> Seq.filter (fun w -> w.Id |> sprintf "%i" = id)

            matchingPieces |> Seq.tryHead

        let updateWeaponIfDifferent (weapon: Weapon option) =
            match weapon with
            | Some matchedWeapon when weapon = (props.ChosenWeapon.Value |> Option.map fst) -> None
            | Some matchedWeapon -> Some(matchedWeapon, DecorationSlots.FromSlots matchedWeapon.Slots)
            | None -> None

        let updateDecorationSlots decorationSlots =
            props.ChosenWeapon.Update(
                props.ChosenWeapon.Value
                |> Option.map (fun (weapon, _) -> weapon, decorationSlots)
            )

        Html.div [
            prop.className "armor-item flex flex-row gap-8 bg-white/80 rounded-md shadow-md p-4 w-full"
            prop.children [
                Html.div [
                    prop.className "flex flex-row"
                    prop.children [
                        Html.div [
                            prop.className "flex flex-col w-max"
                            prop.children [
                                Html.div [
                                    SelectSearch.selectSearch [
                                        selectSearch.value (
                                            props.ChosenWeapon.Value
                                            |> Option.map (fun (weapon, decorations) -> weapon.Id |> sprintf "%i")
                                            |> Option.defaultValue ""
                                        )
                                        selectSearch.placeholder "Select a Weapon"
                                        selectSearch.search true
                                        selectSearch.onChange (
                                            findWeaponFromId >> updateWeaponIfDifferent >> props.ChosenWeapon.Update
                                        )
                                        selectSearch.options [
                                            for weapon in props.Weapons ->
                                                {
                                                    value = weapon.Id |> sprintf "%i"
                                                    name = weapon.Name
                                                    disabled = false
                                                }
                                        ]
                                    ]
                                ]
                                Html.div [
                                    Html.text (
                                        (sprintf
                                            "Attack: %i "
                                            (props.ChosenWeapon.Value
                                             |> Option.map (fun (weapon, decorations) -> weapon.Attack)
                                             |> Option.defaultValue 0))
                                    )
                                ]
                            ]
                        ]
                    ]
                ]
                match props.ChosenWeapon.Value with
                | None -> Html.div [ prop.children [ Html.h3 "No Weapon Selected" ] ]
                | Some(selectedWeapon, decorationSlots) ->
                    DecorationSlots.Component {|
                        Decorations = props.Decorations
                        ChosenDecoSlots = {
                            Value = decorationSlots
                            Update = updateDecorationSlots
                        }
                    |}

            ]
        ]