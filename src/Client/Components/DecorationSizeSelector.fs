namespace Components

module DecorationSizeSelector =
    open Feliz

    open ModelData
    open DataTypes
    open HelperFunctions



    [<ReactComponent>]
    let Component (props: {|
        Position: int
        ChosenDecoSlot: PropDrill<DecorationSlot>
    |}) =
        let noDecorationElement () =
            Html.label [
                prop.children [
                    Html.input [
                        prop.type' "radio"
                        prop.value "Size0"
                        prop.name (sprintf "decosize_%i" props.Position)
                        prop.isChecked (props.ChosenDecoSlot.Value.IsNone)
                        prop.readOnly true
                    ]
                    Html.div [
                        prop.className "decotextwrap"
                        prop.style [ style.textAlign.center ]
                        prop.children [ Html.text "--" ]
                        prop.onClick (fun me -> None |> props.ChosenDecoSlot.Update)
                    ]
                ]
            ]

        let decorationElement size =
            let isChecked =
                match props.ChosenDecoSlot.Value with
                | Some((Slot slot), deco) when slot = size -> true
                | empty_decoration_level -> false

            Html.label [
                prop.children [
                    Html.input [
                        prop.type' "radio"
                        prop.value (sprintf "Size%i" size)
                        prop.name (sprintf "decosize_%i" props.Position)
                        prop.isChecked isChecked
                        prop.readOnly true
                    ]
                    Html.img [
                        prop.src (sprintf "images\\empty_decoration_level_%i.png" size)
                        prop.onClick (fun me -> (Some(Slot size, None)) |> props.ChosenDecoSlot.Update)
                    ]
                ]
            ]



        Html.div [
            prop.className "flex flex-col w-max"
            prop.children (List.append [ noDecorationElement () ] [ for size in 1..4 -> decorationElement size ])
        ]