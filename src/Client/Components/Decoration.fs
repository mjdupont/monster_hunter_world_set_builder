namespace Components

module Decoration =
    open Feliz
    open Feliz.SelectSearch

    open APIDataTypes
    open HelperFunctions

    [<ReactComponent>]
    let Component
        (props:
            {|
                Decorations: Decoration seq
                Slot: Slot
                ChosenDecoration: PropDrill<Decoration option>
            |})
        =
        let (Slot slot) = props.Slot

        let decorations: Decoration list =
            props.Decorations
            |> List.ofSeq
            |> List.filter (fun decoration -> decoration.Slot <= slot)

        let findDecorationFromId (id: string) =
            let matchingDecoration =
                decorations |> List.filter (fun d -> d.Id |> sprintf "%i" = id)

            matchingDecoration |> List.tryHead

        let findDecorationFromDisplayValue (displayValue: string) =
            let matchingDecoration = decorations |> List.filter (fun d -> d.Name = displayValue)
            matchingDecoration |> List.tryHead

        let filterOptions (item: SelectItem) (searchQuery: string) =
            not item.disabled
            && ((item.name.ToLowerInvariant()).Contains(searchQuery.ToLowerInvariant())
                || (
                    (findDecorationFromId item.value) 
                    |> Option.map (matchesByNameOrSkills searchQuery)
                    |> Option.defaultValue false
                  )
                )

        let imageUrlByValue (propertyValue: string) : string =
            let decoration = findDecorationFromId propertyValue

            decoration
            |> Option.map (fun x -> x.IconUri |> Option.defaultValue "")
            |> Option.defaultValue ""

        let imageUrlByDisplayValue (displayValue: string) : string =
            let decoration = findDecorationFromDisplayValue displayValue

            decoration
            |> Option.map (fun x -> x.IconUri |> Option.defaultValue "")
            |> Option.defaultValue ""

        let emptySlotImageUrl (slot: int) : string =
            match slot with
            | i when i >= 1 && 4 >= i -> sprintf "images\\slot_size_%i.png" i
            //| i when i >= 1 && 4 >= i -> sprintf "images\\empty_decoration_level_%i.png" i
            | _ -> ""

        let placeholder = (sprintf "Select a Size %i decoration" slot)

        Html.div [
            prop.style [ style.width 285; style.fontSize 8 ]
            prop.children [
                SelectSearch.selectSearch [
                    selectSearch.search true
                    selectSearch.autoComplete.on
                    selectSearch.placeholder placeholder

                    selectSearch.value (
                        (props.ChosenDecoration.Value
                         |> Option.map (fun decoration -> decoration.Id |> sprintf "%i"))
                        |> Option.defaultValue ""
                    )
                    selectSearch.onChange (findDecorationFromId >> props.ChosenDecoration.Update)
                    selectSearch.options [
                        for decoration in decorations ->
                            {
                                value = decoration.Id |> sprintf "%i"
                                name = decoration.Name
                                disabled = false
                            }
                    ]

                    selectSearch.filterOptions filterOptions

                    selectSearch.renderOption (fun properties ->
                        Html.button [
                            yield! properties.attributes
                            prop.className properties.className
                            prop.children [
                                Html.div [
                                    prop.className "flex flex-row"
                                    prop.children [
                                        Html.img [
                                            prop.height 36
                                            prop.width 36
                                            prop.style [ style.marginRight 10 ]
                                            prop.src (imageUrlByValue properties.option.value)
                                        ]
                                        Html.span properties.option.name
                                    ]
                                ]
                            ]
                        ])
                ]
            ]
        ]