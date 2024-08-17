namespace Components

module UserDecorationList =
    open Feliz
    open APIDataTypes
    open HelperFunctions


    [<ReactComponent>]
    let Component
        (props:
            {|
                Decorations: ((Decoration * int) * int) list
                SetDecorations: ((Decoration * int) list -> unit)
            |})
        =
        let searchWord, setSearchWord = React.useState ""

        let updateCount decoration newCount =
            props.Decorations
            |> List.map (fun ((pDecoration, pCount), pMax) ->
                if pDecoration = decoration then
                    pDecoration, newCount
                else
                    pDecoration, pCount)
            |> props.SetDecorations

        let colorFromCount max count = 
          if count = max then "gold"
          else if count < max && count > 0 then "white"
          else "gray"

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
                        prop.placeholder "Decoration/skill name"
                    ]
                ]
                @ [
                    for ((decoration, count), max) in
                        (props.Decorations
                         |> List.filter (fun ((decoration, count), max) -> matchesByNameOrSkills searchWord decoration)) ->
                        Html.div [
                            prop.style [style.color (colorFromCount max count) ]
                            prop.className "user-decoration-count-selector flex justify-between"
                            prop.children [
                                Html.label [
                                    prop.className "user-decoration-count-label truncate"
                                    prop.text decoration.Name
                                ]
                                Html.input [
                                    prop.id decoration.Name
                                    prop.type' "number"
                                    prop.min 0
                                    prop.max max
                                    prop.onChange (updateCount decoration)
                                    prop.value count
                                    prop.style [ style.color "black"; style.flexShrink 0; style.width 32 ]
                                ]
                            ]
                        ]
                ]
            )
        ]