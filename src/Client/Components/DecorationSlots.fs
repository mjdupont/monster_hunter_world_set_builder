namespace Components

module DecorationSlots =
    open Feliz

    open APIDataTypes
    open ModelData
    open HelperFunctions


    [<ReactComponent>]
    let Component
        (props:
            {|
                Decorations: Decoration seq
                ChosenDecoSlots: PropDrill<DecorationSlots>
            |})
        =

        let updateDecoration (newDecoration: Decoration option) (decorationSlot: DecorationSlot) : DecorationSlot =
            decorationSlot
            |> Option.map (fun (Slot slot, oldDecoration) ->
                match oldDecoration, newDecoration with
                | Some oldDeco, Some newDeco when oldDeco = newDeco -> ((Slot slot), None) // If selecting the same decoration, clear the decoration slot
                | _, Some newDeco when newDeco.Slot <= slot -> ((Slot slot), newDecoration) // Only update to a new decoration if it can fit in the slot
                | _ -> (Slot slot, oldDecoration))

        let updateDecorationSlot position (newDecoration: Decoration option) =
            let updatedDecorationSlots =
                match position with
                | First -> {
                    props.ChosenDecoSlots.Value with
                        First = (props.ChosenDecoSlots.Value.First |> updateDecoration newDecoration)
                  }
                | Second -> {
                    props.ChosenDecoSlots.Value with
                        Second = (props.ChosenDecoSlots.Value.Second |> updateDecoration newDecoration)
                  }
                | Third -> {
                    props.ChosenDecoSlots.Value with
                        Third = (props.ChosenDecoSlots.Value.Third |> updateDecoration newDecoration)
                  }

            updatedDecorationSlots |> props.ChosenDecoSlots.Update

        Html.div [
            prop.className "flex flex-col"
            prop.children [
                for position, decorationSlot in
                    [
                        (First, props.ChosenDecoSlots.Value.First)
                        (Second, props.ChosenDecoSlots.Value.Second)
                        (Third, props.ChosenDecoSlots.Value.Third)
                    ] ->
                    match decorationSlot with
                    | None -> Html.div [ Html.h3 "-----" ]
                    | Some(slot, decoration) ->
                        Decoration.Component {|
                            Decorations = props.Decorations
                            Slot = slot
                            ChosenDecoration = {
                                Value = decoration
                                Update = (updateDecorationSlot position)
                            }
                        |}
            ]
        ]