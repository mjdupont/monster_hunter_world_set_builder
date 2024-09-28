namespace Components

module DecorationSlots =
    open Feliz

    open APIDataTypes
    open ModelData
    open HelperFunctions
    open SetSearchLogic.Interfaces

    [<ReactComponent>]
    let Component
        (props:
            {|
                Decorations: Decoration list
                ChosenDecoSlots: PropDrill<DecorationSlots<'d>>
            |})
        =

        let updateDecoration (newDecoration: 'd option when Decoration<'d>) (decorationSlot: DecorationSlot<'d>) : DecorationSlot<'d> =
            decorationSlot
            |> Option.map (fun (slot, oldDecoration) ->
                match oldDecoration, newDecoration with
                | Some oldDeco, Some newDeco when oldDeco = newDeco -> (slot, None) // If selecting the same decoration, clear the decoration slot
                | _, Some newDeco when newDeco.Slot <= slot -> (slot, newDecoration) // Only update to a new decoration if it can fit in the slot
                | _ -> (slot, oldDecoration))

        let updateDecorationSlot position (newDecoration: Decoration<'d> option) =
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