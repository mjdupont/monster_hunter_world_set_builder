 namespace Components

  module DecorationSlots = 
    open Feliz
    open Feliz.SelectSearch

    open DataTypes
    open ModelData 
  
    [<ReactComponent>]
    let Component (decorations:Decoration seq) (decorationSlots:DecorationSlots) (updateChosenSet:DecorationSlots -> unit) =
      
      let updateDecoration (newDecoration:Decoration option) (decorationSlot:DecorationSlot) : DecorationSlot = 
        decorationSlot 
        |> Option.map (fun (Slot slot, oldDecoration) ->
          match oldDecoration, newDecoration with 
          | Some oldDeco, Some newDeco when oldDeco = newDeco -> ((Slot slot), None) // If selecting the same decoration, clear the decoration slot
          | _, Some newDeco when newDeco.Slot <= slot -> ((Slot slot), newDecoration) // Only update to a new decoration if it can fit in the slot
          | _ -> (Slot slot, oldDecoration)
        )

      let updateDecorationSlot position (newDecoration:Decoration option) =
        let updatedDecorationSlots =
          match position with 
          | First -> { decorationSlots with First = (decorationSlots.First |> updateDecoration newDecoration) }
          | Second -> { decorationSlots with Second = (decorationSlots.Second |> updateDecoration newDecoration) }
          | Third -> { decorationSlots with Third = (decorationSlots.Third |> updateDecoration newDecoration) }
        updatedDecorationSlots |> updateChosenSet
     
      Html.div [
        prop.className "flex flex-col"
        prop.children [
          for position, decorationSlot in [(First, decorationSlots.First); (Second, decorationSlots.Second); (Third, decorationSlots.Third)]->
          match decorationSlot with
          | None -> Html.div [ Html.h3 "-----"]
          | Some decoSlot -> Decoration.Component decorations decoSlot (updateDecorationSlot position)
        ]
      ]