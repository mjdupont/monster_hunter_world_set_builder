 namespace Components

  module ArmorDecoration = 
    open Feliz
    open Feliz.SelectSearch

    open DataTypes
    open ModelData 
  
    [<ReactComponent>]
    let Component (armorType:ArmorType) decorations chosenSet (updateChosenSet) =
      let chosenPiece = chosenSet |> ChosenSet.getArmor armorType
      match chosenPiece with
      | None -> Html.text (sprintf "Error finding selected armor for %A!" armorType)
      | Some (chosenPiece, decorationSlots) ->
        Html.div [
          prop.className "flex flex-col"
          prop.children [
            for position, slot in [(First, decorationSlots.First); (Second, decorationSlots.Second); (Third, decorationSlots.Third)]->
            match slot with
            | None -> Html.div [ Html.h3 "-----"]
            | Some (slot, decoration) -> Decoration.Component position armorType slot decorations chosenSet updateChosenSet
          ]
        ]