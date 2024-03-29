namespace Components

  module Armor = 
    open Feliz
    open Feliz.SelectSearch

    open DataTypes
    open ModelData

    [<ReactComponent>]
    let Component (armorType: ArmorType) decorations armor chosenSet updateChosenSet =
      let pieces = (armor |> List.filter (fun armor -> armor.Type = armorType))

      let findPieceFromId (id:string) =
        let matchingPieces = pieces |> List.filter (fun p -> p.Id |> sprintf "%i" = id)
        matchingPieces |> List.tryHead

      let calculateNewChosenSet chosenSet armorType armor : ChosenSet =
        match armor with
        | None -> chosenSet
        | Some armor when ChosenSet.getArmor armorType chosenSet = Some (armor, DecorationSlots.FromSlots armor.Slots) ->
          ChosenSet.updateArmor armorType None chosenSet
        | Some armor ->
          ChosenSet.updateArmor armorType (Some (armor, DecorationSlots.FromSlots armor.Slots)) chosenSet

      let chosenPiece = chosenSet |> ChosenSet.getArmor armorType

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
                      selectSearch.value (chosenPiece |> Option.map (fun (armor, decorations) -> armor.Id |> sprintf "%i") |> Option.defaultValue "")
                      selectSearch.placeholder "Select an Armor Piece"
                      selectSearch.search true
                      selectSearch.onChange (findPieceFromId >> (calculateNewChosenSet chosenSet armorType) >> updateChosenSet)
                      selectSearch.options [
                        SelectOption.Group {
                          name = "Low Rank"
                          items = [ for piece in pieces |> List.filter (fun armor -> armor.Rank = Rank.Low) -> { value = piece.Id |> sprintf "%i"; name = piece.Name; disabled = false } ]
                        }
                        SelectOption.Group {
                          name = "High Rank"
                          items = [ for piece in pieces |> List.filter (fun armor -> armor.Rank = Rank.High) -> { value = piece.Id |> sprintf "%i"; name = piece.Name; disabled = false } ]
                        }
                        SelectOption.Group {
                          name = "Master Rank"
                          items = [ for piece in pieces |> List.filter (fun armor -> armor.Rank = Rank.Master) -> { value = piece.Id |> sprintf "%i"; name = piece.Name; disabled = false } ]
                        }
                        ]
                    ]
                  ]
                  Html.div [
                    Html.text
                      ( (sprintf "Defense: %i " (chosenPiece |> Option.map (fun (armor, decorations) -> armor.Defense.Augmented) |> Option.defaultValue 0))
                        + (sprintf "F: %i " (chosenPiece |> Option.map (fun (armor, decorations) -> armor.Resistances.Fire) |> Option.defaultValue 0))
                        + (sprintf "W: %i " (chosenPiece |> Option.map (fun (armor, decorations) -> armor.Resistances.Water) |> Option.defaultValue 0))
                        + (sprintf "T: %i " (chosenPiece |> Option.map (fun (armor, decorations) -> armor.Resistances.Thunder) |> Option.defaultValue 0))
                        + (sprintf "I: %i " (chosenPiece |> Option.map (fun (armor, decorations) -> armor.Resistances.Ice) |> Option.defaultValue 0))
                        + (sprintf "D: %i " (chosenPiece |> Option.map (fun (armor, decorations) -> armor.Resistances.Dragon) |> Option.defaultValue 0))
                      )
                  ]
                ]
              ]
              Html.div [
                prop.className ""
                prop.children [

                ]
              ]
            ]
          ]
          match chosenPiece with
          | None -> Html.div [prop.children [Html.h3 "No Piece Selected"]]
          | Some selectedPiece -> ArmorDecoration.Component armorType decorations chosenSet updateChosenSet

        ]
      ]