namespace Components

  module Armor = 
    open Feliz
    open Feliz.SelectSearch

    open DataTypes
    open ModelData

    [<ReactComponent>]
    let Component decorations (armor: Armor seq) (chosenArmor: (Armor * DecorationSlots) option) (updateArmor: (Armor * DecorationSlots) option -> unit) =

      let findPieceFromId (id:string) : Armor option =
        let matchingPieces = armor |> Seq.filter (fun p -> p.Id |> sprintf "%i" = id)
        matchingPieces |> Seq.tryExactlyOne

      let updateArmorIfDifferent (armor:Armor option) =
        match armor with 
        | Some matchedArmor when armor = (chosenArmor |> Option.map fst) -> None
        | Some matchedArmor -> Some (matchedArmor, DecorationSlots.FromSlots matchedArmor.Slots)
        | None -> None

      let updateDecorationSlots decorationSlots = 
        updateArmor (chosenArmor |> Option.map (fun (armor, _) -> armor, decorationSlots) )

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
                      selectSearch.value (chosenArmor |> Option.map (fun (armor, decorations) -> armor.Id |> sprintf "%i") |> Option.defaultValue "")
                      selectSearch.placeholder "Select an Armor Piece"
                      selectSearch.search true
                      selectSearch.onChange (findPieceFromId >> updateArmorIfDifferent >> updateArmor)
                      selectSearch.options [
                        SelectOption.Group {
                          name = "Low Rank"
                          items = [ for a in armor |> Seq.filter (fun a -> a.Rank = Rank.Low) -> { value = a.Id |> sprintf "%i"; name = a.Name; disabled = false } ]
                        }
                        SelectOption.Group {
                          name = "High Rank"
                          items = [ for a in armor |> Seq.filter (fun a -> a.Rank = Rank.High) -> { value = a.Id |> sprintf "%i"; name = a.Name; disabled = false } ]
                        }
                        SelectOption.Group {
                          name = "Master Rank"
                          items = [ for a in armor |> Seq.filter (fun a -> a.Rank = Rank.Master) -> { value = a.Id |> sprintf "%i"; name = a.Name; disabled = false } ]
                        }
                        ]
                    ]
                  ]
                  Html.div [
                    Html.text
                      ( (sprintf "Defense: %s " (chosenArmor |> Option.map (fun (armor, decorations) -> armor.Defense.Augmented |> string) |> Option.defaultValue "-"))
                        + (sprintf "F: %s " (chosenArmor |> Option.map (fun (armor, decorations) -> armor.Resistances.Fire |> string) |> Option.defaultValue "-"))
                        + (sprintf "W: %s " (chosenArmor |> Option.map (fun (armor, decorations) -> armor.Resistances.Water |> string) |> Option.defaultValue "-"))
                        + (sprintf "T: %s " (chosenArmor |> Option.map (fun (armor, decorations) -> armor.Resistances.Thunder |> string) |> Option.defaultValue "-"))
                        + (sprintf "I: %s " (chosenArmor |> Option.map (fun (armor, decorations) -> armor.Resistances.Ice |> string) |> Option.defaultValue "-"))
                        + (sprintf "D: %s " (chosenArmor |> Option.map (fun (armor, decorations) -> armor.Resistances.Dragon |> string) |> Option.defaultValue "-"))
                      )
                  ]
                ]
              ]
            ]
          ]
          match chosenArmor with
          | None -> Html.div [prop.children [Html.h3 "No Piece Selected"]]
          | Some (armor, slots) -> DecorationSlots.Component decorations slots updateDecorationSlots

        ]
      ]