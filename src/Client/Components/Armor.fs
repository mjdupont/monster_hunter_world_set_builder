namespace Components

  module Armor = 
    open Feliz
    open Feliz.SelectSearch

    open DataTypes
    open ModelData
    open HelperFunctions

    [<RequireQualifiedAccess>]
    type Properties =
      { Decorations : Decoration seq
        Armor : Armor seq
        ChosenArmor: PropDrill<(Armor * DecorationSlots) option>
      }

    [<ReactComponent>]
    let Component (props:Properties) =

      let findPieceFromId (id:string) : Armor option =
        let matchingPieces = props.Armor |> Seq.filter (fun p -> p.Id |> sprintf "%i" = id)
        matchingPieces |> Seq.tryExactlyOne

      let updateArmorIfDifferent (armor:Armor option) =
        match armor with 
        | Some matchedArmor when armor = (props.ChosenArmor.Value |> Option.map fst) -> None
        | Some matchedArmor -> Some (matchedArmor, DecorationSlots.FromSlots matchedArmor.Slots)
        | None -> None

      let updateDecorationSlots decorationSlots = 
        props.ChosenArmor.Update (props.ChosenArmor.Value |> Option.map (fun (armor, _) -> armor, decorationSlots) )

      Html.div [
        prop.className "armor-selector flex flex-row gap-8 p-4 w-full"
        prop.children [
          Html.div [
            prop.className "flex flex-col w-max"
            prop.children [
              SelectSearch.selectSearch [
                selectSearch.value (props.ChosenArmor.Value |> Option.map (fun (armor, decorations) -> armor.Id |> sprintf "%i") |> Option.defaultValue "")
                selectSearch.placeholder "Select an Armor Piece"
                selectSearch.search true
                selectSearch.onChange (findPieceFromId >> updateArmorIfDifferent >> props.ChosenArmor.Update)
                selectSearch.options [
                  SelectOption.Group {
                    name = "Low Rank"
                    items = [ for a in props.Armor |> Seq.filter (fun a -> a.Rank = Rank.Low) -> { value = a.Id |> sprintf "%i"; name = a.Name; disabled = false } ]
                  }
                  SelectOption.Group {
                    name = "High Rank"
                    items = [ for a in props.Armor |> Seq.filter (fun a -> a.Rank = Rank.High) -> { value = a.Id |> sprintf "%i"; name = a.Name; disabled = false } ]
                  }
                  SelectOption.Group {
                    name = "Master Rank"
                    items = [ for a in props.Armor |> Seq.filter (fun a -> a.Rank = Rank.Master) -> { value = a.Id |> sprintf "%i"; name = a.Name; disabled = false } ]
                  }
                  ]
              ]
              Html.div [
                Html.text
                  ( (sprintf "Defense: %s " (props.ChosenArmor.Value |> Option.map (fun (armor, decorations) -> armor.Defense.Augmented |> string) |> Option.defaultValue "-"))
                    + (sprintf "F: %s " (props.ChosenArmor.Value |> Option.map (fun (armor, decorations) -> armor.Resistances.Fire |> string) |> Option.defaultValue "-"))
                    + (sprintf "W: %s " (props.ChosenArmor.Value |> Option.map (fun (armor, decorations) -> armor.Resistances.Water |> string) |> Option.defaultValue "-"))
                    + (sprintf "T: %s " (props.ChosenArmor.Value |> Option.map (fun (armor, decorations) -> armor.Resistances.Thunder |> string) |> Option.defaultValue "-"))
                    + (sprintf "I: %s " (props.ChosenArmor.Value |> Option.map (fun (armor, decorations) -> armor.Resistances.Ice |> string) |> Option.defaultValue "-"))
                    + (sprintf "D: %s " (props.ChosenArmor.Value |> Option.map (fun (armor, decorations) -> armor.Resistances.Dragon |> string) |> Option.defaultValue "-"))
                  )
              ]
            ]
          ]
          match props.ChosenArmor.Value with
          | None -> Html.h3 "No Piece Selected"
          | Some (armor, slots) -> DecorationSlots.Component { Decorations = props.Decorations; ChosenDecoSlots = { Value = slots; Update = updateDecorationSlots } }

        ]
      ]