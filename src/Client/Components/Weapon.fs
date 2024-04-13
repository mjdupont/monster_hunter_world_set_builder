namespace Components

  module Weapon = 
    open Feliz
    open Feliz.SelectSearch

    open DataTypes
    open ModelData

    [<ReactComponent>]
    let Component decorations weapons chosenWeapon updateWeapon =
      
      let findWeaponFromId (id:string) =
        let matchingPieces = weapons |> List.filter (fun w -> w.Id |> sprintf "%i" = id)
        matchingPieces |> List.tryHead

      let updateWeaponIfDifferent (weapon:Weapon option) =
        match weapon with 
        | Some matchedWeapon when weapon = (chosenWeapon |> Option.map fst) -> None
        | Some matchedWeapon -> Some (matchedWeapon, DecorationSlots.FromSlots matchedWeapon.Slots)
        | None -> None

      let updateDecorationSlots decorationSlots = 
        updateWeapon (chosenWeapon |> Option.map (fun (weapon, _) -> weapon, decorationSlots) )

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
                      selectSearch.value (chosenWeapon |> Option.map (fun (weapon, decorations) -> weapon.Id |> sprintf "%i") |> Option.defaultValue "")
                      selectSearch.placeholder "Select a Weapon"
                      selectSearch.search true
                      selectSearch.onChange (findWeaponFromId >> updateWeaponIfDifferent >> updateWeapon)
                      selectSearch.options [ for weapon in weapons -> { value = weapon.Id |> sprintf "%i"; name = weapon.Name; disabled = false } ]
                    ]
                  ]
                  Html.div [
                    Html.text
                      ( (sprintf "Attack: %i " (chosenWeapon |> Option.map (fun (weapon, decorations) -> weapon.Attack) |> Option.defaultValue 0))
                      )
                  ]
                ]
              ]
            ]
          ]
          match chosenWeapon with
          | None -> Html.div [prop.children [Html.h3 "No Weapon Selected"]]
          | Some (selectedWeapon, decorationSlots) -> DecorationSlots.Component decorations decorationSlots updateDecorationSlots

        ]
      ]