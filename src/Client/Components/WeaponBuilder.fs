namespace Components

  module WeaponBuilder = 
    open Feliz
    open Feliz.SelectSearch

    open DataTypes
    open ModelData

    [<ReactComponent>]
    let Component decorations (chosenWeapon:(Weapon * DecorationSlots) option) (updateWeapon: (Weapon * DecorationSlots) option -> unit) =
      let updateDecorationSlots decorationSlots = 
        updateWeapon (chosenWeapon |> Option.map (fun (weapon, _) -> weapon, decorationSlots) )
      
      Html.div [
        prop.className "weapon-builder flex flex-row gap-8 bg-white/80 rounded-md shadow-md p-4 w-full"
        prop.children [
          match chosenWeapon with
          | None ->  Html.text "No Weapon Selected"
          | Some (weapon, slots) ->
            Html.div [
              prop.className "flex flex-row"
              prop.children [
                DecorationSizeSelector.ComponentX 1 slots.First (fun (decoSlot:DecorationSlot) -> Some (weapon, {slots with First = decoSlot}) |> updateWeapon)
                DecorationSizeSelector.ComponentX 2 slots.Second (fun (decoSlot:DecorationSlot) -> Some (weapon, {slots with Second = decoSlot}) |> updateWeapon)
                DecorationSizeSelector.ComponentX 3 slots.Third (fun (decoSlot:DecorationSlot) -> Some (weapon, {slots with Third = decoSlot}) |> updateWeapon)
              ]
            ]
            DecorationSlots.Component decorations slots updateDecorationSlots
        ]
      ]