namespace Components

  module Decoration = 
    open Feliz
    open Feliz.SelectSearch

    open DataTypes
    open ModelData
    
    [<ReactComponent>]
    let Component position (armorType: ArmorType) (slot:Slot) (decorations) (chosenSet) (updateChosenSet ) =
      let (Slot slot) = slot
      let decorations : Decoration list = decorations |> List.filter (fun decoration -> decoration.Slot <= slot)

      let findDecorationFromId (id:string) =
        let matchingDecoration = decorations |> List.filter (fun d -> d.Id |> sprintf "%i" = id)
        matchingDecoration |> List.tryHead

      let findDecorationFromDisplayValue (displayValue:string) =
        let matchingDecoration = decorations |> List.filter (fun d -> d.Name = displayValue)
        matchingDecoration |> List.tryHead

      let containsSkill (decoration:Decoration option) (searchQuery:string) =
        match decoration with
        | None -> false
        | Some decoration ->
          decoration.Skills
          |> Array.exists (fun skill -> skill.SkillName.ToLower().Replace(" ", "").Contains(searchQuery.ToLower().Replace(" ", "")))

      let containsSkillSimple (decoration:Decoration option) (searchQuery:string) =
        match decoration with
        | None -> false
        | Some decoration ->
          decoration.Skills
          |> Array.exists (fun skill -> skill.SkillName.StartsWith(searchQuery))


      let filterOptions (item:SelectItem) (searchQuery:string) =
        not item.disabled
        &&
        (item.name.Contains searchQuery
        || (containsSkillSimple (findDecorationFromId item.value) searchQuery)
        )

      let updateIfDifferent (newDecoration:Decoration option) (slot, decoration) =
        if newDecoration = decoration then (slot, None) else (slot, newDecoration)

      let setSelectedDecoration decoration =
        let updatedArmor =
          chosenSet
          |> ChosenSet.getArmor armorType
          |> Option.map
            (fun (armor, decorationSlots) ->
              (armor,
                match position with
                | First -> { decorationSlots with First = decorationSlots.First |> Option.map (updateIfDifferent decoration)}
                | Second -> { decorationSlots with Second = decorationSlots.Second |> Option.map (updateIfDifferent decoration)}
                | Third -> { decorationSlots with Third = decorationSlots.Third |> Option.map (updateIfDifferent decoration)}
                )
              )
        chosenSet |> ChosenSet.updateArmor armorType updatedArmor

      let slotAndDecoration =
        chosenSet
        |> ChosenSet.getArmor armorType
        |> Option.map snd
        |> Option.bind (fun decoslots -> decoslots.SlotFromPosition position)

      let imageUrlByValue (propertyValue:string) : string =
        let decoration = findDecorationFromId propertyValue
        decoration |> Option.map (fun x -> x.IconUri |> Option.defaultValue "") |> Option.defaultValue ""

      let imageUrlByDisplayValue (displayValue:string) : string =
        let decoration = findDecorationFromDisplayValue displayValue
        decoration |> Option.map (fun x -> x.IconUri |> Option.defaultValue "") |> Option.defaultValue ""

      let emptySlotImageUrl (slot:int) : string = 
        match slot with
        | i when i >= 1 && 4 >= i -> sprintf "images\\slot_size_%i.png" i
        //| i when i >= 1 && 4 >= i -> sprintf "images\\empty_decoration_level_%i.png" i
        | _ -> ""

      let placeholder = (sprintf "Select a Size %i decoration" slot)

      match slotAndDecoration with
      | None -> Html.text "Error, and fix this message you schmuck" //TODO: Fix this
      | Some (Slot slot, decoration) ->
        Html.div [
          prop.style [ style.width 300 ]
          prop.children [
            SelectSearch.selectSearch [
              selectSearch.search true
              selectSearch.autoComplete.on
              selectSearch.placeholder placeholder
              
              selectSearch.value ((decoration |> Option.map (fun decoration -> decoration.Id |> sprintf "%i")) |> Option.defaultValue "")
              selectSearch.onChange (findDecorationFromId >> setSelectedDecoration >> updateChosenSet)
              selectSearch.options
                [ for decoration in decorations ->
                  { value = decoration.Id |> sprintf "%i"; name = decoration.Name; disabled = false }
                ]
              
              selectSearch.filterOptions filterOptions
              
              selectSearch.renderValue (fun properties ->
                Html.button [
                  yield! properties.attributes
                  prop.className "select-search__input"
                  prop.children [
                    Html.div [
                      prop.className "flex flex-row"
                      prop.children [
                        Html.img [
                          prop.height 36
                          prop.width 36
                          prop.style [ style.marginRight 10; ]
                          prop.src (if properties.displayValue = "" then emptySlotImageUrl slot
                          else imageUrlByDisplayValue properties.displayValue)
                        ]
                        Html.span (if properties.displayValue = "" then placeholder else properties.displayValue)
                        ]
                    ]
                    
                  ]
                ]
              )

              selectSearch.renderOption (fun properties ->
                Html.button [
                  yield! properties.attributes
                  prop.className properties.className
                  prop.children [
                    Html.div [
                      prop.className "flex flex-row"
                      prop.children [
                        Html.img [
                          prop.height 36
                          prop.width 36
                          prop.style [ style.marginRight 10; ]
                          prop.src (imageUrlByValue properties.option.value)
                        ]
                        Html.span properties.option.name]
                    ]
                  ]
                ]
                )
            ]
          ]
        ]