module Index

open Elmish
open Fable.Remoting.Client
open Shared

open Feliz
open Feliz.Bulma
open Feliz.SelectSearch
open DataTypes
open ModelData
open Components
open HelperFunctions

type Model =
  { Armor: Deferred<Armor list, string>
    Decorations: Deferred<Decoration list, string>
    Skills: Deferred<Skill list, string>
    Charms: Deferred<Charm list, string>
    Input: string
    ChosenSet: ChosenSet
  }

type LoadedModel = 
  { Armor: Armor list
    Decorations: Decoration list
    Skills: Skill list
    Charms: Charm list
    Input: string
    ChosenSet: ChosenSet
  }

let modelLoadedStatus (model:Model) : Deferred<LoadedModel,string> =
  match model.Decorations, model.Armor, model.Charms, model.Skills with
  | Deferred.Success decorations, Deferred.Success armor, Deferred.Success charms, Deferred.Success skills -> Deferred.Success { Armor = armor; Decorations = decorations; Skills = skills; Charms = charms; Input = model.Input; ChosenSet = model.ChosenSet }
  | Deferred.Failure f, _, _, _  -> Deferred.Failure f
  | _, Deferred.Failure f, _, _ -> Deferred.Failure f
  | _, _, Deferred.Failure f, _ -> Deferred.Failure f
  | _, _, _, Deferred.Failure f -> Deferred.Failure f
  | Deferred.InProgress, _, _, _ -> Deferred.InProgress
  | _, Deferred.InProgress, _, _ -> Deferred.InProgress
  | _, _, Deferred.InProgress, _ -> Deferred.InProgress
  | _, _, _, Deferred.InProgress -> Deferred.InProgress
  | Deferred.NotAsked, _, _, _ -> Deferred.NotAsked
  | _, Deferred.NotAsked, _, _ -> Deferred.NotAsked
  | _, _, Deferred.NotAsked, _ -> Deferred.NotAsked
  | _, _, _, Deferred.NotAsked -> Deferred.NotAsked


type Msg =
    | GotArmor of Armor list
    | GotDecorations of Decoration list
    | GotSkills of Skill list
    | GotCharms of Charm list
    | UpdateChosenSet of ChosenSet
    | SetInput of string

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IMHWApi>

let init () : (Model * Cmd<Msg>)=
    let (model:Model) = { Input = "" ; Armor = Deferred.NotAsked; Decorations = Deferred.NotAsked; Skills = Deferred.NotAsked; Charms = Deferred.NotAsked; ChosenSet = ChosenSet.Default }
    let cmd =
      Cmd.batch [
        Cmd.OfAsync.perform todosApi.getArmor () GotArmor
        Cmd.OfAsync.perform todosApi.getDecorations () GotDecorations
        Cmd.OfAsync.perform todosApi.getSkills () GotSkills
        Cmd.OfAsync.perform todosApi.getCharms () GotCharms
      ]


    model, cmd

let update msg (model:Model) =
    match msg with
    | GotArmor armor -> { model with Armor = Deferred.Success armor }, Cmd.none
    | GotDecorations decorations -> { model with Decorations = Deferred.Success decorations}, Cmd.none
    | GotSkills skills -> { model with Skills = Deferred.Success skills}, Cmd.none
    | GotCharms charms -> { model with Charms = Deferred.Success charms}, Cmd.none
    | UpdateChosenSet set -> 
      do ChosenSet.storeToWebStorage set
      { model with ChosenSet = set}, Cmd.none
    | SetInput value -> { model with Input = value }, Cmd.none

let skillsFromDecorationSlot (decorationSlot:DecorationSlot) =
  decorationSlot
  |> Option.bind snd
  |> Option.map (fun deco -> deco.Skills)
  |> Option.defaultValue [||]

let skillsfromDecorationSlots (decorationSlots:DecorationSlots) =
  [|decorationSlots.First |> skillsFromDecorationSlot
  ; decorationSlots.Second |> skillsFromDecorationSlot
  ; decorationSlots.Third |> skillsFromDecorationSlot
  |]
  |> Array.concat

let skillsFromArmor ((armor:Armor), decorationSlots) =
  [| decorationSlots |> skillsfromDecorationSlots; armor.Skills |]
  |> Array.concat


let totalSkills (chosenSet:ChosenSet) =
  let skillsFromArmor = 
    [|chosenSet.Headgear
    ; chosenSet.Chest
    ; chosenSet.Gloves
    ; chosenSet.Waist
    ; chosenSet.Legs
    |] |> Array.choose (Option.map skillsFromArmor)
    |> Array.concat
  let skillsFromCharm =
    chosenSet.Charm 
    |> Option.map (fun (charm, charmRank) -> charmRank.Skills)
    |> Option.defaultValue [||]

  [ skillsFromArmor; skillsFromCharm ] |> Array.concat

let accumulateSkills (skills:SkillRank array) =
  skills
  |> Array.groupBy (fun sr -> sr.Skill)
  |> Array.map (fun (skill, items) ->
    items
    |> Array.reduce (fun skillRankState newSkillRank ->
      { skillRankState with Level = skillRankState.Level + newSkillRank.Level }))



module Components =

  [<ReactComponent>]
  let armorDecorationWidget (armorType:ArmorType) decorations chosenSet (dispatch) =
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
          | Some (slot, decoration) -> Decoration.Component position armorType slot decorations chosenSet (UpdateChosenSet >> dispatch)
        ]
      ]




  [<ReactComponent>]
  let armorItem (armorType: ArmorType) decorations armor chosenSet dispatch =
    let pieces = (armor |> List.filter (fun armor -> armor.Type = armorType))

    let findPieceFromId (id:string) =
      let matchingPieces = pieces |> List.filter (fun p -> p.Id |> sprintf "%i" = id)
      matchingPieces |> List.tryHead

    let updateChosenSet chosenSet armorType armor : ChosenSet =
      match armor with
      | None -> chosenSet
      | Some armor ->
        ChosenSet.updateArmor armorType (Some (armor, DecorationSlots.FromSlots armor.Slots)) chosenSet

    let chosenPiece = chosenSet |> ChosenSet.getArmor armorType

    Html.div [
      prop.className "armor-item flex flex-row bg-white/80 rounded-md shadow-md p-4 w-5/6 lg:w-3/4 lg:max-w-2xl"
      prop.children [
        Html.div [
          prop.className "flex flex-row"
          prop.children [
            Html.div [
              prop.className "flex flex-col"
              prop.children [
                Html.div [
                  SelectSearch.selectSearch [
                    selectSearch.value (chosenPiece |> Option.map (fun (armor, decorations) -> armor.Id |> sprintf "%i") |> Option.defaultValue "")
                    selectSearch.placeholder "Select an Armor Piece"
                    selectSearch.search true
                    selectSearch.onChange (findPieceFromId >> (updateChosenSet chosenSet armorType) >> UpdateChosenSet >> dispatch)
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
        | Some selectedPiece -> armorDecorationWidget armorType decorations chosenSet dispatch

      ]
    ]

  let charmItem charms chosenSet dispatch =
    let charms = charms
    
    let findCharmFromId (id:string) =
      let matchingCharms: Charm list = charms |> List.filter (fun c -> c.Id |> sprintf "%i" = id)
      let matchingCharm = matchingCharms |> List.tryHead
      matchingCharm

    let containsSkillSimple (charm:Charm option) (searchQuery:string) =
      match charm with
      | None -> false
      | Some charm ->
        charm.Ranks
        |> Array.exists (fun (cr: CharmRank) -> cr.Skills |> Array.exists ( fun sr -> sr.SkillName.ToLowerInvariant().StartsWith(searchQuery.ToLowerInvariant())))

    let filterOptions (item:SelectItem) (searchQuery:string) =
      not item.disabled
      &&
        (item.name.Contains searchQuery
          || (containsSkillSimple (findCharmFromId item.value) searchQuery)
      )

    let findCharmRankFromLevel (charm:Charm) (level:string) : CharmRank option =
      let matchingCharmRank = charm.Ranks |> Array.filter (fun cr -> cr.Level |> string = level)
      matchingCharmRank |> Array.tryHead

    Html.div [
      prop.className "armor-item flex flex-row bg-white/80 rounded-md shadow-md p-4 w-5/6 lg:w-3/4 lg:max-w-2xl"
      prop.children [
        Html.div [
          prop.className "flex flex-row"
          prop.children [

            Html.div [
              prop.className "flex flex-col"
              prop.children [
                Html.div [
                  SelectSearch.selectSearch [
                    selectSearch.value (chosenSet.Charm |> Option.map (fun (charm, charmRank) -> charm.Id |> sprintf "%i") |> Option.defaultValue "")
                    selectSearch.placeholder "Select a Charm"
                    selectSearch.search true
                    selectSearch.filterOptions filterOptions
                    selectSearch.onChange
                      ( findCharmFromId
                        >> (fun charm -> 
                          match charm |> Option.map (fun c -> c.Ranks) with 
                          | Some ranks when ranks |> Array.length > 0 ->{ chosenSet with Charm = charm |> Option.map (fun c -> (c, c.Ranks |> Array.sortByDescending (fun sr -> sr.Level) |> Array.head)) }
                          | _ -> chosenSet
                          )
                        >> UpdateChosenSet
                        >> dispatch
                      )
                    selectSearch.options [
                        for charm in charms -> { value = charm.Id |> sprintf "%i"; name = charm.Name; disabled = false }
                      ]
                  ]
                ]
              ]
            ]
          ]
        ]
        match chosenSet.Charm with
        | None -> Html.none
        | Some (chosenCharm, charmRank) when chosenCharm.Ranks.Length = 1 ->
          Html.div [
            prop.children [
              Html.text ((chosenCharm.Ranks |> Array.head).Level |> string)
            ]
          ]
        | Some (chosenCharm, charmRank) ->
          Html.div [
            SelectSearch.selectSearch [
                selectSearch.value (charmRank |> string)
                selectSearch.placeholder "Rank"
                selectSearch.search true
                selectSearch.filterOptions filterOptions
                selectSearch.onChange
                  ( findCharmRankFromLevel chosenCharm
                    >> (fun cr -> { chosenSet with Charm = Some (chosenCharm, cr |> Option.defaultValue (chosenCharm.Ranks |> Array.sortByDescending (fun sr -> sr.Level) |> Array.head))})
                    >> UpdateChosenSet
                    >> dispatch
                  )

                selectSearch.options [
                    for charmRank in chosenCharm.Ranks -> { value = charmRank.Level |> sprintf "%i"; name = charmRank.Level |> sprintf "%i"; disabled = false }
                  ]
              ]
          ]
      ]
    ]

let view (model:Model) dispatch =

    match model |> modelLoadedStatus with
    | Deferred.NotAsked -> Html.text "NotAsked"
    | Deferred.InProgress -> Html.text "Loading..."
    | Deferred.Failure f -> Html.text (sprintf "Failed with \"%s\" and maybe more errors" f) 
    | Deferred.Success model ->
      let totalSkills = (model.ChosenSet |> totalSkills |> accumulateSkills)
      Html.section [
          prop.className "h-screen w-screen"
          prop.style [
              style.backgroundSize "cover"
              style.backgroundImageUrl "https://unsplash.it/1200/900?random"
              style.backgroundPosition "no-repeat center center fixed"
          ]
          prop.children [
              Html.a [
                  prop.href "https://safe-stack.github.io/"
                  prop.className "absolute block ml-12 h-12 w-12 bg-teal-300 hover:cursor-pointer hover:bg-teal-400"
                  prop.children [ Html.img [ prop.src "/favicon.png"; prop.alt "Logo" ] ]
              ]
              Html.div [
                prop.className "content flex flex-col items-center justify-center h-full w-full"
                prop.children [
                  Html.div [
                    prop.className "armorsetbuilder flex flex-col items-center stretch center center w-full"
                    prop.children [
                      Components.armorItem ArmorType.Headgear model.Decorations model.Armor model.ChosenSet dispatch
                      Components.armorItem ArmorType.Chest model.Decorations model.Armor model.ChosenSet dispatch
                      Components.armorItem ArmorType.Gloves model.Decorations model.Armor model.ChosenSet dispatch
                      Components.armorItem ArmorType.Waist model.Decorations model.Armor model.ChosenSet dispatch
                      Components.armorItem ArmorType.Legs model.Decorations model.Armor model.ChosenSet dispatch
                      Components.charmItem model.Charms model.ChosenSet dispatch
                    ]
                  ]
                ]
              ]

              Html.div [
                prop.className "Armor Summary"
                prop.children [
                  for skill in totalSkills ->
                  Html.h3 (sprintf "%s: %i" skill.SkillName skill.Level)
                ]
              ]
          ]
      ]