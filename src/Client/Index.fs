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

type MHWDataType =
  | Armor of Armor list
  | Decorations of Decoration list
  | Skills of Skill list
  | Charms of Charm list

type MHWData = 
  { Armor: Armor list
    Decorations: Decoration list
    Skills: Skill list
    Charms: Charm list
  }

type LoadingMHWData = 
  { Armor: Deferred<Armor list, string>
    Decorations: Deferred<Decoration list, string>
    Skills: Deferred<Skill list, string>
    Charms: Deferred<Charm list, string>
  }
  static member Default =
    { Armor = Deferred.NotAsked 
      Decorations = Deferred.NotAsked 
      Skills = Deferred.NotAsked 
      Charms = Deferred.NotAsked 
    }
  member this.isFullyLoaded = 
    this.Armor |> Deferred.isSuccessful 
    && this.Decorations |> Deferred.isSuccessful
    && this.Skills |> Deferred.isSuccessful 
    && this.Charms |> Deferred.isSuccessful

  member this.asFullyLoaded : PartialDeferred<LoadingMHWData, MHWData, string> = 
    match this.Armor, this.Decorations, this.Skills, this.Charms with
    | Deferred.Success a, Deferred.Success d, Deferred.Success s, Deferred.Success c -> PartialDeferred.Success { Armor = a; Decorations = d; Skills = s; Charms = c}
    | _ -> PartialDeferred.Failure "Tried to treat input data as fully loaded before ready"

type Model = 
  { GameData : PartialDeferred<LoadingMHWData, MHWData, string>
    Input: string
    ChosenSet: ChosenSet
  }

type Msg =
    | LoadData of DeferredMessage<MHWDataType, string>
    | CheckIfFullyLoaded
    | LoadChosenSetFromWebStorage 
    | UpdateChosenSet of ChosenSet
    | SetInput of string


let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IMHWApi>

let init () : (Model * Cmd<Msg>)=
    let (model:Model) = { Input = "" ; GameData = PartialDeferred.NotAsked; ChosenSet = ChosenSet.Default }
    let cmd =
      Cmd.batch [
        Cmd.OfAsync.perform (todosApi.getArmor >> Async.map (MHWDataType.Armor >> DeferredMessage.Success)) () LoadData
        Cmd.OfAsync.perform (todosApi.getDecorations >> Async.map (MHWDataType.Decorations >> DeferredMessage.Success)) () LoadData
        Cmd.OfAsync.perform (todosApi.getSkills >> Async.map (MHWDataType.Skills >> DeferredMessage.Success)) () LoadData
        Cmd.OfAsync.perform (todosApi.getCharms >> Async.map (MHWDataType.Charms >> DeferredMessage.Success)) () LoadData
      ]
    model, cmd

let update msg (model:Model) =
    match msg with
    | LoadData (Success (MHWDataType.Armor armor)) -> 
      let newGameData = 
        match model.GameData with 
        | NotAsked -> PartialDeferred.InProgress { LoadingMHWData.Default with Armor = Deferred.Success armor } 
        | PartialDeferred.InProgress p -> PartialDeferred.InProgress { p with Armor = Deferred.Success armor } 
        | _ -> model.GameData
      { model with GameData = newGameData}, Cmd.ofMsg CheckIfFullyLoaded
    | LoadData (Success (MHWDataType.Decorations decorations)) -> 
      let newGameData = 
        match model.GameData with 
        | NotAsked -> PartialDeferred.InProgress { LoadingMHWData.Default with Decorations = Deferred.Success decorations } 
        | PartialDeferred.InProgress p -> PartialDeferred.InProgress { p with Decorations = Deferred.Success decorations } 
        | _ -> model.GameData
      { model with GameData = newGameData}, Cmd.ofMsg CheckIfFullyLoaded
    | LoadData (Success (MHWDataType.Skills skills)) ->       
      let newGameData = 
        match model.GameData with 
        | NotAsked -> PartialDeferred.InProgress { LoadingMHWData.Default with Skills = Deferred.Success skills } 
        | PartialDeferred.InProgress p -> PartialDeferred.InProgress { p with Skills = Deferred.Success skills } 
        | _ -> model.GameData
      { model with GameData = newGameData}, Cmd.ofMsg CheckIfFullyLoaded
    | LoadData (Success (MHWDataType.Charms charms)) ->       
      let newGameData = 
        match model.GameData with 
        | NotAsked -> PartialDeferred.InProgress { LoadingMHWData.Default with Charms = Deferred.Success charms } 
        | PartialDeferred.InProgress p -> PartialDeferred.InProgress  { p with Charms = Deferred.Success charms } 
        | _ -> model.GameData
      { model with GameData = newGameData}, Cmd.ofMsg CheckIfFullyLoaded
    | LoadData (InProgress) -> model, Cmd.none //TODO: Handle this case
    | LoadData (Failure _) -> model, Cmd.none // TODO: Handle this case
    | CheckIfFullyLoaded -> 
      match model.GameData with 
      | PartialDeferred.InProgress loadingMHWData when loadingMHWData.isFullyLoaded -> { model with GameData = loadingMHWData.asFullyLoaded }, Cmd.ofMsg LoadChosenSetFromWebStorage
      | _ -> model, Cmd.none
    | LoadChosenSetFromWebStorage ->
      let loadedChosenSet = 
        match model.GameData with
        | PartialDeferred.Success gameData ->
          let loadedChosenset = ChosenSet.readFromWebStorage gameData.Decorations [] gameData.Armor gameData.Charms
          loadedChosenset
        | _ -> ChosenSet.Default
      { model with ChosenSet = loadedChosenSet}, Cmd.none

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
      prop.className "charm-item flex flex-row bg-white/80 rounded-md shadow-md p-4 w-full justify-center items-center gap-8"
      prop.children [
        Html.div [
          prop.className "flex flex-col w-max"
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
                      | Some ranks when ranks |> Array.length > 0 -> { chosenSet with Charm = charm |> Option.map (fun c -> (c, c.Ranks |> Array.sortByDescending (fun sr -> sr.Level) |> Array.head)) }
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
                selectSearch.value (charmRank.Level |> string)
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

    match model.GameData with
    | PartialDeferred.NotAsked -> Html.text "NotAsked"
    | PartialDeferred.InProgress _ -> Html.text "Loading..."
    | PartialDeferred.Failure f -> Html.text (sprintf "Failed with \"%s\" and maybe more errors" f) 
    | PartialDeferred.Success gameData ->
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
                prop.className "content flex flex-row items-center justify-center h-full w-full gap-8"
                prop.children [
                  Html.div [
                    prop.className "armor-summary bg-white/80 rounded-md shadow-md p-4"
                    prop.children [
                      for skill in totalSkills ->
                      Html.div [
                        prop.className ""
                        prop.children [
                          Html.h3 (sprintf "%s: %i" skill.SkillName skill.Level)
                        ]
                      ]
                    ]
                  ]
                  Html.div [
                    prop.className "armorsetbuilder flex flex-col items-center stretch center center w-max"
                    prop.children [
                      Components.armorItem ArmorType.Headgear gameData.Decorations gameData.Armor model.ChosenSet dispatch
                      Components.armorItem ArmorType.Chest gameData.Decorations gameData.Armor model.ChosenSet dispatch
                      Components.armorItem ArmorType.Gloves gameData.Decorations gameData.Armor model.ChosenSet dispatch
                      Components.armorItem ArmorType.Waist gameData.Decorations gameData.Armor model.ChosenSet dispatch
                      Components.armorItem ArmorType.Legs gameData.Decorations gameData.Armor model.ChosenSet dispatch
                      Components.charmItem gameData.Charms model.ChosenSet dispatch
                    ]
                  ]
                ]
              ]
          ]
      ]