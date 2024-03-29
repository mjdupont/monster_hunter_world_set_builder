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
                      Armor.Component ArmorType.Headgear gameData.Decorations gameData.Armor model.ChosenSet (UpdateChosenSet >> dispatch)
                      Armor.Component ArmorType.Chest gameData.Decorations gameData.Armor model.ChosenSet (UpdateChosenSet >> dispatch)
                      Armor.Component ArmorType.Gloves gameData.Decorations gameData.Armor model.ChosenSet (UpdateChosenSet >> dispatch)
                      Armor.Component ArmorType.Waist gameData.Decorations gameData.Armor model.ChosenSet (UpdateChosenSet >> dispatch)
                      Armor.Component ArmorType.Legs gameData.Decorations gameData.Armor model.ChosenSet (UpdateChosenSet >> dispatch)
                      Charm.Component gameData.Charms model.ChosenSet (UpdateChosenSet >> dispatch)
                    ]
                  ]
                ]
              ]
          ]
      ]