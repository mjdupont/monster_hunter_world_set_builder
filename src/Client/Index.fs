module Index

open Elmish
open Shared
open Helpers

open SAFE

open APIDataTypes
open ModelData
open Components
open HelperFunctions.Deferred
open SetSearchLogic
open Feliz
open GameData.APIData

type Model = {
    GameData: PartialDeferred<LoadingMHWData, MHWData, string>
    ChosenSet: ChosenSet
    SkillList: SkillList
    SearchStatus: SearchStatus
    UserData: UserData
}

type Msg =
    | LoadData of DeferredMessage<MHWDataType, string>
    | CheckIfFullyLoaded

    | FindMatchingSet of (Skill * int) list
    | FoundSet of ChosenSet
    | SetSearchStatus of SearchStatus

    | UpdateChosenSet of ChosenSet
    | UpdateSkillList of SkillList
    | UpdateUserData of UserData
    | NoOp

let ignoreMsg () = NoOp

let mhwApi = Api.makeProxy<IMHWApi> ()

let customWeapon =
    (Some(
        {
            Attack = 0
            Id = 0
            Name = "Custom Weapon"
            Rarity = 0
            Slots = [||]
        },
        DecorationSlots.FromSlots [||]
    ))

let init () : (Model * Cmd<Msg>) =

    let (model: Model) = {
        GameData = PartialDeferred.NotAsked
        ChosenSet = {
            ChosenSet.Default with
                Weapon = customWeapon
        }
        SkillList = SkillList []
        SearchStatus = NotAsked
        UserData = UserData.Default
    }

    let cmd =
        Cmd.batch [
            Cmd.OfAsync.perform
                (mhwApi.getArmor >> Async.map (MHWDataType.Armor >> DeferredMessage.Success))
                ()
                LoadData
            Cmd.OfAsync.perform
                (mhwApi.getDecorations
                 >> Async.map (MHWDataType.Decorations >> DeferredMessage.Success))
                ()
                LoadData
            Cmd.OfAsync.perform
                (mhwApi.getSkills >> Async.map (MHWDataType.Skills >> DeferredMessage.Success))
                ()
                LoadData
            Cmd.OfAsync.perform
                (mhwApi.getCharms >> Async.map (MHWDataType.Charms >> DeferredMessage.Success))
                ()
                LoadData
            Cmd.OfAsync.perform
                (mhwApi.getArmorSets
                 >> Async.map (MHWDataType.ArmorSets >> DeferredMessage.Success))
                ()
                LoadData
            Cmd.OfAsync.perform
                (mhwApi.getWeapons >> Async.map (MHWDataType.Weapons >> DeferredMessage.Success))
                ()
                LoadData
        ]

    model, cmd

let loadNewGameData model mhwDataType =
    let updateField (loadingData: LoadingMHWData) =
        function
        | MHWDataType.Armor x -> {
            loadingData with
                Armor = Deferred.Success x
          }
        | MHWDataType.Decorations x -> {
            loadingData with
                Decorations = Deferred.Success x
          }
        | MHWDataType.Charms x -> {
            loadingData with
                Charms = Deferred.Success x
          }
        | MHWDataType.Skills x -> {
            loadingData with
                Skills = Deferred.Success x
          }
        | MHWDataType.ArmorSets x -> {
            loadingData with
                ArmorSets = Deferred.Success x
          }
        | MHWDataType.Weapons x -> {
            loadingData with
                Weapons = Deferred.Success x
          }

    match model.GameData with
    | PartialDeferred.NotAsked -> PartialDeferred.InProgress(updateField LoadingMHWData.Default mhwDataType)
    | PartialDeferred.InProgress loadingData -> PartialDeferred.InProgress(updateField loadingData mhwDataType)
    | _ -> model.GameData

let update msg (model: Model) =
    match msg with
    | NoOp -> model, Cmd.none
    | LoadData(DeferredMessage.InProgress) -> model, Cmd.none

    | LoadData(DeferredMessage.Success dataType) ->
        {
            model with
                GameData = loadNewGameData model dataType
        },
        Cmd.ofMsg CheckIfFullyLoaded
    | LoadData(DeferredMessage.Failure _) -> model, Cmd.none // TODO: Handle this case

    | CheckIfFullyLoaded ->
        match model.GameData with
        | PartialDeferred.InProgress loadingMHWData when loadingMHWData.isFullyLoaded ->
            let gameData = loadingMHWData.asFullyLoaded 
            let newModel = 
              {
                  model with
                      GameData = loadingMHWData.asFullyLoaded
              }
            let cmd = 
              match gameData with 
                | PartialDeferred.Success gameData ->
                  Cmd.batch [
                      Cmd.OfAsync.perform 
                        (fun () -> ChosenSet.readFromWebStorage gameData.Decorations gameData.Weapons gameData.Armor gameData.Charms) 
                        () 
                        (fun chosenSet -> UpdateChosenSet (chosenSet |> Option.defaultValue {ChosenSet.Default with Weapon = customWeapon} ))
                      Cmd.OfAsync.perform 
                        (fun () -> SkillList.readFromWebStorage gameData.Skills) 
                        () 
                        (fun skillList -> UpdateSkillList (skillList |> Option.defaultValue (SkillList [])))
                      Cmd.OfAsync.perform 
                        (fun () -> UserData.readFromWebStorage gameData.Armor gameData.Charms gameData.Decorations) 
                        () 
                        (fun maybeUserData -> 
                          match maybeUserData with 
                          | Some userData -> UpdateUserData userData 
                          | None -> UpdateUserData (UserData.allItems gameData.Skills gameData.Armor gameData.Charms gameData.Decorations)
                        )
                  ]

                | _ -> Cmd.none
            newModel, cmd
        
        | _ -> model, Cmd.none

    | UpdateChosenSet set -> 
        { model with ChosenSet = set }, Cmd.OfAsync.perform ChosenSet.storeToWebStorage set ignoreMsg

    | UpdateSkillList list ->
        {
            model with
                SkillList = list |> (fun (SkillList sl) -> SkillList(sl |> List.sort))
        },
        Cmd.OfAsync.perform SkillList.storeToWebStorage list ignoreMsg

    | UpdateUserData userData ->
        { model with UserData = userData }, Cmd.OfAsync.perform UserData.storeToWebStorage userData ignoreMsg



    | FindMatchingSet requestedSkills ->
        let findSetAsync (gameData:MHWData) = 
            async {
                let armor = (model.UserData.Armor |> List.filter snd |> List.map fst |> armorByType)

                let charms =
                    (model.UserData.Charms
                    |> List.map (fun (c, maxR) -> c, c.Ranks |> Array.filter (fun cr -> cr.Level = maxR) |> Array.head))

                let decorations = model.UserData.Decorations

                let maybeSet = 
                  match assignArmor3 1 gameData.Skills model.ChosenSet armor charms decorations requestedSkills with
                  | [] -> None
                  | set :: rest -> Some (set, rest)

                printfn "%A" maybeSet
                return maybeSet
                  
            }
        let onSuccess maybeSet = 
          match maybeSet with
          | Some (set, _rest) -> FoundSet set
          | None -> SetSearchStatus Failed
        
        let onFail _e = 
          SetSearchStatus Failed
        
        match model.GameData, model.ChosenSet.Weapon with
            | PartialDeferred.Success gameData, _ ->
                model, Cmd.OfAsync.either findSetAsync gameData onSuccess onFail
            | _ -> model, Cmd.ofMsg (SetSearchStatus Failed)
          
     | FoundSet set ->
        model, Cmd.batch [ Cmd.ofMsg (UpdateChosenSet set); Cmd.ofMsg (SetSearchStatus Found) ] 

    | SetSearchStatus status ->
        match status with
        | Found
        | Failed
        | _ -> { model with SearchStatus = status }, Cmd.none


let view (model: Model) dispatch =

    match model.GameData with
    | PartialDeferred.NotAsked
    | PartialDeferred.InProgress _ -> Html.text "Loading..."
    | PartialDeferred.Failure f -> Html.text (sprintf "Failed with \"%s\" and maybe more errors" f)
    | PartialDeferred.Success gameData ->
        let sortedSkills = partitionSkills gameData.ArmorSets gameData.Decorations gameData.Skills
        let armorSetSkills = sortedSkills.ArmorSetSkills
        let armorSetAndDecorationSkills = sortedSkills.ArmorSetAndDecorationSkills
        let armorUniqueSkills = sortedSkills.ArmorUniqueSkills
        let decorationSkills = sortedSkills.DecorationSkills

        let updateArmorPiece (armorType: ArmorType) newPiece =
            match armorType with
            | ArmorType.Headgear -> {
                model.ChosenSet with
                    Headgear =
                        if newPiece = model.ChosenSet.Headgear then
                            None
                        else
                            newPiece
              }
            | ArmorType.Chest -> {
                model.ChosenSet with
                    Chest = if newPiece = model.ChosenSet.Chest then None else newPiece
              }
            | ArmorType.Gloves -> {
                model.ChosenSet with
                    Gloves = if newPiece = model.ChosenSet.Gloves then None else newPiece
              }
            | ArmorType.Waist -> {
                model.ChosenSet with
                    Waist = if newPiece = model.ChosenSet.Waist then None else newPiece
              }
            | ArmorType.Legs -> {
                model.ChosenSet with
                    Legs = if newPiece = model.ChosenSet.Legs then None else newPiece
              }

        let armorProps
            armorType
            : ({|
                  Decorations: Decoration seq
                  Armor: Armor seq
                  ChosenArmor: HelperFunctions.PropDrill<(Armor * DecorationSlots) option>
              |})
            =
            let filteredArmor = gameData.Armor |> Seq.filter (fun a -> a.Type = armorType)
            let updateArmor = (updateArmorPiece armorType >> UpdateChosenSet >> dispatch)

            {|
                Decorations = gameData.Decorations
                Armor = filteredArmor
                ChosenArmor = {
                    Value = model.ChosenSet.tryGetPiece armorType
                    Update = updateArmor
                }
            |}

        Html.section [
            prop.className "h-screen w-screen"
            prop.children [
                UserEquipmentSelector.Component {|
                    UserData = model.UserData
                    GameData = gameData
                    UpdateUserData = (UpdateUserData >> dispatch)
                |}
                Html.div [
                    prop.className "content flex flex-row h-full w-full gap-8"
                    prop.children [
                        ArmorSetSkillsDisplay.Component {| ChosenSet= model.ChosenSet; GameData = gameData |}
                        Html.div [
                            prop.className
                                "armorsetbuilder m-auto flex flex-col items-center stretch center center w-max bg-white/80 rounded-md shadow-md"
                            prop.children [
                                //Weapon.Component gameData.Decorations gameData.Weapons model.ChosenSet.Weapon ((fun weapon -> { model.ChosenSet with Weapon = weapon }) >> UpdateChosenSet >> dispatch)
                                Html.button [
                                    let newChosenSet = {
                                        ChosenSet.Default with
                                            Weapon = customWeapon
                                    }

                                    prop.onClick (fun _me -> ((UpdateChosenSet newChosenSet) |> dispatch))
                                    prop.children [ Html.text "Clear Chosen Armor/Decorations" ]
                                ]
                                SearchStatus.Component {|
                                    Status = model.SearchStatus
                                    SetSearchStatus = (SetSearchStatus >> dispatch)
                                |}
                                WeaponBuilder.Component {|
                                    Decorations = gameData.Decorations
                                    ChosenWeapon = {
                                        Value = model.ChosenSet.Weapon
                                        Update =
                                            ((fun weapon -> { model.ChosenSet with Weapon = weapon })
                                             >> UpdateChosenSet
                                             >> dispatch)
                                    }
                                |}
                                Armor.Component(armorProps ArmorType.Headgear)
                                Armor.Component(armorProps ArmorType.Chest)
                                Armor.Component(armorProps ArmorType.Gloves)
                                Armor.Component(armorProps ArmorType.Waist)
                                Armor.Component(armorProps ArmorType.Legs)
                                Charm.Component {|
                                    Charms = gameData.Charms
                                    ChosenCharm = {
                                        Value = model.ChosenSet.Charm
                                        Update =
                                            (fun charm ->
                                                { model.ChosenSet with Charm = charm } |> (UpdateChosenSet >> dispatch))
                                    }
                                |}
                            ]
                        ]
                        Html.div [
                            prop.className
                                "armorsetbuilder m-auto flex flex-col items-center stretch center center w-max bg-white/80 rounded-md shadow-md"
                            prop.children [
                                SetSearcher.Component {|
                                    Skills = decorationSkills @ armorUniqueSkills @ armorSetAndDecorationSkills
                                    SkillList = (model.SkillList |> (fun (SkillList sl) -> SkillList (sl |> List.sortBy (fun (skill:Skill, level) -> skill.Name))))
                                    UpdateSkillList = (UpdateSkillList >> dispatch)
                                    SubmitSkills =
                                        (fun skills ->
                                            do dispatch (SetSearchStatus Searching)
                                            skills |> (FindMatchingSet >> dispatch))
                                |}
                            ]
                        ]
                    ]
                ]
            ]
        ]