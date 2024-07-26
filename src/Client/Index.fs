module Index

open Elmish
open Shared
open Helpers

open SAFE

open DataTypes
open ModelData
open Components
open HelperFunctions.Deferred
open SetSearchLogic

type MHWDataType =
    | Armor of Armor list
    | ArmorSets of ArmorSet list
    | Decorations of Decoration list
    | Skills of Skill list
    | Charms of Charm list
    | Weapons of Weapon list

type MHWData = {
    Armor: Armor list
    ArmorSets: ArmorSet list
    Decorations: Decoration list
    Skills: Skill list
    Charms: Charm list
    Weapons: Weapon list
}

type LoadingMHWData = {
    Armor: Deferred<Armor list, string>
    ArmorSets: Deferred<ArmorSet list, string>
    Decorations: Deferred<Decoration list, string>
    Skills: Deferred<Skill list, string>
    Charms: Deferred<Charm list, string>
    Weapons: Deferred<Weapon list, string>
} with

    static member Default = {
        Armor = Deferred.NotAsked
        ArmorSets = Deferred.NotAsked
        Decorations = Deferred.NotAsked
        Skills = Deferred.NotAsked
        Charms = Deferred.NotAsked
        Weapons = Deferred.NotAsked
    }

    member this.isFullyLoaded =
        this.Armor |> Deferred.isSuccessful
        && this.ArmorSets |> Deferred.isSuccessful
        && this.Decorations |> Deferred.isSuccessful
        && this.Skills |> Deferred.isSuccessful
        && this.Charms |> Deferred.isSuccessful
        && this.Weapons |> Deferred.isSuccessful

    member this.asFullyLoaded: PartialDeferred<LoadingMHWData, MHWData, string> =
        match this.Armor, this.ArmorSets, this.Decorations, this.Skills, this.Charms, this.Weapons with
        | Deferred.Success a,
          Deferred.Success asets,
          Deferred.Success d,
          Deferred.Success s,
          Deferred.Success c,
          Deferred.Success w ->
            PartialDeferred.Success {
                Armor = a
                ArmorSets = asets
                Decorations = d
                Skills = s
                Charms = c
                Weapons = w
            }
        | _ -> PartialDeferred.Failure "Tried to treat input data as fully loaded before ready"

type Model = {
    GameData: PartialDeferred<LoadingMHWData, MHWData, string>
    Input: string
    ChosenSet: ChosenSet
}

type Msg =
    | LoadData of DeferredMessage<MHWDataType, string>
    | CheckIfFullyLoaded
    | LoadChosenSetFromWebStorage
    | UpdateChosenSet of ChosenSet
    | SetInput of string


let mhwApi =
    Api.makeProxy<IMHWApi> ()

let init () : (Model * Cmd<Msg>) =
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

    let (model: Model) = {
        Input = ""
        GameData = PartialDeferred.NotAsked
        ChosenSet = {
            ChosenSet.Default with
                Weapon = customWeapon
        }
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
            {
                model with
                    GameData = loadingMHWData.asFullyLoaded
            },
            Cmd.ofMsg LoadChosenSetFromWebStorage
        | _ -> model, Cmd.none
    | LoadChosenSetFromWebStorage ->
        let loadedChosenSet =
            match model.GameData with
            | PartialDeferred.Success gameData ->
                let loadedChosenset =
                    ChosenSet.readFromWebStorage gameData.Decorations gameData.Weapons gameData.Armor gameData.Charms

                loadedChosenset
            | _ -> model.ChosenSet

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

        match loadedChosenSet.Weapon with
        | None ->
            {
                model with
                    ChosenSet = {
                        loadedChosenSet with
                            Weapon = customWeapon
                    }
            },
            Cmd.none
        | Some w ->
            {
                model with
                    ChosenSet = loadedChosenSet
            },
            Cmd.none
    | UpdateChosenSet set ->
        do ChosenSet.storeToWebStorage set
        { model with ChosenSet = set }, Cmd.none
    | SetInput value -> { model with Input = value }, Cmd.none

open Feliz

let view (model: Model) dispatch =

    match model.GameData with
    | PartialDeferred.NotAsked
    | PartialDeferred.InProgress _ -> Html.text "Loading..."
    | PartialDeferred.Failure f -> Html.text (sprintf "Failed with \"%s\" and maybe more errors" f)
    | PartialDeferred.Success gameData ->
        let armorSetBonuses =
            model.ChosenSet |> ChosenSet.armorSetBonuses gameData.ArmorSets

        let totalSkills = (model.ChosenSet |> ChosenSet.totalSkills |> accumulateSkills)

        let totalSkillsElement = [
            for skill in totalSkills do
                let skillFromData =
                    gameData.Skills
                    |> Seq.filter (fun skillData -> skillData.Id = skill.Skill)
                    |> Seq.tryExactlyOne

                let skillColor =
                    match skillFromData with
                    | None -> "black"
                    | Some skillData ->
                        let maxLevel =
                            skillData.Ranks
                            |> Seq.sortByDescending (fun sr -> sr.Level)
                            |> Seq.head
                            |> (fun sr -> sr.Level)

                        match skill with
                        | s when s.Level = maxLevel -> "green"
                        | s when s.Level > maxLevel -> "red"
                        | _ -> "black"

                yield
                    Html.div [
                        prop.className ""
                        prop.children [
                            Html.h3 [
                                prop.style [ style.color skillColor ]
                                prop.text (sprintf "%s: %i" skill.SkillName skill.Level)
                            ]
                        ]
                    ]
        ]

        let armorSetSkillsElement = [
            for bonus, rank in armorSetBonuses ->
                Html.div [
                    prop.className ""
                    prop.children [
                        Html.h2 [
                            prop.style [ style.color "black" ]
                            prop.text (sprintf "%s - %s" bonus.Name rank.Skill.SkillName)
                        ]
                    ]
                ]
        ]

        let updateArmorPiece armorType newPiece =
            match armorType with
            | Headgear -> {
                model.ChosenSet with
                    Headgear =
                        if newPiece = model.ChosenSet.Headgear then
                            None
                        else
                            newPiece
              }
            | Chest -> {
                model.ChosenSet with
                    Chest = if newPiece = model.ChosenSet.Chest then None else newPiece
              }
            | Gloves -> {
                model.ChosenSet with
                    Gloves = if newPiece = model.ChosenSet.Gloves then None else newPiece
              }
            | Waist -> {
                model.ChosenSet with
                    Waist = if newPiece = model.ChosenSet.Waist then None else newPiece
              }
            | Legs -> {
                model.ChosenSet with
                    Legs = if newPiece = model.ChosenSet.Legs then None else newPiece
              }

        let armorProps armorType : ({| Decorations : Decoration seq; Armor: Armor seq; ChosenArmor: HelperFunctions.PropDrill<(Armor * DecorationSlots) option> |}) =
            let filteredArmor = gameData.Armor |> Seq.filter (fun a -> a.Type = armorType)
            let updateArmor = (updateArmorPiece armorType >> UpdateChosenSet >> dispatch)

            {|
                Decorations = gameData.Decorations
                Armor = filteredArmor
                ChosenArmor = {
                    Value = model.ChosenSet.getPiece armorType
                    Update = updateArmor
                }
            |}

        Html.section [
            prop.className "h-screen w-screen"
            prop.style [
                style.backgroundSize "cover"
                //style.backgroundImageUrl "https://unsplash.it/1200/900?random"
                style.backgroundColor "black"
                style.backgroundPosition "no-repeat center center fixed"
            ]
            prop.children [
                Html.a [
                    prop.href "https://safe-stack.github.io/"
                    prop.className "absolute block ml-12 h-12 w-12 bg-teal-300 hover:cursor-pointer hover:bg-teal-400"
                    prop.children [ Html.img [ prop.src "/favicon.png"; prop.alt "Logo" ] ]
                ]
                Html.div [
                    prop.className "content flex flex-row h-full w-full gap-8" // items-center justify-center
                    prop.children [
                        Html.div [
                            prop.className "armor-summary m-auto bg-white/80 rounded-md shadow-md p-4"
                            prop.children ([ armorSetSkillsElement; totalSkillsElement ] |> List.concat)
                        ]
                        Html.div [
                            prop.className
                                "armorsetbuilder m-auto flex flex-col items-center stretch center center w-max bg-white/80 rounded-md shadow-md"
                            prop.children [
                                //Weapon.Component gameData.Decorations gameData.Weapons model.ChosenSet.Weapon ((fun weapon -> { model.ChosenSet with Weapon = weapon }) >> UpdateChosenSet >> dispatch)
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
                                Armor.Component(armorProps Headgear)
                                Armor.Component(armorProps Chest)
                                Armor.Component(armorProps Gloves)
                                Armor.Component(armorProps Waist)
                                Armor.Component(armorProps Legs)
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
                                    Skills = gameData.Skills
                                    SubmitSkills = (fun skills -> ())
                                |}
                            ]
                        ]
                    ]
                ]
            ]
        ]