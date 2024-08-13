module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared
open GameDataTypes
open DataAccess

module Storage =
    let armor: ResizeArray<Armor> = ResizeArray()
    let armorSets: ResizeArray<ArmorSet> = ResizeArray()
    let skills: ResizeArray<Skill> = ResizeArray()
    let charms: ResizeArray<Charm> = ResizeArray()
    let decorations: ResizeArray<Decoration> = ResizeArray()

    let weapons: ResizeArray<Weapon> = ResizeArray()


    let initialize = async {
        let! skillNames, skills' = InferredTypes.Skill.loadSkills
        skills.AddRange(skills')
        let! armorSets' = InferredTypes.ArmorSet.loadArmorSets skillNames
        armorSets.AddRange(armorSets')
        let! armor' = InferredTypes.Armor.loadArmor skillNames
        armor.AddRange(armor')
        let! decorations' = InferredTypes.Decoration.loadDecorations skillNames
        let! decorations' = InferredTypes.Decoration.bindIcons decorations'
        decorations.AddRange(decorations')
        let! charms' = InferredTypes.Charm.loadCharms skillNames
        charms.AddRange(charms')

        let! weapons' = InferredTypes.Weapon.loadWeapons
        weapons.AddRange(weapons')

        ()
    }


let monsterHunterApi = {
    getArmor = fun () -> async { return Storage.armor |> List.ofSeq }
    getArmorSets = fun () -> async { return Storage.armorSets |> List.ofSeq }
    getDecorations = fun () -> async { return Storage.decorations |> List.ofSeq }
    getSkills = fun () -> async { return Storage.skills |> List.ofSeq }
    getCharms = fun () -> async { return Storage.charms |> List.ofSeq }
    getWeapons = fun () -> async { return Storage.weapons |> List.ofSeq }
}

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue monsterHunterApi
    |> Remoting.buildHttpHandler

let app = application {
    use_router webApp
    memory_cache
    use_static "public"
    use_gzip
}

[<EntryPoint>]
let main _ =
    Storage.initialize |> Async.RunSynchronously
    run app
    0