namespace Shared

open System
open DataTypes


module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IMHWApi = {
    getArmor: unit -> Async<Armor list>
    getArmorSets: unit -> Async<ArmorSet list>
    getDecorations: unit -> Async<Decoration list>
    getSkills: unit -> Async<Skill list>
    getCharms: unit -> Async<Charm list>
    getWeapons: unit -> Async<Weapon list>
}