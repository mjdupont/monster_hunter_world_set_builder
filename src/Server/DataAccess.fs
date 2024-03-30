module DataAccess

open FSharp.Data
open DataTypes
open System.Text.RegularExpressions

[<RequireQualifiedAccess>]
module InferredTypes = 

  [<RequireQualifiedAccess>]
  module SkillRank =
    // Source: https://docs.mhw-db.com/#skillrank-fields
    [<Literal>]
    let sample = """{"id":39,"slug":"attack-boost-rank-1","skill":15,"level":1,"description":"Attack +3","modifiers":{"attack":3}}"""

    type Api = JsonProvider<sample>

    let toReal (skillNames : Map<int, string> ) (sr: Api.Root) : SkillRank = 
      { Description = sr.Description
      ; Id = sr.Id
      ; Level = sr.Level
      ; Modifiers = 
        sr.Modifiers.JsonValue 
        |> function 
          | JsonValue.Record mods when mods |> Array.length > 0 ->
            mods |> Array.choose skillRankModifierMatcher
          | _ -> [||]
      ; Skill = sr.Skill
      ; SkillName = skillNames |> Map.tryFind sr.Skill |> Option.defaultValue "" //TODO - handle this better
      }


  [<RequireQualifiedAccess>]
  module Skill =
      // Source: https://docs.mhw-db.com/#skill-fields
      [<Literal>]
      let sample = """{"id":15,"slug":"attack-boost","name":"Attack Boost","description":"Increases attack power. Also ...","ranks":[{"id":39,"slug":"attack-boost-rank-1","skill":15,"level":1,"description":"Attack +3","modifiers":{"attack":3}}]}"""

      type Api = JsonProvider<sample>

      let uri = """https://mhw-db.com/skills"""

      let buildSkillDict (skills: Api.Root []) : Map<int, string> =
        skills |> Array.map (fun skill -> skill.Id, skill.Name) |> Map.ofSeq

      let toReal skillNames (skill: Api.Root) : Skill =
        { Description = skill.Description
        ; Id = skill.Id
        ; Name = skill.Name
        ; Ranks = skill.Ranks |> Array.map (fun sr -> sr.JsonValue.ToString() |> SkillRank.Api.Parse |> SkillRank.toReal skillNames)
        ; Slug = skill.Slug
        } 

      let loadSkills : Async<(Map<int, string> * Skill list)> = 
        async {
          printfn "Loading Skills..."
          let! skills = JsonValue.AsyncLoad(uri)
          let skills =
            skills
            |> (fun x -> x.ToString())
            |> Api.ParseList
            |> Array.ofSeq

          let skillDict = buildSkillDict skills
          let realSkills = 
            skills |> Array.map (toReal skillDict) |> List.ofSeq
          return (skillDict, realSkills)
        }
        

  [<RequireQualifiedAccess>]
  module Decoration =
      // Source: https://docs.mhw-db.com/#decoration-fields
      [<Literal>]
      let sample = """{"id":1,"rarity":5,"slot":1,"name":"Antidote Jewel 1","skills":[{"id":1,"level":1,"modifiers":{},"description":"Reduces the number of times you take poison damage.","skill":1,"skillName":"Poison Resistance"}]}"""

      type Api = JsonProvider<sample>

      let uri = """https://mhw-db.com/decorations"""

      let toReal skillNames (deco:Api.Root) : Decoration =
        let string = deco.Skills |> Array.map (fun skill -> skill.JsonValue |> (_.ToString())) 
        { Id = deco.Id
        ; Name = deco.Name
        ; Rarity = deco.Rarity
        ; Slot = deco.Slot
        ; IconUri = None
        ; Skills = deco.Skills 
          |> Array.map (fun skill -> 
            skill.JsonValue 
            |> _.ToString() 
            |> SkillRank.Api.Parse
            |> SkillRank.toReal skillNames)
        }
      
      let loadDecorations (skillNames : Map<int, string>) : Async<Decoration list> = 
        async {
          printfn "Loading Decorations..."
          let! decorations = JsonValue.AsyncLoad(uri)
          return
            decorations
            |> (fun x -> x.ToString())
            |> Api.ParseList
            |> Array.ofSeq
            |> Array.map (toReal skillNames)
            |> List.ofSeq
        }

      let getIconsFromHtml : Async<(string*string) list> = 
        async {
          let htmlRegex = Regex(System.IO.File.ReadAllLines("../../resources/decoration_image_regex.txt") |> String.concat "")
          let htmlLines = System.IO.File.ReadAllLines "../../resources/decorations.htm"
          let htmlLines = Regex.Split(htmlLines |> String.concat "\n", "<td>")
          let matches = 
            [
              for htmlLine in htmlLines ->
                htmlRegex.Match(htmlLine)
            ] |> List.filter (fun m -> m.Success)

          return matches |> List.map (fun m -> (m.Groups[2].Value, m.Groups[1].Value))
        }

      let hardCodedCaptureUris = 
        [ "Shaver Jewel 3", "68_14.png"
          "Acrobat Jewel 3", "68_3.png"
          "Diversion Jewel 3", "68_7.png"
          "Warming Jewel 2", "67_0.png"
          "Survival Jewel 1", "66_0.png"
        ]

      let obsoleteJewels = 
           [ "Smoke Jewel 1" ]

      let private findMatchedUris decorations = 
        async {
          let! captureUrisFromHtml = getIconsFromHtml
          let captureUris = captureUrisFromHtml @ hardCodedCaptureUris

          let decorationCopy = decorations |> List.ofSeq
          let matchDecorations (matchedDecorations, unmatchedDecorations) (decoration:Decoration) =
            match captureUris |> List.filter(fun (name, url) -> name.ToLowerInvariant() = decoration.Name.ToLowerInvariant()) with
            | [(matchingName, matchingUrl)] -> ((matchingName, decoration, matchingUrl) :: matchedDecorations, unmatchedDecorations)
            | _ -> (matchedDecorations, decoration :: unmatchedDecorations)
          
          return decorationCopy |> List.fold matchDecorations ([], [])
        }

      let bindIcons (decorations : Decoration seq) : Async<Decoration list> = 
        let rootPath = @"images\"

        async {
          let! matchingDecorations, unmatchedDecorations = findMatchedUris decorations
          return 
            matchingDecorations 
            |> List.map (fun (_name, decoration, uri) -> { decoration with IconUri = Some (System.IO.Path.Join([|rootPath; uri|]))})
            |> List.ofSeq
            |> List.filter (fun decoration -> not (obsoleteJewels |> List.contains decoration.Name))
        }






  [<RequireQualifiedAccess>]
  module Armor =
      // Source: https://docs.mhw-db.com/#armor-fields
      [<Literal>]
      let sample = """{"id":159,"slug":"leather-headgear-beta","name":"Leather Headgear Beta","type":"head","rank":"high","rarity":5,"defense":{"base":32,"max":54,"augmented":70},"resistances":{"fire":2,"water":0,"ice":0,"thunder":0,"dragon":0},"slots":[{"rank":1}],"attributes":{},"skills":[{"id":207,"slug":"hunger-resistance-rank-1","level":1,"description":"Extends the time until ...","modifiers":[],"skill":67,"skillName":"Hunger Resistance"}],"armorSet":{"id":35,"name":"Leather Beta","rank":"high","pieces":[159,160,161,162,163]},"assets":{"imageMale":"https://assets.mhw-db.com/armor/...","imageFemale":"https://assets.mhw-db.com/armor/..."},"crafting":{"materials":[{"quantity":2,"item":{"id":119,"name":"Carbalite Ore","description":"Ore obtained from mining outcrops. Still ...","rarity":0,"carryLimit":0,"sellPrice":0,"buyPrice":0}}]}}"""

      type Api = JsonProvider<sample>

      let uri = """https://mhw-db.com/armor"""

      let toReal (skillNames:Map<int, string>) (armor: Api.Root) : Armor =
        { ArmorSet = armor.ArmorSet.Id
        ; Defense = { Base = armor.Defense.Base; Max = armor.Defense.Max; Augmented = armor.Defense.Augmented }
        ; Id = armor.Id
        ; Name = armor.Name
        ; Rank = armor.Rank |> (|Rank|_|) |> Option.defaultValue DataTypes.Rank.Low //TODO Handle this default better
        ; Rarity = armor.Rarity
        ; Resistances = 
          { Ice = armor.Resistances.Ice
          ; Fire = armor.Resistances.Fire
          ; Water = armor.Resistances.Water
          ; Thunder = armor.Resistances.Thunder
          ; Dragon = armor.Resistances.Dragon
          }
        ; Skills = armor.Skills |> Array.map (fun skill -> skill.JsonValue.ToString() |> SkillRank.Api.Parse |> (SkillRank.toReal skillNames))
        ; Slots = armor.Slots |> Array.map (fun slot -> Slot slot.Rank)
        ; Slug = armor.Id.ToString()
        ; Type = armor.Type |> (|ArmorType|_|) |> Option.defaultValue DataTypes.ArmorType.Legs //TODO Handle this default better
        }

      let loadArmor (skillNames: Map<int, string>) : Async<Armor list> = 
        async {
          printfn "Loading Armor..."
          let! armor = JsonValue.AsyncLoad(uri)
          let armor =
            armor
            |> (fun x -> x.ToString())
            |> Api.ParseList
            |> Array.ofSeq
            |> Array.map (toReal skillNames)
            |> List.ofSeq
          return armor
        }


      let naiveSkillPotential (armor : Api.Root) =
        let skillContribution = armor.Skills |> Seq.map (_.Level >> int) |> Seq.sum
        let slotContribution = armor.Slots |> Seq.map (_.Rank >> int >> function | 4 -> 2 | _ -> 1) |> Seq.sum
        let _notused = armor.Skills
        skillContribution + slotContribution

  [<RequireQualifiedAccess>]
  module CharmRank = 
    [<Literal>]
    let sample = """{"level":1,"rarity":3,"skills":[{"id":1,"slug":"poison-resistance-rank-1","level":1,"description":"Reduces the duration of poison by 30%.","skill":1,"skillName":"Poison Resistance","modifiers":[]}],"crafting":{"craftable":true,"materials":[{"quantity":1,"item":{"id":231,"name":"Pukei-Pukei Sac","description":"Pukei-Pukei material. Obtained by ...","rarity":0,"carryLimit":0,"sellPrice":0,"buyPrice":0}}]}}"""

    type Api = JsonProvider<sample>

    let toReal (skillNames:Map<int, string>) (charmRank: Api.Root) : CharmRank =
      { Level = charmRank.Level 
        Rarity = charmRank.Rarity
        Skills = charmRank.Skills 
          |> Array.map (fun skill -> 
            { Id = skill.Id 
              Description = skill.Description
              Level = skill.Level
              Skill = skill.Skill
              SkillName = skill.SkillName
              Modifiers = [||] // TODO
            })
      } 


  [<RequireQualifiedAccess>]
  module Charm =
      // Source: https://docs.mhw-db.com/#charm-fields
      [<Literal>]
      let sample = """{"id":234,"slug":"poison-charm","name":"Poison Charm","ranks":[{"level":1,"rarity":3,"skills":[{"id":1,"slug":"poison-resistance-rank-1","level":1,"description":"Reduces the duration of poison by 30%.","skill":1,"skillName":"Poison Resistance","modifiers":[]}],"crafting":{"craftable":true,"materials":[{"quantity":1,"item":{"id":231,"name":"Pukei-Pukei Sac","description":"Pukei-Pukei material. Obtained by ...","rarity":0,"carryLimit":0,"sellPrice":0,"buyPrice":0}}]}}]}"""

      type Api = JsonProvider<sample>

      let uri = """https://mhw-db.com/charms"""

      let toReal (skillNames:Map<int, string>) (charm: Api.Root) : Charm =
        { Id = charm.Id
        ; Name = charm.Name
        ; Ranks = charm.Ranks |> Array.map (fun charmRank -> charmRank.JsonValue.ToString() |> CharmRank.Api.Parse |> CharmRank.toReal skillNames)
        ; Slug = charm.Slug
        }

      let loadCharms (skillNames: Map<int, string>) : Async<Charm list> = 
        async {
          printfn "Loading Charms..."
          let! charms = JsonValue.AsyncLoad(uri)
          let charms =
            charms
            |> (fun x -> x.ToString())
            |> Api.ParseList
            |> Array.ofSeq
            |> Array.map (toReal skillNames)
            |> List.ofSeq
          return charms
        }



  [<RequireQualifiedAccess>]
  module ArmorSet =
      // Source: https://docs.mhw-db.com/#armorset-fields
      [<Literal>]
      let sample = """[{"id":20,"name":"Anja","rank":"low","pieces":[{"id":84,"slug":"anja-helm","name":"Anja Helm","type":"head","rank":"low","rarity":3,"armorSet":20,"attributes":{"defense":20,"resistFire":3,"resistWater":-3,"resistThunder":-1,"resistIce":-1},"skills":[{"id":80,"slug":"fire-attack-rank-1","level":1,"description":"Fire attack +30","modifiers":{"damageFire":30},"skill":26,"skillName":"Fire Attack"}],"assets":{"imageMale":"https://assets.mhw-db.com/armor/...","imageFemale":"https://assets.mhw-db.com/armor/..."}}],"bonus":{"id":1,"name":"Anjanath Power","ranks":[{"pieces":3,"skill":{"id":311,"slug":"adrenaline-rank-1","level":1,"description":"Temporarily reduces stamina depletion ...","modifiers":[],"skill":112,"skillName":"Adrenaline"}}]}},{"id":20,"name":"Anja","rank":"low","pieces":[{"id":84,"slug":"anja-helm","name":"Anja Helm","type":"head","rank":"low","rarity":3,"armorSet":20,"attributes":{"defense":20,"resistFire":3,"resistWater":-3,"resistThunder":-1,"resistIce":-1},"skills":[{"id":80,"slug":"fire-attack-rank-1","level":1,"description":"Fire attack +30","modifiers":{"damageFire":30},"skill":26,"skillName":"Fire Attack"}],"assets":{"imageMale":"https://assets.mhw-db.com/armor/...","imageFemale":"https://assets.mhw-db.com/armor/..."}}],"bonus":null}]"""

      type Api = JsonProvider<sample, SampleIsList=true>

      let uri = """https://mhw-db.com/armor/sets"""

      let toReal (skillNames:Map<int,string>) (armorSet: Api.Root) = 
        { Id = armorSet.Id
        ; Name = armorSet.Name
        ; Rank = armorSet.Rank |> (|Rank|_|) |> Option.defaultValue DataTypes.Rank.Low //TODO Handle this default better
        ; Pieces = [| for piece in armorSet.Pieces -> piece.Id |]
        ; Bonus = armorSet.Bonus 
          |> Option.map (fun bonus -> 
            { Id = bonus.Id; Name = bonus.Name; Ranks = [| for rank in bonus.Ranks -> { Pieces = rank.Pieces; Skill = rank.Skill.JsonValue.ToString() |> SkillRank.Api.Parse |> (SkillRank.toReal skillNames) } |] }
          ) 
        }

      let loadArmorSets (skillNames: Map<int, string>) : Async<ArmorSet list> = 
        async {
          printfn "Loading ArmorSets..."
          let! armorSets = JsonValue.AsyncLoad(uri)
          let armorSets =
            armorSets
            |> (fun x -> x.ToString())
            |> Api.ParseList
            |> Array.ofSeq
            |> Array.map (toReal skillNames)
            |> List.ofSeq
          return armorSets
        }

  [<RequireQualifiedAccess>]
  module Weapon =
      // Source: https://docs.mhw-db.com/#weapon-fields
      [<Literal>]
      let sample = """{"id":94,"name":"Iron Grace 3","type":"long-sword","rarity":5,"attack":{"display":462,"raw":140},"elderseal":null,"attributes":{"damageType":"sever"},"damageType":"sever","durability":[{"red":90,"orange":50,"yellow":50,"green":80,"blue":30,"white":0,"purple":0}],"slots":[{"rank":1}],"elements":[{"type":"water","damage":120,"hidden":true}],"crafting":{"craftable":false,"previous":93,"branches":[95],"craftingMaterials":[],"upgradeMaterials":[{"quantity":8,"item":{"id":119,"name":"Carbalite Ore","description":"Ore obtained from mining outcrops. Still ...","rarity":6,"carryLimit":99,"value":680}}]},"assets":{"icon":"https://assets.mhw-db.com/weapons/long-sword/icons/...","image":"https://assets.mhw-db.com/weapons/long-sword/..."}}"""

      type Api = JsonProvider<"../../resources/ElementJson/weapons.json", SampleIsList=true>

      let uri = """https://mhw-db.com/weapons"""

      let toReal (weapon:Api.Root) : Weapon = 
        { Id = weapon.Id
        ; Name = weapon.Name
        //; Type: WeaponType
        ; Rarity = weapon.Rarity
        ; Attack = weapon.Attack.Display
        ; Slots = weapon.Slots |> Array.map (fun s -> Slot s.Rank)
        }

      let loadWeapons : Async<Weapon list> =
        async {
          printfn "Loading Weapons..."
          let! weapons = JsonValue.AsyncLoad(uri)
          let weapons =
            weapons
            |> (fun x -> x.ToString())
            |> Api.ParseList
            |> Array.ofSeq
            |> Array.map (toReal)
            |> List.ofSeq
          return weapons
        }

[<AutoOpen>]
module Testing =

    let explore_armor (armor : InferredTypes.Armor.Api.Root array) = 
        let armorMap = 
          armor 
          |> Seq.groupBy _.Type
          |> Seq.choose (fun (key, value) -> DataTypes.(|ArmorType|_|) key |> Option.map (fun x -> x, value))
          |> Map.ofSeq

        for KeyValue(armorSlot, armor) in armorMap do
            printfn "%i Unique pieces of Armor for %A" (armor |> Seq.length) armorSlot

        let armorBySkillPotential = 
            armorMap 
            |> Map.map (fun armorSlot armor -> armor |> Array.ofSeq |> Array.sortByDescending InferredTypes.Armor.naiveSkillPotential)
        
        for KeyValue(armorSlot, armor) in armorBySkillPotential do
            printfn "%A:" armorSlot
            for armorPiece in armor do
              printfn "    %i  %s" (InferredTypes.Armor.naiveSkillPotential armorPiece) armorPiece.Name

    let explore_armorset_skills (armorsets: InferredTypes.ArmorSet.Api.Root array) (skills: InferredTypes.Skill.Api.Root array) =
        let armorsetsWithSkills =
            armorsets
            |> Seq.choose (fun armorset ->
                try
                    armorset.Bonus
                with ex -> None)

        let armorSetSkills =
            armorsetsWithSkills
            |> Seq.map (fun armorSet ->
                armorSet.Ranks
                |> Seq.map (fun rank -> rank.Skill))
            |> Seq.concat

        let nonArmorSetSkills =
            let armorSetSkillIds = armorSetSkills |> Seq.map (fun armorSetSkill -> armorSetSkill.Skill)
            skills
            |> Seq.filter (fun skill -> armorSetSkillIds |> Seq.contains (skill.Id) |> not)


        printfn "Total Skills: %i, Non-Armor-Set Skills: %i" (skills |> Seq.length) (nonArmorSetSkills |> Seq.length)

        printfn ""
        printfn "Armor Set Skills: \n%A" armorSetSkills
        printfn ""
        printfn "Non-Armor Set Skills: \n%A" nonArmorSetSkills




    let explore_decorations (decorations : InferredTypes.Decoration.Api.Root array) = 
        
        let test = 
          [ for deco in decorations ->
              let temp = deco.Skills
              let temp2 = 
                [ for skill in temp ->
                  let mods = skill.Modifiers
                  match mods.JsonValue with
                  | JsonValue.Record y when y |> Array.length > 0 -> Some true
                  | _ -> None
                ]
              temp2
          ]
        
        let groupedDecos = decorations |> Seq.groupBy (fun deco -> deco.Slot)
        
        for slot, decogroup in groupedDecos do
          printfn "Size %i Slot : %i decos" slot (decogroup |> Seq.length)
          for deco in decogroup do printfn "%s" deco.Name
          

        let doublyGroupedDecos = 
          groupedDecos 
          |> Seq.map (fun (size, decos) ->
            (size, decos |> Seq.groupBy (fun deco -> deco.Rarity) |> Map.ofSeq)
          ) |> Map.ofSeq

        for KeyValue(size, decosByRarity) in doublyGroupedDecos do
          printfn "Size %i Decorations:" size
          for KeyValue(rarity, decos) in decosByRarity do
            printfn "    Rarity %i Decorations:" rarity
            for decoration in decos do
              printfn "        %s" decoration.Name

        let fourSlotDecos =
            groupedDecos
            |> Seq.filter (fun (slot, decos) -> slot = 4)
            |> Seq.head
            |> snd

        let multiLevelSkillsInFourSlotDecos =
            fourSlotDecos
            |> Seq.map (fun deco -> deco.Skills)
            |> Seq.filter (fun skills -> skills |> Seq.length = 1)
            |> Seq.concat

        let multipleSkillsInFourSlotDecos =
            fourSlotDecos
            |> Seq.map (fun deco -> deco.Skills)
            |> Seq.filter (fun skills -> skills |> Seq.length > 1)

        let skillsInMultiSkillDecos =
            multipleSkillsInFourSlotDecos
            |> Seq.concat
            |> Seq.distinctBy (fun skill -> skill.Id)

        printfn "\n%i Skills appearing with a different skill across %i decorations:\n" (skillsInMultiSkillDecos |> Seq.length) (multipleSkillsInFourSlotDecos |> Seq.length)

        for skill in skillsInMultiSkillDecos do
            let decosContainingThisSkill = 
                multipleSkillsInFourSlotDecos 
                |> Seq.filter (fun skills -> 
                    skills |> Seq.filter (fun skill' -> skill'.Id = skill.Id) 
                    |> Seq.length > 0)
            in printfn "%s, (%i decorations)" skill.SkillName (decosContainingThisSkill |> Seq.length) 

        let pairedSkills skillName = 
            multipleSkillsInFourSlotDecos
            |> Seq.map (Seq.map (fun x -> x.SkillName))
            |> Seq.filter (Seq.contains skillName)
            |> Seq.concat
            |> Seq.filter (fun skn -> not (skn = skillName))

        let nodeNeighborsList = 
            [| for skill in skillsInMultiSkillDecos |> Seq.map (fun x -> x.SkillName) ->
                (skill, pairedSkills skill |> Array.ofSeq) |]
        
        let sharedNeighbors = nodeNeighborsList |> Seq.groupBy snd |> Map.ofSeq
        let sharedNeighborsPairs = 
            sharedNeighbors 
            |> Map.map (fun neighbors skillPairs -> 
                skillPairs 
                |> Seq.map fst 
                |> Array.ofSeq 
                )
        
        printfn "%i sharedNeighbors Pairs:" (sharedNeighborsPairs |> Seq.length) 
        for KeyValue(key, value) in sharedNeighborsPairs do
            printfn "Skills %A all map to skills %A" value key







    let potential_indices = seq { 1 .. 2000 }
    let print_information () =
      async {

        printfn "Loading ArmorSets..."
        let! armorsets = 
          JsonValue.AsyncLoad(InferredTypes.ArmorSet.uri)
          |> Async.map (fun x -> x.ToString())
          |> Async.map InferredTypes.ArmorSet.Api.ParseList
          |> Async.map Array.ofSeq
   
        

        let! skillNames, realSkills = InferredTypes.Skill.loadSkills
        let! realDecos = InferredTypes.Decoration.loadDecorations skillNames
        let! realArmor = InferredTypes.Armor.loadArmor skillNames
        //explore_decorations decorations

        ()
      }
