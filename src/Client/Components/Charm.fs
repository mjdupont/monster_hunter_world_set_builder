namespace Components

module Charm =
    open Feliz
    open Feliz.SelectSearch

    open GameDataTypes
    open HelperFunctions

    [<ReactComponent>]
    let Component
        (props:
            {|
                Charms: Charm list
                ChosenCharm: PropDrill<(Charm * CharmRank) option>
            |})
        =

        let addDefaultRank (charm: Charm option) : (Charm * CharmRank) option =
            charm
            |> Option.bind (fun c ->
                c.Ranks
                |> Array.sortByDescending (fun sr -> sr.Level)
                |> Array.tryHead
                |> Option.map (fun cr -> c, cr))

        let findCharmFromId (id: string) =
            let matchingCharms: Charm list =
                props.Charms |> List.filter (fun c -> c.Id |> sprintf "%i" = id)

            let matchingCharm = matchingCharms |> List.tryHead
            matchingCharm

        let containsSkillSimple (charm: Charm option) (searchQuery: string) =
            match charm with
            | None -> false
            | Some charm ->
                charm.Ranks
                |> Array.exists (fun (cr: CharmRank) ->
                    cr.Skills
                    |> Array.exists (fun sr ->
                        sr.SkillName.ToLowerInvariant().StartsWith(searchQuery.ToLowerInvariant())))

        let filterOptions (item: SelectItem) (searchQuery: string) =
            not item.disabled
            && (item.name.Contains searchQuery
                || (containsSkillSimple (findCharmFromId item.value) searchQuery))

        let findCharmRankFromLevel (charm: Charm) (level: string) : CharmRank option =
            let matchingCharmRank =
                charm.Ranks |> Array.filter (fun cr -> cr.Level |> string = level)

            matchingCharmRank |> Array.tryHead

        Html.div [
            prop.className "charm-selector flex flex-row p-4 w-full justify-center items-center gap-8"
            prop.children [
                SelectSearch.selectSearch [
                    selectSearch.value (
                        props.ChosenCharm.Value
                        |> Option.map (fun (charm, charmRank) -> charm.Id |> sprintf "%i")
                        |> Option.defaultValue ""
                    )
                    selectSearch.placeholder "Select a Charm"
                    selectSearch.search true
                    selectSearch.filterOptions filterOptions
                    selectSearch.onChange (findCharmFromId >> addDefaultRank >> props.ChosenCharm.Update)
                    selectSearch.options [
                        for charm in props.Charms ->
                            {
                                value = charm.Id |> sprintf "%i"
                                name = charm.Name
                                disabled = false
                            }
                    ]
                ]
                match props.ChosenCharm.Value with
                | None -> Html.none
                | Some(chosenCharm, charmRank) when chosenCharm.Ranks.Length = 1 ->
                    Html.text ((chosenCharm.Ranks |> Array.head).Level |> string)
                | Some(chosenCharm, charmRank) ->
                    SelectSearch.selectSearch [
                        selectSearch.value (charmRank.Level |> string)
                        selectSearch.placeholder "Rank"
                        selectSearch.search true
                        selectSearch.filterOptions filterOptions
                        selectSearch.onChange (
                            findCharmRankFromLevel chosenCharm
                            >> function
                                | Some cr -> Some(chosenCharm, cr)
                                | None -> chosenCharm |> Some |> addDefaultRank
                            >> props.ChosenCharm.Update
                        )

                        selectSearch.options [
                            for charmRank in chosenCharm.Ranks ->
                                {
                                    value = charmRank.Level |> sprintf "%i"
                                    name = charmRank.Level |> sprintf "%i"
                                    disabled = false
                                }
                        ]
                    ]
            ]
        ]