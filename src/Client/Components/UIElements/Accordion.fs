namespace Components.UIElements

module Accordion =
    open Feliz

    type AccordionContent = { Title: string; Content: ReactElement }

    let activeColor, inactiveColor = "bg-slate-600", "bg-slate-700"

    let private AccordionTab
        (props:
            {|
                Title: string
                SetTab: (unit -> unit)
                IsActive: bool
            |})
        =
        let color = if props.IsActive then activeColor else inactiveColor

        Html.div [
            prop.className $"accordion-tab {color}"
            prop.style [
                style.custom ("clipPath", "polygon(10% 0%, 90% 0%, 100% 100%, 0% 100%)")
                style.width.maxContent
                style.flexGrow 1
                style.display.flex
                style.justifyContent.center
            ]
            prop.children [
                Html.button [
                    prop.className $"accordion-tab-button {color}"
                    prop.onClick (fun _me -> props.SetTab())
                    prop.children [ Html.text props.Title ]
                ]
            ]
        ]

    let private contentWrapper content =
        Html.div [
            prop.className "accordion-content-wrapper bg-slate-600"
            prop.style [ style.height.inheritFromParent ]
            prop.children [ content ]
        ]

    [<ReactComponent>]
    let Component (props: {| Tabs: AccordionContent list |}) =

        match props.Tabs with
        | [] -> Html.text "This accordion is empty :("
        | (firstTab :: restTabs as tabs) ->
            let tabIndices = props.Tabs |> List.indexed |> Map.ofList
            let activeTab, setActiveTab = React.useState (0)

            Html.div [
                prop.className "w-full max-w-md mx-auto h-full"
                prop.children [
                    Html.div [
                        prop.className "accordion-tab-row bg-slate-800"
                        prop.style [ style.display.flex; style.flexDirection.row ]
                        let tabsAsElements =
                            tabs
                            |> List.mapi (fun i tab ->
                                AccordionTab {|
                                    Title = tab.Title
                                    SetTab = (fun () -> setActiveTab i)
                                    IsActive = i = activeTab
                                |})

                        prop.children tabsAsElements
                    ]
                    tabIndices
                    |> Map.tryFind activeTab
                    |> Option.map (fun ac -> contentWrapper ac.Content)
                    |> Option.defaultValue (Html.text "Tab Index out of bounds! This should not happen.")
                ]
            ]