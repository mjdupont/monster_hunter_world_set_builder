namespace Components.UIElements

open Feliz
open ModelData

module Sidebar =

    [<ReactComponent>]
    let Component openName (props: {| Content: ReactElement |})
        =

        let (isOpen, setIsOpen) = React.useState false

        let toggleSidebar _mouseEvent = if isOpen then setIsOpen false else setIsOpen true
        let status = if isOpen then "open" else ""
        let transition = if isOpen then "translate-x-0" else "-translate-x-full"

        Html.div [
          prop.className $"sidebar {status} fixed top-0 left-0 h-full bg-gray-800 text-white transition-transform duration-300 {transition} w-64 z-50"
          prop.children [
            Html.div [
              prop.children [
                Html.button [
                let transition_transform_button = if isOpen then "translate-x-0" else "translate-x-full"
                let transition_transform_button = "translate-x-full"
                prop.className $"sidebar-toggle absolute {transition_transform_button} right-0 transition-transform duration-300 z-51 w-12 h-12 bg-gray-700 text-white rounded-lg focus:outline-none flex items-center justify-center"
                prop.onClick (toggleSidebar)
                prop.children [ Html.text (if isOpen then "<" else openName)]
                ]
              ]
            ]

            Html.div [
              prop.className "sidebar-content"
              prop.style [ style.height.inheritFromParent ]
              prop.children [ props.Content ]
            ]
          ]
        ]
        