namespace Components

module SearchStatus =
    open Feliz
    open ModelData


    [<ReactComponent>]
    let Component
        (props:
            {|
                Status: SearchStatus
                SetSearchStatus: (SearchStatus -> unit)
            |})
        =

        if props.Status = Failed || props.Status = Found then
            Fable.Core.JS.setTimeout (fun () -> props.SetSearchStatus NotAsked) 10000
            |> ignore
        //else 0 |> ignore

        let statusColor =
            match props.Status with
            | Failed -> "red"
            | Found -> "green"
            | _ -> "black"

        let message =
            match props.Status with
            | Failed -> "Failed to find a set..."
            | Found -> "Found a Set!"
            | NotAsked -> "Set weapon decorations, fix pieces/decorations, and choose skills!"
            | Searching -> "Searching..."

        Html.div [
            prop.children [ Html.h3 [ prop.style [ style.color statusColor ]; prop.text message ] ]
        ]