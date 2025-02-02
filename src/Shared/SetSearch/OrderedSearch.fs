let OrderedSet((elements : 'a seq))

let elems = []

elems |> List.map (fun elem -> seq {1 .. 100} |> List.map (fun _ -> (square elem)))