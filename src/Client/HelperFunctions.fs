module HelperFunctions

open Interfaces

// Derived from https://zaid-ajaj.github.io/the-elmish-book/#/chapters/commands/deferred-module-utilities, with some changes/augmentations
module Deferred =
    [<RequireQualifiedAccess>]
    type Deferred<'a, 'err> =
        | NotAsked
        | InProgress
        | Success of 'a
        | Failure of 'err

    [<RequireQualifiedAccess>]
    module Deferred =
        let isSuccessful =
            function
            | Deferred.Success _ -> true
            | _ -> false

    [<RequireQualifiedAccess>]
    type PartialDeferred<'loadingState, 'completeState, 'err> =
        | NotAsked
        | InProgress of 'loadingState
        | Success of 'completeState
        | Failure of 'err

    [<RequireQualifiedAccess>]
    type DeferredMessage<'a, 'err> =
        | InProgress
        | Success of 'a
        | Failure of 'err

type PropDrill<'a> = { Value: 'a; Update: 'a -> unit }

let inline matchesByNameOrSkillsByPredicate
    (namePred: 'a -> string)
    (skillsPred: 'a -> string list)
    (queryString: string)
    (element: 'a)
    : bool =
    [ element |> namePred ] @ (element |> skillsPred)
    |> List.exists (fun nameStr -> nameStr.ToLowerInvariant().StartsWith(queryString.ToLowerInvariant()))

let inline matchesByNameOrSkills
    (queryString: string)
    (element: 'a )
    : bool when 'a :> IHasName and 'a :> SetSearchLogic.Interfaces.IProvidesSkills<'s> and Skill<'s> =
    [ yield element.Name; for (s, level) in element.Skills -> s.Name ]
    |> List.exists (fun nameStr -> nameStr.ToLowerInvariant().StartsWith(queryString.ToLowerInvariant()))