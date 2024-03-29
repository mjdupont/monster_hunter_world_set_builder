module HelperFunctions

// Derived from https://zaid-ajaj.github.io/the-elmish-book/#/chapters/commands/deferred-module-utilities, with some changes/augmentations

[<RequireQualifiedAccess>]
type Deferred<'a, 'err> = 
  | NotAsked
  | InProgress
  | Success of 'a
  | Failure of 'err

[<RequireQualifiedAccess>]
module Deferred = 
  let isSuccessful = function Deferred.Success _ -> true | _ -> false

type PartialDeferred<'loadingState, 'completeState, 'err> = 
  | NotAsked
  | InProgress of 'loadingState
  | Success of 'completeState
  | Failure of 'err

type DeferredMessage<'a, 'err> =  
  | InProgress
  | Success of 'a
  | Failure of 'err
