module SetSearchLogic.Tests

open Shared
open APIDataTypes
open Helpers.Constants
open Server
open DataAccess
open DecorationAssignment
#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let unsafeUnwrapResult d (r: Result<'a, 'err>) =
    match r with
    | Ok r -> r
    | _ -> d

let setSearchLogic =
    testList "SetSearchLogic" [
        testCase "Identifies Slot Sizes"
        <| fun _ -> Expect.isTrue true "Placeholder is true"
    ]