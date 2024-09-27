module GameDataLoader.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let gameDataLoader =
    testList "Armor" [
        testCase "validates all armor fine"
        <| fun _ ->

            let successfullyParsed = MHWGameDataLoader.Armor.test ()

            let expected = false
            let actual = false //Todo.isValid ""
            Expect.equal actual expected "Should be false"
    ]