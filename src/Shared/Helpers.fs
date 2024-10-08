namespace Helpers

module Prelude =
    let flip f a b = f b a

    // let memoizeConcurrent f = 
    //   let cache = System.Collections.Concurrent.ConcurrentDictionary()
    //   fun x -> cache.GetOrAdd(x, lazy f x).Value

    let memoize f =
      let cache = new System.Collections.Generic.Dictionary<_,_>()
      fun x ->
        match cache.TryGetValue x with
        | true, result -> result
        | _ -> let result = f x in cache.Add(x, result); result

module Async =
    let map f computation =
        async.Bind(computation, f >> async.Return)


module List =

    let tryMax xs = 
      match xs with 
      | [] -> None
      | _ -> Some (List.max xs)

    let join (on: 'x -> 'y -> bool) (xs: 'x List) (ys: 'y List) =
        let pairs = ys |> List.allPairs xs
        let matches, _notMatched = pairs |> List.partition (fun (x, y) -> on x y)
        let matchedXs, matchedYs = matches |> List.map fst, matches |> List.map snd
        let unmatchedXs = set xs - set matchedXs |> List.ofSeq
        let unmatchedYs = set ys - set matchedYs |> List.ofSeq
        matches, unmatchedXs, unmatchedYs

    let rec foldRest (folder: 'state -> 'x list -> 'x -> 'state) state elements =
        match elements with
        | [] -> state
        | elem :: rest ->
            let nextState = folder state rest elem
            foldRest folder nextState rest

    let allPairsWithoutReplacement (xs: 'x list) : ('x * 'x) list =
        let labelledXs = xs |> List.indexed
        let allPairs = List.allPairs labelledXs labelledXs

        allPairs
        |> List.filter (fun ((idx1, d1), (idx2, d2)) -> not (idx1 = idx2))
        |> List.map (fun ((idx1, d1), (idx2, d2)) -> (d1, d2))


    type private StoredResult<'Element, 'Projection> =
        | Final of ('Element * 'Element list) * 'Projection
        | Searching of ('Element list)

    let tryRemoveByAndWith (projection: 'x -> 'a option) (xs: 'x list) : (('x * 'x list) * 'a) option =
        let folder state elem =
            match state with
            | Final((matched, unusedElements), projected) -> Final((matched, (elem :: unusedElements)), projected)
            | Searching unusedElements ->
                match projection elem with
                | Some projected -> Final((elem, unusedElements), projected)
                | None -> Searching(elem :: unusedElements)

        match xs |> List.fold folder (Searching []) with
        | Final((elem, unusedElements), projected) -> Some((elem, unusedElements |> List.rev), projected)
        | _ -> None

    let tryRemoveByAndWithR (projection: 'x list -> 'x -> 'a option) (xs: 'x list) : (('x * 'x list) * 'a) option =
        let folder state rest elem =
            match state with
            | Final((matched, unusedElements), projected) -> Final((matched, (elem :: unusedElements)), projected)
            | Searching unusedElements ->
                match projection rest elem with
                | Some projected -> Final((elem, unusedElements), projected)
                | None -> Searching(elem :: unusedElements)

        match xs |> foldRest folder (Searching []) with
        | Final((elem, unusedElements), projected) -> Some((elem, unusedElements |> List.rev), projected)
        | _ -> None

    type MatchedElementState<'Element, 'Projection> = {
        MatchedElement: 'Element
        Projection: 'Projection
        NonMatchingElements: 'Element list
        UncheckedElements: 'Element list
    }

    type UnmatchedElementState<'Element> = {
        NonMatchingElements: 'Element list
        UncheckedElements: 'Element list
    }

    type ElementSearchState<'Element, 'Projection> =
        | Match of MatchedElementState<'Element, 'Projection>
        | Searching of UnmatchedElementState<'Element>

    let tryRemoveByAndWithPartial
        (projection: 'x -> 'a option)
        (elements: ElementSearchState<'x, 'a>)
        : ElementSearchState<'x, 'a> =
        let folder state elem =
            match state with
            | Match m ->
                Match {
                    m with
                        UncheckedElements = elem :: m.UncheckedElements
                }
            | Searching u ->
                match projection elem with
                | Some projected ->
                    Match {
                        MatchedElement = elem
                        Projection = projected
                        NonMatchingElements = u.NonMatchingElements
                        UncheckedElements = u.UncheckedElements
                    }
                | None ->
                    Searching {
                        u with
                            NonMatchingElements = elem :: u.NonMatchingElements
                    }

        let elementsToCheck =
            match elements with
            | Match m -> m.UncheckedElements
            | Searching s -> s.UncheckedElements

        elementsToCheck
        |> List.fold
            folder
            (Searching {
                NonMatchingElements = []
                UncheckedElements = []
            })


module Option =
    let (>>=) o f = Option.bind f o

    let maybeDefaultValue (firstChoice: 'a option) (secondChoice: 'a option) = 
      match firstChoice with
      | Some o -> Some o
      | None -> secondChoice 

    let traverseList (f: 'a -> 'b option) (ls: 'a list) : 'b list option =
        let folder (state: 'b list option) (next: 'a) =
            f next >>= (fun next' -> state >>= (fun state' -> Some(next' :: state')))

        ls |> List.fold folder (Some [])

    let sequenceList ls = traverseList id ls

    let withFallback (fallback: 'a -> 'b option) (success: 'a -> 'b option) = 
      fun a ->
        match success a with
        | Some answer -> Some answer
        | None -> fallback a


type SymmetricMatrix<'Key, 'Value when 'Key: comparison> = private {
    KeyIndex: System.Collections.Generic.IReadOnlyDictionary<'Key, int>
    PairIndex: System.Collections.Generic.IReadOnlyDictionary<('Key * 'Key), int>
    mutable Values: 'Value[]
}

module SymmetricMatrix =
    let private rowIndex (rowIdx: int) = rowIdx * (rowIdx + 1) / 2

    let private index (keyIndex: System.Collections.Generic.IReadOnlyDictionary<'Key, int>) (key1: 'Key) (key2: 'Key) =
        let a, b = keyIndex[key1], keyIndex[key2]
        if a > b then (rowIndex a) + b else (rowIndex b) + a

    let get (keys) (symMatrix: SymmetricMatrix<'Key, 'Value>) =
        symMatrix.Values[symMatrix.PairIndex[keys]]

    let getMany (key) (symMatrix: SymmetricMatrix<'Key, 'Value>) = [|
        for key' in symMatrix.KeyIndex.Keys -> symMatrix.Values[symMatrix.PairIndex[(key, key')]]
    |]

    let set (keys) newValue (symMatrix: SymmetricMatrix<'Key, 'Value>) =
        symMatrix.Values[symMatrix.PairIndex[keys]] <- newValue

    let fromSeq
        (keyProjection: 'Value -> 'Key * 'Key)
        (initialValue: 'Value)
        (elements: 'Value seq)
        : SymmetricMatrix<'Key, 'Value> =
        let keys =
            elements
            |> Seq.map (keyProjection >> (fun (a, b) -> [ a; b ]))
            |> Seq.concat
            |> Set.ofSeq

        let keyIndex = keys |> Seq.indexed |> Seq.map (fun (a, b) -> (b, a)) |> readOnlyDict

        let pairIndex =
            let k = List.ofSeq keys

            List.allPairs k k
            |> List.map (fun (k1, k2) -> (k1, k2), (index keyIndex k1 k2))
            |> readOnlyDict

        let mutable values = Array.create (rowIndex (keys |> Seq.length)) initialValue

        for e in elements do
            values[pairIndex[keyProjection e]] <- e

        {
            KeyIndex = keyIndex
            PairIndex = pairIndex
            Values = values
        }

    let tryFromSeq
        (keyProjection: 'Value -> ('Key * 'Key) option)
        (initialValue: 'Value)
        (elements: 'Value list)
        : SymmetricMatrix<'Key, 'Value> option =
        let keyValuePairs =
            elements
            |> Option.traverseList (fun a -> (a |> keyProjection) |> Option.map (fun k -> k, a))

        keyValuePairs
        |> Option.map (fun kvps ->
            let keys = kvps |> List.map fst |> List.map (fun (a, b) -> [ a; b ]) |> List.concat
            let keyIndex = keys |> Seq.indexed |> Seq.map (fun (a, b) -> (b, a)) |> readOnlyDict

            let pairIndex =
                let k = List.ofSeq keys

                List.allPairs k k
                |> List.map (fun (k1, k2) -> (k1, k2), (index keyIndex k1 k2))
                |> readOnlyDict

            let mutable values = Array.create (rowIndex (keys |> Seq.length)) initialValue

            for k, v in kvps do
                values[pairIndex[k]] <- v

            {
                KeyIndex = keyIndex
                PairIndex = pairIndex
                Values = values
            })

    let chooseFromSeq
        (keyProjection: 'Value -> ('Key * 'Key) option)
        (initialValue: 'Value)
        (elements: 'Value list)
        : SymmetricMatrix<'Key, 'Value> =
        let keyValuePairs =
            elements
            |> List.choose (fun a -> (a |> keyProjection) |> Option.map (fun k -> k, a))

        let keys =
            keyValuePairs
            |> List.map fst
            |> List.map (fun (a, b) -> [ a; b ])
            |> List.concat

        let keyIndex = keys |> Seq.indexed |> Seq.map (fun (a, b) -> (b, a)) |> readOnlyDict

        let pairIndex =
            let k = List.ofSeq keys

            List.allPairs k k
            |> List.map (fun (k1, k2) -> (k1, k2), (index keyIndex k1 k2))
            |> readOnlyDict

        let mutable values = Array.create (rowIndex (keys |> Seq.length)) initialValue

        for k, v in keyValuePairs do
            values[pairIndex[k]] <- v

        {
            KeyIndex = keyIndex
            PairIndex = pairIndex
            Values = values
        }

module Constants =

    [<Literal>]
    let skillFile = @"..\..\resources\data\skillNames.json"

    [<Literal>]
    let skillsNameFile = @"..\..\resources\data\skills.json"

    [<Literal>]
    let decorationsFile = @"..\..\resources\data\decorations.json"