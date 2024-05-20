namespace Helpers

  module Prelude = 
    let flip f a b = f b a 


  module Async =
      let map f computation =
          async.Bind(computation, f >> async.Return)


  module List = 

    let join (on: 'x -> 'y -> bool) (xs: 'x List) (ys: 'y List) =
      let pairs = ys |> List.allPairs xs
      let matches, _notMatched = pairs |> List.partition (fun (x, y) -> on x y)
      let matchedXs, matchedYs = matches |> List.map fst, matches |> List.map snd
      let unmatchedXs = set xs - set matchedXs |> List.ofSeq
      let unmatchedYs = set ys - set matchedYs |> List.ofSeq
      matches, unmatchedXs, unmatchedYs

  module Option = 
    let (>>=) o f = Option.bind f o 
    
    let traverseList (f: 'a -> 'b option) (ls : 'a list) : 'b list option =
      let folder (state:'b list option) (next : 'a) = f next >>= (fun next' -> state >>= (fun state' -> Some (next' :: state')))
      ls |> List.fold folder (Some [])
    let sequenceList ls = 
      traverseList id ls

  type SymmetricMatrix<'Key, 'Value when 'Key : comparison> private (keys:Set<'Key>, initial_value: 'Value) =
    let keyIndex = keys |> Seq.indexed |> Seq.map (fun (a, b) -> (b, a)) |> readOnlyDict
    let mutable values = Array.create (keys |> Seq.length) initial_value

    let rowIndex' (rowIdx:int) = 
      rowIdx * (rowIdx + 1) / 2

    let index' (key1:'Key) (key2:'Key) = 
      let a, b = keyIndex[key1], keyIndex[key2]
      if a > b then rowIndex'(a) + b else rowIndex'(b) + a

    let singleIndex' key = 
      let kIdx = keyIndex[key]
      let rIdx = rowIndex' kIdx
      let indices = 
        // Column contribution   
        ([kIdx+1 .. ((keys |> Seq.length)-1)]
        |> List.map (fun x -> rowIndex' x + kIdx))
        // Row contribution
        @ [rIdx .. rIdx + kIdx]
      indices

    let pairLookup =
      let k = List.ofSeq keys
      List.allPairs k k 
      |> List.map (fun (k1,k2) -> (k1, k2), (index' k1 k2))
      |> readOnlyDict
      

    member this.Get((key1:'Key), (key2:'Key)) = 
      values[pairLookup[(key1, key2)]]

    member this.GetAll(key:'Key) =
      [| for key' in keys -> values[pairLookup[(key, key')]]|]

    member this.Set(((key1:'Key), (key2:'Key)), (newValue:'Value)) =
      values[pairLookup[(key1, key2)]] <- newValue

    static member FromSeq((keyProjection: 'Value -> 'Key * 'Key), (initialValue:'Value), (elements: 'Value seq )) =
      let keys = elements |> Seq.map (keyProjection >> (fun (a, b) -> [a;b])) |> Seq.concat |> Set.ofSeq
      let symMatrix = SymmetricMatrix(keys, initialValue)
      for e in elements do
        symMatrix.Set(keyProjection e, e)
      symMatrix

    static member TryFromSeq((keyProjection: 'Value -> ('Key * 'Key) option), (initialValue:'Value), (elements: 'Value list )) =
      let keys = 
        elements 
        |> Option.traverseList (fun a -> a |> keyProjection |> Option.map (fun (a, b) -> [a;b])) 
        |> Option.map (List.concat >> Set.ofSeq)
      
      keys |> Option.map (fun keys ->
        let symMatrix = SymmetricMatrix(keys, initialValue)
        for e in elements do
          symMatrix.Set(keyProjection e |> Option.get, e)
        symMatrix
      )

    static member ChooseFromSeq((keyProjection: 'Value -> ('Key * 'Key) option), (initialValue:'Value), (elements: 'Value list )) =
      let keys = 
        elements 
        |> List.choose (fun a -> a |> keyProjection |> Option.map (fun (a, b) -> [a;b]))
        |> List.concat 
        |> Set.ofSeq
  
      let symMatrix = SymmetricMatrix(keys, initialValue)
      for e in elements do
        let keys = keyProjection e
        match keys with
        | None -> ()
        | Some keys -> symMatrix.Set(keys, e)
      symMatrix

  module Constants = 
    
    [<Literal>]
    let skillFile = @"..\..\resources\data\skillNames.json"
    [<Literal>]
    let skillsNameFile = @"..\..\resources\data\skills.json"
    [<Literal>]
    let decorationsFile = @"..\..\resources\data\decorations.json"
