namespace Helpers


  module Prelude = 
    let flip f a b = f b a 


  module Async =
      let map f computation =
          async.Bind(computation, f >> async.Return)


  module List = 
    let partitionBy (predicate: 'a -> bool) (list: 'a list) =
      list |> List.filter predicate, list |> List.filter (not << predicate)

    let join (on: 'x -> 'y -> bool) (xs: 'x List) (ys: 'y List) =
      let pairs = ys |> List.allPairs xs
      let matches, _notMatched = pairs |> partitionBy (fun (x, y) -> on x y)
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

  module Constants = 
    
    [<Literal>]
    let skillFile = @"..\..\resources\data\skillNames.json"
    [<Literal>]
    let skillsNameFile = @"..\..\resources\data\skills.json"
    [<Literal>]
    let decorationsFile = @"..\..\resources\data\decorations.json"