namespace Helpers

// This entire code is directly pulled from the SafetyFirst library.
// Source: https://github.com/ntwilson/SafetyFirst/blob/main/SafetyFirst/OptionExpression.fs
type OptionExpression() =
    member this.Bind(maybeX, f) =
        match maybeX with
        | Some x -> f x
        | None -> None

    member this.Return x = Some x

    member this.ReturnFrom x = x

    member this.Delay f = f
    member this.Run f = f ()

    member this.TryWith(body, handler) =
        try
            this.ReturnFrom(body ())
        with e ->
            handler e

    member this.TryFinally(body, compensation) =
        try
            this.ReturnFrom(body ())
        finally
            compensation ()

    member this.Using(disposable: #System.IDisposable, body) =
        let body' = fun () -> body disposable

        this.TryFinally(
            body',
            fun () ->
                match disposable with
                | null -> ()
                | disp -> disp.Dispose()
        )


[<AutoOpen>]
module OptionExpression =

    /// <summary>
    /// The computation expression to manage the Option monad.
    ///
    /// Code wrapped in a Option monad is known to potentially fail.
    ///
    /// Accessing <c>Option</c> values with <c>let!</c>, <c>do!</c>, <c>return!</c>, etc. methods in the computation expression
    /// will use the value of the Option if the Option is <c>Some x</c>, or terminate early if the Option is <c>None</c>.
    /// </summary>
    let option = OptionExpression()

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

    let toResult (error: 'error) (opt: 'value option) : Result<'value, 'error> =
        match opt with
        | Some x -> Ok x
        | None -> Error error