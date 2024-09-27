namespace Helpers

type ResultExpression() =
    member this.Bind(maybeX, f) =
        match maybeX with
        | Ok x -> f x
        | Error e -> Error e

    member this.Return x = Ok x

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
module ResultExpression =

    /// <summary>
    /// The computation expression to manage the Result monad.
    ///
    /// Code wrapped in a Result monad is known to potentially fail.
    /// Unlike an Option, the failure case of a Result monad stores data describing why a failure occurred.
    ///
    /// Accessing <c>Result</c> values with <c>let!</c>, <c>do!</c>, <c>return!</c>, etc. methods in the computation expression
    /// will use the value of the Result if the Result is <c>Ok x</c>, or terminate early if the Result is <c>Error e</c>.
    /// </summary>
    let result = ResultExpression()

module Result =

    let (>>=) o f = Result.bind f o

    let traverseList (f: 'a -> Result<'b, 'e>) (ls: 'a list) : Result<'b list, 'e> =

        let folder (state: Result<'b list, 'e>) (next: 'a) =
            f next >>= (fun next' -> state >>= (fun state' -> Ok(next' :: state')))

        ls |> List.fold folder (Ok [])

    let sequenceList ls = traverseList id ls