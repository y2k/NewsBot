module Application

open System

let inline (^) f x = f x

module Domain =
    open System.Text.RegularExpressions

    type RawMessage = RawMessage of string

    let findUrl (text : string) : Uri option = 
        Regex.Match(text, "https://[^\" ]+")
        |> fun x -> if x.Success then Some x.Value else None
        |> Option.map Uri

    let parse (updates : RawMessage list) =
        updates
        |> List.choose ^ fun (RawMessage x) -> findUrl x |> Option.map ^ fun y -> x, y
        |> List.map ^ fun (x, u) -> sprintf "%s" x
        |> List.fold (+) ""

open Telegram.Bot

[<EntryPoint>]
let main argv =
    // use client = new Net.Http.HttpClient()
    // let client = TelegramBotClient(argv.[0], client)
    0
