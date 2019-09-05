module Application

open System

let inline (^) f x = f x

type Message = { text : string; url : Uri }
type Database = { newMessages : Message list; sended : Message list }
type RawMessage = RawMessage of string

module Store =
    let private store = ref { newMessages = []; sended = [] }

    let update f = async { store := f !store }
    let dispatch f = async {
        let (db, x) = f !store
        store := db
        return x }

module Domain =
    open System.Text.RegularExpressions

    let private findUrl (text : string) : Uri option =
        Regex.Match(text, "https://[^\" ]+")
        |> fun x -> if x.Success then Some x.Value else None
        |> Option.map Uri

    let update db message =
        match findUrl message with
        | Some url ->
            let exists =
                db.newMessages
                |> List.append db.sended
                |> List.exists ^ fun x -> x.url = url
            if exists then db
            else { db with newMessages = { text = message; url = url } :: db.newMessages }
        | None -> db

    let private mkMessage (updates : RawMessage list) =
        updates
        |> List.choose ^ fun (RawMessage x) -> findUrl x |> Option.map ^ fun y -> x, y
        |> List.groupBy ^ fun (_, x) -> x
        |> List.map ^ fun (url, xs) -> fst xs.[0], url
        |> List.map ^ fun (x, _) -> sprintf "%s" x
        |> List.fold (sprintf "%s- %s\n") ""
        |> fun x -> if String.IsNullOrEmpty x then None else Some x

    let mkMesssage db =
        let message =
            db.newMessages
            |> List.map ^ fun x -> RawMessage x.text
            |> mkMessage
        { db with newMessages = []; sended = db.newMessages @ db.sended }, message

open Telegram.Bot.Types

[<EntryPoint>]
let main _ =
    let listenSlack tag =
        SlackParser.startReadingUpdates
            tag
            ^ fun message ->
                Store.update ^ fun db -> Domain.update db message

    let sendToTelegram token = async {
        while true do
            let! optMsg = Store.dispatch Domain.mkMesssage

            match optMsg with
            | None -> ()
            | Some msg ->
                let bot = Telegram.Bot.TelegramBotClient token
                let! _ = bot.SendTextMessageAsync(ChatId.op_Implicit "TODO", msg) |> Async.AwaitTask
                ()

            do! Async.Sleep 5000 }

    Async.Parallel
        [ sendToTelegram "TODO"
          listenSlack "compose"
          listenSlack "feed" ]
    |> Async.RunSynchronously |> ignore
    0
