module Application

open System
open SlackParser

type Message = { text : string; url : Uri }
type Database = { newMessages : Message list; sended : Message list }
type RawMessage = RawMessage of string

module Store =
    let private store = ref { newMessages = []; sended = [] }

    let dispatch f = async {
        let (db, x) = f !store
        store := db
        return x }
    let update f = dispatch ^ fun db -> f db, ()

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
    let cfg : {| slack : SlackConfig; token : string; chatId : string |} =
        {| slack = { userId = Environment.GetEnvironmentVariable "SLACK_API_USERID"
                     password = Environment.GetEnvironmentVariable "SLACK_API_PASSWORD" }
           token = Environment.GetEnvironmentVariable "TELEGRAM_TOKEN"
           chatId = Environment.GetEnvironmentVariable "TELEGRAM_CHAT_ID" |}

    let listenSlack tag =
        startReadingUpdates cfg.slack tag
            ^ fun message ->
                Store.update ^ fun db -> Domain.update db message

    let sendToTelegram = async {
        while true do
            let! optMsg = Store.dispatch Domain.mkMesssage

            do! optMsg
                |> Option.map ^ fun msg ->
                    let bot = Telegram.Bot.TelegramBotClient cfg.token
                    bot.SendTextMessageAsync(ChatId.op_Implicit cfg.chatId, msg) |> Async.AwaitTask |> Async.Ignore
                |> Option.defaultValue ^ async.Zero()

            do! Async.Sleep 5000 }

    Async.Parallel
        [ sendToTelegram
          listenSlack "compose"
          listenSlack "feed" ]
    |> Async.RunSynchronously |> ignore
    0
