﻿open System

let inline (^) f x = f x
let inline (>>-) a f = async.Bind(a, fun x -> async.Return ^ f x)
let inline (>>-!) t f = (Async.AwaitTask t) >>- f

module SlackParser =
    open SlackAPI

    type SlackConfig =
        { userId : string
          password : string }

    let private getSlackMessages (client : SlackTaskClient) name from = async {
        let! channels = client.GetChannelListAsync() |> Async.AwaitTask
        let channel = channels.channels |> Array.tryFind ^ fun channel -> channel.name = name
        return!
            match channel with
            | None -> async.Return [||]
            | Some id ->
                client.GetChannelHistoryAsync(id, count = Nullable 30, oldest = Nullable from)
                >>-! fun x -> x.messages }

    let startReadingUpdates cfg name f = async {
        let! client =
            SlackClientHelpers().AuthSigninAsync(cfg.userId, "T09229ZC6", cfg.password)
            >>-! fun response -> SlackTaskClient response.token

        let from = ref DateTime.Now

        while true do
            let! messages = getSlackMessages client name !from
            from := messages |> Array.map (fun x -> x.ts) |> Array.max
            for m in messages do
                do f m.text
            do! Async.Sleep 30_000 }

type Message = { text : string; url : Uri }
type Database = { newMessages : Message list; sended : Message list }
type RawMessage = RawMessage of string

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

    let private mkMessage' (updates : RawMessage list) =
        updates
        |> List.choose ^ fun (RawMessage x) -> findUrl x |> Option.map ^ fun y -> x, y
        |> List.groupBy ^ fun (_, x) -> x
        |> List.map ^ fun (url, xs) -> fst xs.[0], url
        |> List.map ^ fun (x, _) -> sprintf "%s" x
        |> List.fold (sprintf "%s- %s\n") ""
        |> fun x -> if String.IsNullOrEmpty x then None else Some x

    let mkMessage db =
        let message =
            db.newMessages
            |> List.map ^ fun x -> RawMessage x.text
            |> mkMessage'
        { db with newMessages = []; sended = db.newMessages @ db.sended }, message

module Store =
    open System.Threading
    
    let private store = ref { newMessages = []; sended = [] }

    let rec dispatch f = 
        let currentValue = !store
        let (newValue, result') = f currentValue
        let result = Interlocked.CompareExchange(store, newValue, currentValue)
        if obj.ReferenceEquals(result, currentValue) then result'
        else Thread.SpinWait 20; dispatch f
    let update f = dispatch ^ fun db -> f db, ()

open Telegram.Bot.Types

[<EntryPoint>]
let main _ =
    let cfg : {| slack : SlackParser.SlackConfig; token : string; chatId : string |} =
        {| slack = { userId = Environment.GetEnvironmentVariable "SLACK_API_USERID"
                     password = Environment.GetEnvironmentVariable "SLACK_API_PASSWORD" }
           token = Environment.GetEnvironmentVariable "TELEGRAM_TOKEN"
           chatId = Environment.GetEnvironmentVariable "TELEGRAM_CHAT_ID" |}

    let listenSlack tag =
        SlackParser.startReadingUpdates cfg.slack tag
            ^ fun message ->
                Store.update ^ fun db -> Domain.update db message

    let sendToTelegram = async {
        while true do
            let optMsg = Store.dispatch Domain.mkMessage

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
