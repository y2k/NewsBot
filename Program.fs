open System

module Domain =
    open System.Text.RegularExpressions

    type Message = { text : string; url : Uri }
    type Database = { newMessages : Message list; sended : Message list }
    type RawMessage = RawMessage of string

    let private findUrl (text : string) : Uri option =
        Regex.Match(text, "https://[^\" ]+")
        |> fun x -> if x.Success then Some x.Value else None
        |> Option.map Uri

    let saveMessagesFromSlack db message =
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
        |> List.groupBy snd
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

module SlackParser =
    open SlackAPI

    type SlackConfig = { userId : string; password : string }

    let private getSlackMessages (client : SlackTaskClient) name from = async {
        let! channels = client.GetChannelListAsync() |> Async.AwaitTask
        return!
            channels.channels
            |> Array.tryFind ^ fun channel -> channel.name = name
            |> Option.defaultWith ^ fun _ -> failwithf "Can' find channel '%s'" name
            |> fun id -> client.GetChannelHistoryAsync(id, count = Nullable 30, oldest = Nullable from)
            >>-! fun x -> x.messages }

    let startReadingUpdates cfg name f = async {
        let! client =
            SlackClientHelpers().AuthSigninAsync(cfg.userId, "T09229ZC6", cfg.password)
            >>-! fun response -> SlackTaskClient response.token

        let from = ref DateTime.Now

        while true do
            let! messages = getSlackMessages client name !from

            if not ^ Array.isEmpty messages then
                messages |> Array.map (fun x -> x.text) |> printfn "LOGX (%s) :: %A" name 

            from := 
                messages 
                |> Array.map (fun x -> x.ts) 
                |> Array.sortDescending 
                |> Array.tryHead
                |> Option.defaultValue !from
            for m in messages do
                do f m.text
            do! Async.Sleep 30_000 }

let Store = Atom.atom ({ Domain.Database.newMessages = []; Domain.Database.sended = [] })

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
                Store.update ^ fun db -> Domain.saveMessagesFromSlack db message

    let sendToTelegram = async {
        while true do
            let optMsg = Store.dispatch Domain.mkMessage
            optMsg |> Option.iter ^ printfn "Telegram msg: %O"
            do! optMsg
                |> Option.map ^ fun msg ->
                    let bot = Telegram.Bot.TelegramBotClient cfg.token
                    bot.SendTextMessageAsync(ChatId.op_Implicit cfg.chatId, msg) |> Async.AwaitTask |> Async.Ignore
                |> Option.defaultValue ^ async.Zero()

            do! Async.Sleep 5000 }

    Async.Parallel
        [ sendToTelegram
          listenSlack "getting-started"
          listenSlack "android"
          listenSlack "random"
          listenSlack "general"
          listenSlack "compose"
          listenSlack "feed" ]
    |> Async.RunSynchronously |> ignore
    0
