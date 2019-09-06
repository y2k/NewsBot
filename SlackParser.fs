module SlackParser

open System
open SlackAPI

let inline (^) f x = f x
let inline (>>-) a f = async.Bind(a, fun x -> async.Return ^ f x)
let inline (>>-!) t f = (Async.AwaitTask t) >>- f

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
            do! f m.text
        do! Async.Sleep 30_000 }
