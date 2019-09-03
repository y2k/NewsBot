module SlackParser

let private (>>-) a f = async.Bind(a, fun x -> async.Return <| f x)
let private (>>-!) t f = (Async.AwaitTask t) >>- f

module Slack =
    open System
    open SlackAPI

    type SlackChannel = 
        { channel_id : string
          name : string
          num_members : int
          purpose : string }

    type Message = 
        { text : string
          user : string
          ts   : string }

    let private userId = lazy (Environment.GetEnvironmentVariable "SLACK_API_USERID")
    let private password = lazy (Environment.GetEnvironmentVariable "SLACK_API_PASSWORD")
    let private cachedClient : SlackTaskClient option ref = ref None

    let private makeClient =
        async {
            match !cachedClient with
            | Some client -> return client
            | None ->
                let! response =
                    SlackClientHelpers().AuthSigninAsync(userId.Value, "T09229ZC6", password.Value)
                    |> Async.AwaitTask
                let x = SlackTaskClient response.token
                cachedClient := Some x
                return x
        }

    let getSlackChannels =
        async {
            let! client = makeClient
            let! channels = client.GetChannelListAsync() |> Async.AwaitTask

            return
                channels.channels
                |> Array.map (fun x -> { channel_id = x.id
                                         name = x.name
                                         num_members = x.num_members
                                         purpose = "" })
                |> Array.toList
        }

    let getSlackMessages name =
        async {
            let! client = makeClient
            let! channels = client.GetChannelListAsync() |> Async.AwaitTask

            let id =
                channels.channels
                |> Array.tryFind (fun x -> x.name = name)

            let! history =
                match id with
                | None -> async.Return [||]
                | Some id ->
                    client.GetChannelHistoryAsync(id, count = Nullable 10)
                    >>-! fun x -> x.messages

            return
                history
                |> Array.map (fun x ->
                    { text = x.text
                      user = x.username
                      ts = string x.ts.Ticks })
                |> Array.toList
        }
