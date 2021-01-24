#r "nuget: Akka.FSharp" 
#r "nuget: FSharp.Data, Version=3.0.1"
open Akka.FSharp
open FSharp.Data
open Akka.Actor
open System.Threading
open System
open FSharp.Data.HttpRequestHeaders
let mutable USERNAME = ""
let mutable feed:list<string> = []

let printMessage (msg: string) = 
    printfn "\n    ***********************************"
    printfn "            %s" msg
    printfn "    ***********************************"

let printTweet (tweet:string) =
    let arr = tweet.Split('~') |> Array.toList
    if arr.Length = 2 then
        printfn "    Writer: %s | Tweet: %s" arr.[0] arr.[1]
    else
        printfn "    Writer: %s | Original Writer: %s | Tweet: %s" arr.[0] arr.[1] arr.[2]

/// User registration command
let registerUser userName =
    try
        let sendingJson = sprintf """{"userId": "%s"}""" userName
        let response = Http.Request(
                        "http://localhost:5000/api/registerUser",
                        httpMethod = "POST",
                        headers = [ ContentType HttpContentTypes.Json ],
                        body = TextRequest sendingJson
        )
        let r1 = response.Body
        let response1 =
            match r1 with
            | Text a -> a
            | Binary b -> System.Text.ASCIIEncoding.ASCII.GetString b
        let msg = sprintf "User %s registered." userName
        printMessage msg
        response1
    with
    | _ -> 
        let msg = sprintf "The usename %s already exists" userName
        printMessage msg
        ""

/// Function to post the tweet
let postTweet userName tweet=
    try
        let sendingJson = sprintf """{"userId": "%s", "tweet":"%s"}""" userName tweet
        let response = Http.Request(
                        "http://localhost:5000/api/tweet",
                        httpMethod = "POST",
                        headers = [ ContentType HttpContentTypes.Json ],
                        body = TextRequest sendingJson
        )
        let r1 = response.Body
        let response1 =
            match r1 with
            | Text a -> a
            | Binary b -> System.Text.ASCIIEncoding.ASCII.GetString b
        let msg = "Tweet posted successfully."
        printMessage msg
        response1
    with
    | _ -> 
        let msg = "Please check the userid"
        printMessage msg
        ""
let getTheTweet (tweet:string) =
    let arr = tweet.Split('~') |> Array.toList
    let x = arr.Length
    arr.[x-1]


/// Post retweet
let postRetweet (feed: list<string>) =
    let mutable idx = 1
    printfn "\n    ***********************************"
    printfn "Tweet ID  -  Tweet"
    for i in feed do 
        printfn "%d - %s" idx (getTheTweet i)
        idx <- idx+1
    printfn "    ***********************************\n"
    let sz = feed.Length
    let mutable tweetIdNum = sz+1
    while tweetIdNum > sz do 
        printfn "Enter the tweet ID to retweet: "
        let tweetId = Console.ReadLine() 
        tweetIdNum <- tweetId |> int
        if tweetIdNum > sz then
            let msg = "The tweet ID you entered is invalid\n"
            printMessage msg
    let mutable reTweet = feed.[tweetIdNum-1]
    reTweet<-reTweet.Replace("\"", "")
    postTweet USERNAME reTweet

/// Get tweets hashtags without '#'
let getTweetsWithHashTag hashtagToRequest =
        let url = "http://localhost:5000/api/hashtags/"+hashtagToRequest
        let a = FSharp.Data.JsonValue.Load url
        let tweetList = a.GetProperty("tweets")
        let mutable len=0
        let mutable tweetStrList:list<string> = []
        for i in tweetList do
            len<-len+1
        if len > 0 then
            printfn "\n    ***********************************"
            printfn "        Tweets with #%s found: " hashtagToRequest
            printfn "    ***********************************"
            for i in tweetList do
                let str = i.ToString()
                printTweet str
                tweetStrList <- tweetStrList @ [str]
            printfn "    ***********************************\n"
            printfn "Do you wish to retweet any of these tweets? (y/n)"
            let reply = Console.ReadLine()
            let x = reply.ToString()
            if x = "y" then
                postRetweet tweetStrList |> ignore
                ()
        else 
            let msg = sprintf "No tweets with #%s." hashtagToRequest
            printMessage msg


/// Get tweets from subscriptions
let getTweetsFromSubscription userId =
    // try
        let url = "http://localhost:5000/api/tweet/"+userId
        let a = FSharp.Data.JsonValue.Load url
        let tweetList = a.GetProperty("feed")
        let mutable len=0
        let mutable tweetStrList:list<string> = []
        for i in tweetList do
            len<-len+1
        if len > 0 then
            printfn "\n    ***********************************"
            printfn "        Tweets from users you are subscribed to: "
            printfn "    ***********************************"
            for i in tweetList do
                let str = i.ToString()
                printTweet str
                tweetStrList <- tweetStrList @ [str]
            printfn "    ***********************************\n"
            printfn "Do you wish to retweet any of these tweets? (y/n)"
            let reply = Console.ReadLine()
            let x = reply.ToString()
            if x = "y" then
                postRetweet tweetStrList |> ignore
                ()
        else 
            let msg = "No tweets from users you are subscribed to."
            printMessage msg



/// Get user mentions 
let getMentions mentionedUser =
    try
        let url = "http://localhost:5000/api/mentions/"+mentionedUser
        let a = FSharp.Data.JsonValue.Load url
        let tweetList = a.GetProperty("tweets")
        let mutable len=0
        let mutable tweetStrList:list<string> = []
        for i in tweetList do
            len<-len+1
        if len > 0 then
            printfn "\n    ***********************************"
            printfn "        Tweets with %s mentions found: " mentionedUser
            printfn "    ***********************************"
            for i in tweetList do
                let str = i.ToString()
                printTweet str
                tweetStrList <- tweetStrList @ [str]
            printfn "    ***********************************\n"
            printfn "Do you wish to retweet any of these tweets? (y/n)"
            let reply = Console.ReadLine()
            let x = reply.ToString()
            if x = "y" then
                postRetweet tweetStrList |> ignore
                ()
        else 
            let msg = "You don't have any mentions."
            printMessage msg
            
    with
    | _ -> 
        let msg = sprintf "No tweets with %s mentions found" mentionedUser
        printMessage msg



/// Post a follow request from user to leader 
let startFollowing userId leaderId =
    try
        let sendingJson = sprintf """{"userId": "%s", "leaderId":"%s"}""" userId leaderId
        let response = Http.Request(
                        "http://localhost:5000/api/subscribe",
                        httpMethod = "POST",
                        headers = [ ContentType HttpContentTypes.Json ],
                        body = TextRequest sendingJson
        )
        let r1 = response.Body
        let response1 =
            match r1 with
            | Text a -> a
            | Binary b -> System.Text.ASCIIEncoding.ASCII.GetString b
        let msg = sprintf "%s now following %s" userId leaderId
        printMessage msg      
        response1
    with
    | _ -> 
        let msg = "Please check the userId you wish to subscribe to"
        printMessage msg
        ""

/// Login user
let loginUser userId =
    try
        let response = Http.Request(
                        "http://localhost:5000/api/login/"+userId,
                        httpMethod = "POST",
                        headers = [ ContentType HttpContentTypes.Json ],
                        body = TextRequest ""
        )
        let r1 = response.Body
        let response1 =
            match r1 with
            | Text a -> a
            | Binary b -> System.Text.ASCIIEncoding.ASCII.GetString b
        printfn "\n    ***********************************"
        printfn "            %s logged in" userId 
        printfn "    ***********************************"  
        USERNAME <- userId
        response1
    with
    | _ -> 
        let msg = "Please check the userId"
        printMessage msg
        ""

/// Logout user
let logoutUser userId =
    try
        let response = Http.Request(
                        "http://localhost:5000/api/logout/"+userId,
                        httpMethod = "POST",
                        headers = [ ContentType HttpContentTypes.Json ],
                        body = TextRequest ""
        )
        let r1 = response.Body
        let response1 =
            match r1 with
            | Text a -> a
            | Binary b -> System.Text.ASCIIEncoding.ASCII.GetString b
        printfn "\n    ***********************************"
        printfn "            %s logged out" userId
        printfn "    ***********************************"       
        response1
    with
    | _ -> 
        let msg = "Please check the userId"
        printMessage msg
        ""

let getFeed requestedUser =
    try
        let url = "http://localhost:5000/api/feed/"+requestedUser
        let a = FSharp.Data.JsonValue.Load url
        let c = a.GetProperty("feed")
        let mutable len=0
        for i in c do
            len<-len+1
        if len > 0 then
            printfn "\n    ***********************************"
            printfn "        You have a new tweet! "
            printfn "    ***********************************"
            for i in c do
                let r = i.ToString()
                feed <- feed @ [r]
                printTweet (r)
            printfn "    ***********************************\n"

    with 
    | _ -> ()

let system = ActorSystem.Create("pollerSystem")
let poller =
    spawn system "poller"
    <| fun mailbox ->
            mailbox.Self <! "poll"
            let rec loop() =
                actor {
                    let! message = mailbox.Receive()
                    if USERNAME.Length > 0 then
                        getFeed USERNAME
                    Thread.Sleep 100
                    mailbox.Self <! "poll"
                    return! loop()
                }
            loop()

let rec loop () = 
    printfn "\n1. Register User\n2. Login User\n3. Tweet\n4. Follow another user \n5. Query Hashtag\n6. Get tweets with my mentions\n7. Get tweets from my subscription\n8. Show Feed\n9. Logout\n10. Exit\n"
    let option = Console.ReadLine() |> int
    
    if option = 1 then
        printfn "Enter username:"
        let uname = Console.ReadLine()
        registerUser uname |> ignore
        Thread.Sleep 500

    else if option = 2 then
        if USERNAME.Length>0 then
            let msg = "You are already logged in."
            printMessage msg
            ()
        else
            printfn "Enter username to login-"
            let uname = Console.ReadLine()

            loginUser uname |> ignore
            ()

    else if option = 3 then
        if USERNAME.Length>0 then
            printfn "Enter tweet to be posted:"
            let tweet = Console.ReadLine()

            postTweet USERNAME tweet |> ignore
            ()
        else
            let msg = "Please log in."
            printMessage msg
        ()

    else if option = 4 then
        if USERNAME.Length>0 then
            printfn "Enter username to be followed"
            let leader = Console.ReadLine()
            startFollowing USERNAME leader |> ignore
            ()
        else
            let msg = "Please log in."
            printMessage msg
        ()

    else if option = 5 then
        if USERNAME.Length>0 then
            printfn "Enter hashtag to be quried without #"
            let tag = Console.ReadLine()
            let x = String.length tag 
            let qTag = tag.[0..x-1]
            getTweetsWithHashTag qTag
            ()
        else
            let msg = "Please log in."
            printMessage msg
        ()

    else if option = 6 then
        if USERNAME.Length>0 then
            getMentions USERNAME
            ()
        else
            let msg = "Please log in."
            printMessage msg
        ()
    
    else if option = 7 then
        // Get tweets from subscriotpn
        if USERNAME.Length>0 then
            getTweetsFromSubscription USERNAME
            ()
        else
            let msg = "Please log in."
            printMessage msg
        ()

    else if option = 8 then
        if USERNAME.Length>0 then
            printfn "    ***********************************"
            printfn "\n          Here is your feed"
            printfn "    ***********************************"
            for i in feed do
                printTweet i
            printfn "    ***********************************\n"
            printfn "Do you wish to retweet any of these tweets? (y/n)"
            let reply = Console.ReadLine()
            let x = reply.ToString()
            if x = "y" then
                postRetweet feed |> ignore
                ()
            ()
        else
            let msg = "Please log in."
            printMessage msg
            ()
        ()

    else if option = 9 then
        if USERNAME.Length > 0 then
            logoutUser USERNAME |> ignore
            USERNAME <- ""
        else
            let msg = "You are not logged in"
            printMessage msg
        ()

    else if option = 10 then
        system.Terminate() |> ignore
        exit 0
        
    loop()
loop()

Thread.Sleep 5000000
Console.ReadLine |> ignore