module PeopleApi.App

open System
open System.Collections.Generic
open WebSharper
open WebSharper.Sitelets

/// The types used by this application.
module Model =

    /// Data type for registering a user
    type UserData = { userId: string }

    /// Response type for sending user's mentions
    type UsersMentions =
        { userId: string
          tweets: list<string> }

    /// Response type for hashtag tweets
    type hashTags =
        { hashtag: string
          tweets: list<string> }

    /// request response model to post tweet
    type TweetModel =
        { userId: string
          tweet: string }

    /// Model for subcribe/follow request. NOTE: same model is used for response for now
    type SubscribeData = { userId: string; leaderId: string }

    /// Model for feed response
    type FeedModel =
        {
            userId: string
            feed: list<string>
        }

    type ApiEndPoint =


        /// Accepts GET requests to /mentions/{userid}
        | [<EndPoint "GET /mentions">] GetMentions of userName: string

        /// Accepts POST requests to /login/{userid}
        | [<EndPoint "POST /login">] LoginUser of userName: string

        /// Accepts POST requests to /logout/{userid}
        | [<EndPoint "POST /logout">] LogoutUser of userName: string

        /// Accepts GET requests to /hashtags/{tag}
        | [<EndPoint "GET /hashtags">] GetHashTags of hashtag: string

        /// Accepts POST requests to /tweet with TweetModel as JSON body
        | [<EndPoint "POST /tweet"; Json "tweetModel">] PostTweet of tweetModel: TweetModel

        /// Accepts GET request to /tweet/{userid} and responds with tweets of all users that are followed by userId
        | [<EndPoint "GET /tweet">] GetTweets of userName: string

        /// Accepts POST requests to /registerUser UserData as JSON body
        | [<EndPoint "POST /registerUser"; Json "userData">] CreateUser of userData: UserData

        /// Accepts POST requests to /subscribe SubscribeData as JSON body
        | [<EndPoint "POST /subscribe"; Json "subscribeData">] SubscribeUser of subscribeData: SubscribeData

        /// Accepts GET request to /feed/{userid}
        | [<EndPoint "GET /feed">] GetFeed of userName: string

    /// The type of all endpoints for the application.
    type EndPoint =

        /// Accepts requests to /
        | [<EndPoint "/">] Home

        /// Accepts requests to /api/...
        | [<EndPoint "/api">] Api of Cors<ApiEndPoint>

    /// Error result value.
    type Error = { error: string }

    type ApiResult<'T> = Result<'T, Http.Status * Error>

    /// Result value for CreatePerson.
    type Id = { id: int }

    type Resp = { resp: string }

open Model

module Backend =

    // Data structures
    let mutable requestCounter: int = 0 // to count he number of requests received
    let mutable allTweetsMap: Map<string, list<string>> = Map.empty // map to store all tweets
    let mutable hashTagMap: Map<string, list<string>> = Map.empty
    let mutable mentionsMap: Map<string, list<string>> = Map.empty
    let mutable feedMap: Map<string, list<string>> = Map.empty
    let mutable followerMap: Map<string, list<string>> = Map.empty
    let mutable followingMap: Map<string, list<string>> = Map.empty
    let mutable usersSet: Set<string> = Set.empty
    let mutable isUserOnlineMap: Map<string, bool> = Map.empty
    let mutable counterMap: Map<string, int> = Map.empty
    let mutable totalTweets=0
    let mutable onlineUsers = 0
    let mutable offlineUsers = 0
    let mutable totalUsers = 0

    let init =
        counterMap <- counterMap.Add("totalUsers", 0)
        counterMap <- counterMap.Add("onlineUsers", 0)
        counterMap <- counterMap.Add("offlineUsers", 0)

    //helper methods

    let updateCounter (key: string) (increment: int) =
        if(key = "totalUsers") then totalUsers <- totalUsers+increment
        else if (key = "offlineUsers") then offlineUsers <- offlineUsers + increment
        else if (key = "onlineUsers") then onlineUsers <- onlineUsers + increment

    let createUser (userId: string) =
        usersSet <- usersSet.Add(userId)
        isUserOnlineMap <- isUserOnlineMap.Add(userId, true)
        mentionsMap <- mentionsMap.Add(userId, List.empty)
        mentionsMap <- mentionsMap.Add(userId, List.empty)
        followingMap <- followingMap.Add(userId, List.empty)
        followerMap <- followerMap.Add(userId, List.empty)
        updateCounter "totalUsers" 1
        updateCounter "onlineUsers" 1
        feedMap <- feedMap.Add(userId, List.empty)

    let addFollowerAndLeader (userId: string) (leaderId: string) =
        // check if both users exist
        if ((usersSet.Contains(userId))
            && (usersSet.Contains(leaderId))) then
            // userId added into leader's followers
            let found = followerMap.TryFind leaderId
            match found with
            | Some currentList ->
                let mutable temp = currentList
                temp <- temp @ [ userId ]
                followerMap <- followerMap.Add(leaderId, temp)
                ()
            | None -> printfn "leaderId not found in follower Map @ 126"

            // Adding leaderId into userId's following list
            let found = followingMap.TryFind userId
            match found with
            | Some currentList ->
                let mutable temp1 = currentList
                temp1 <- temp1 @ [ leaderId ]
                followingMap <- followingMap.Add(userId, temp1)
                ()
            | None -> printfn "userId not found in following Map @ 138"
            true
        else
            false

    let sendTweetToUser (tweet: string) (senderUser: string) (toUser: string) =
        totalTweets <- totalTweets+1
        let found = feedMap.TryFind toUser
        match found with
        | Some currentToUserFeed ->
            let hybridTweet = senderUser + "~" + tweet
            let newToUserFeed = currentToUserFeed @ [ hybridTweet ]
            feedMap <- feedMap.Add(toUser, newToUserFeed)
        | None -> printfn "\nLOG: toUser: %s not found in feedMap" toUser

    let saveTweet (tweet: string) (senderUser: string) =
        let hybridTweet = senderUser + "~" + tweet
        let found = allTweetsMap.TryFind senderUser
        match found with
        | Some allTweets ->
            let newTweetList = allTweets @ [ hybridTweet ]
            allTweetsMap <- allTweetsMap.Add(senderUser, newTweetList)
        | None ->
            let newTweetList = [ hybridTweet ]
            allTweetsMap <- allTweetsMap.Add(senderUser, newTweetList)

    let updateUserStatus (userId: string) (newStatus: bool) =
        let found = isUserOnlineMap.TryFind userId
        match found with
        | Some currStatus ->
            if currStatus <> newStatus then
                if newStatus then // offline -> online
                    updateCounter "onlineUsers" 1
                    updateCounter "offlineUsers" -1
                else // online -> offline
                    updateCounter "onlineUsers" -1
                    updateCounter "offlineUsers" 1
                isUserOnlineMap <- isUserOnlineMap.Add(userId, newStatus)
            ()
        | None -> printfn "user not found in isOnlineMap at line 87"

    let getHashTags (tweet: string) =
        let arr = tweet.Split(' ') |> Array.toList
        let isHT (string1: string) = (string1.[0] = '#')

        let hashTagList =
            List.choose (fun elem ->
                match elem with
                | elem when isHT elem -> Some(elem)
                | _ -> None) arr

        hashTagList

    let getMentions (tweet: string) =
        let arr = tweet.Split(' ') |> Array.toList
        let isMention (string1: string) = (string1.[0] = '@')

        let mentionsList =
            List.choose (fun elem ->
                let x = String.length elem
                match elem with
                | elem when isMention elem -> Some(elem.[1..x - 1])
                | _ -> None) arr

        mentionsList

    let saveHashTags (hashtagsList: list<string>) (tweet: string) (senderUser: string) =
        let hybridTweet = senderUser + "~" + tweet
        for tag in hashtagsList do
            let found = hashTagMap.TryFind tag
            match found with
            | Some currentList ->
                let newList = currentList @ [ hybridTweet ]
                hashTagMap <- hashTagMap.Add(tag, newList)
                ()
            | None ->
                let newList = [ hybridTweet ]
                hashTagMap <- hashTagMap.Add(tag, newList)

    let saveMentions (mentionList: list<string>) (tweet: string) (senderUser: string) =
        let hybridTweet = senderUser + "~" + tweet
        for oneMention in mentionList do
            if (usersSet.Contains oneMention) then // check if the parsed mention is a valid userID
                sendTweetToUser tweet senderUser oneMention
                let found = mentionsMap.TryFind oneMention
                match found with
                | Some currentList ->
                    let newList = currentList @ [ hybridTweet ]
                    mentionsMap <- mentionsMap.Add(oneMention, newList)
                    ()
                | None ->
                    let newList = [ hybridTweet ]
                    mentionsMap <- mentionsMap.Add(oneMention, newList)

    // request fullfilment functions
    let GetHashTags (hashtag: string): ApiResult<hashTags> =
        printfn "\nLOG: GetHashTags triggered hahstag: %A" hashtag
        let mutable queriedTag = hashtag
        if(hashtag.[0] <> '#') then
            queriedTag <- sprintf "#%s" hashtag

        let found = hashTagMap.TryFind queriedTag
        match found with
        | Some hashtagTweets ->
            Ok { hashtag = hashtag; tweets = hashtagTweets }
        | None -> 
            let emptyHashTagList = List.Empty
            Ok { hashtag = hashtag; tweets = emptyHashTagList }

    let GetFeed (userName: string): ApiResult<FeedModel> =
        let found = feedMap.TryFind userName
        match found with
        | Some userFeed ->
            if userFeed.Length > 0 then
                printfn "\nLOG: Pushing feed to user: %A" userName
            feedMap <- feedMap.Add(userName, List.Empty)
            Ok { userId = userName; feed = userFeed }
        | None -> 
            let emptyFeedList = List.Empty
            Ok { userId = userName; feed = emptyFeedList }

    let GetMentions (userId: string): ApiResult<UsersMentions> =
        printfn "\nLOG: GetMentions Query for %A's mentions" userId
        let found = mentionsMap.TryFind userId
        match found with
        | Some mentionedList ->
            Ok { userId = userId; tweets = mentionedList }
        | None -> 
            printfn "\nLOG: no mentions found for %s" userId
            let err = sprintf "No mentios for user: %s %A" userId Http.Status.NotFound
            Error (Http.Status.NotFound, { error = err })

    let PostTweet (data: TweetModel): ApiResult<TweetModel> =
        printfn "\nLOG: tweet: %s posted by %s" data.tweet data.userId
        saveTweet data.tweet data.userId
        let hashTags = getHashTags data.tweet
        saveHashTags hashTags data.tweet data.userId
        let mentions = getMentions data.tweet
        saveMentions mentions data.tweet data.userId
        let found = followerMap.TryFind data.userId
        match found with
        | Some followersList ->
            for oneUser in followersList do
                sendTweetToUser data.tweet data.userId oneUser
        | None -> printfn "senderUser: %s not found in followerMap " data.userId
        Ok { userId = data.userId; tweet=data.tweet }

    let CreateUser (data: UserData): ApiResult<Resp> =
        printfn "\nLOG: User %A attempting to create an account." data.userId
        if usersSet.Contains(data.userId) then
            let err = sprintf "Username already exists."
            printfn "\nLOG: Username already exists. %A"data.userId
            Error (Http.Status.Forbidden, { error = err })
        else
            createUser data.userId
            printfn "\nLOG: Account for usename %A successfully created." data.userId
            Ok { resp = data.userId }

    let SubscribeUser (data: SubscribeData): ApiResult<SubscribeData> =
        printfn "\nLOG: Follow request by %s to follow %s" data.userId data.leaderId
        if (addFollowerAndLeader data.userId data.leaderId) then
            Ok { userId = data.userId; leaderId = data.leaderId }
        else
            let err = sprintf "Username does not exists. %A" Http.Status.Forbidden
            printfn "\nLOG: Username does not exists. %s %s" data.userId data.leaderId
            Error (Http.Status.Forbidden, { error = err })

    let LoginUser (userName: string): ApiResult<Resp> =
        if usersSet.Contains(userName) then
            printfn "\nLOG: logging in %A" userName
            updateUserStatus userName true
            Ok{resp= userName}
        else
            let err = sprintf "Username does not exists. %A" Http.Status.Forbidden
            printfn "\nLOG: Username does not exists. %s" userName
            Error (Http.Status.Forbidden, { error = err })

    let LogoutUser (userName: string): ApiResult<Resp> =
        if (usersSet.Contains(userName)) then
            printfn "\nLOG: logging out %A" userName
            updateUserStatus userName false
            Ok {resp = userName}
        else
            let err = sprintf "Username does not exists. %A" Http.Status.Forbidden
            printfn "\nLOG: Username does not exists. %s" userName
            Error (Http.Status.Forbidden, { error = err })

    let GetTweets (userName: string): ApiResult<FeedModel> =
        if usersSet.Contains(userName) then
            let followingList = followingMap.Item userName
            let mutable tweetList:list<string> = []
            for user in followingList do
                let found = allTweetsMap.TryFind user
                match found with
                | Some userTweets ->
                    tweetList <- tweetList @ userTweets
                | None -> printfn ""
            Ok {userId = userName; feed = tweetList}
        else
            let err = sprintf "Username does not exists. %A" Http.Status.Forbidden
            printfn "\nLOG: Username does not exists. %s" userName
            Error (Http.Status.Forbidden, { error = err })

/// The server side website, tying everything together.
module Site =
    open WebSharper.UI
    open WebSharper.UI.Html
    open WebSharper.UI.Server

    /// Helper function to convert our internal ApiResult type into WebSharper Content.
    let JsonContent (result: ApiResult<'T>): Async<Content<EndPoint>> =
        match result with
        | Ok value -> Content.Json value
        | Error (status, error) -> Content.Json error |> Content.SetStatus status
        |> Content.WithContentType "application/json"

    /// Respond to an ApiEndPoint by calling the corresponding backend function
    /// and converting the result into Content.
    let ApiContent (ep: ApiEndPoint): Async<Content<EndPoint>> =
        match ep with
        | CreateUser userData -> JsonContent(Backend.CreateUser userData)
        | PostTweet tweetModel -> JsonContent(Backend.PostTweet tweetModel)
        | SubscribeUser userData -> JsonContent(Backend.SubscribeUser userData)
        | GetMentions userid -> JsonContent(Backend.GetMentions userid)
        | GetHashTags hashtag -> JsonContent(Backend.GetHashTags hashtag)
        | GetFeed userid -> JsonContent(Backend.GetFeed userid)
        | LoginUser userid -> JsonContent(Backend.LoginUser userid)
        | LogoutUser userid -> JsonContent(Backend.LogoutUser userid)
        | GetTweets userid -> JsonContent(Backend.GetTweets userid)
   
    /// The Sitelet parses requests into EndPoint values
    /// and dispatches them to the content function.
    let Main corsAllowedOrigins: Sitelet<EndPoint> =
        Application.MultiPage(fun ctx endpoint ->
            match endpoint with
            | Api api ->
                Content.Cors api (fun allows ->
                    { allows with
                          Origins = corsAllowedOrigins
                          Headers = [ "Content-Type" ] }) ApiContent)
