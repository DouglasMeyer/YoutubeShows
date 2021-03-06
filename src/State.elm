port module State exposing
  ( init, update, subscriptions
  , indexToTab, tabToIndex
  , Msg(..), setStorage
  )

import Types exposing (Model, StoredState, Channel, Video, findById)
import Auth exposing (authorize)
import Notification exposing (notify, requestPermission)

import Array
import Dict exposing (Dict)
import Time exposing (Time)
import Material
import Material.Layout

init : Maybe StoredState -> ( Model, Cmd Msg )
init maybeStartingState =
  let
    (authModel, authCmd) = Auth.init <| maybeStartingState `Maybe.andThen` (.authorization >> Just)
    (notificationModel, notificationCmd) = Notification.init
    model =
      { authorization = authModel
      , notification = notificationModel
      , mdl = Material.Layout.setTabsWidth 100 Material.model
      , isFetchingChannels = False
      , lastChannelFetch = 0
      , isFetchingVideos = False
      , lastVideosFetch = 0
      , timeTillNextVideosFetch = 0
      , channels = case maybeStartingState of
        Nothing -> Dict.empty
        Just startingState ->
          addChannelsToChannels startingState.channels Dict.empty
      , channelFilter = ""
      , videoFilter = ""
      , selectedTab = "main"
      , newVideos = []
      }
  in
    model !
    [ Material.init Mdl
    , Cmd.map AuthMsg authCmd
    , Cmd.map NotificationMsg notificationCmd
    ]

tabs : Array.Array String
tabs = Array.fromList [ "main", "about", "permissions" ]

indexToTab : Int -> String
indexToTab index =
  Array.get index tabs
  |> Maybe.withDefault "main"

tabToIndex : String -> Int
tabToIndex tab =
  tabs
    |> Array.toIndexedList
    |> List.filterMap (\(i,t) -> if t == tab then Just i else Nothing)
    |> List.head
    |> Maybe.withDefault 0

type Msg
  = Mdl (Material.Msg Msg)
  | AuthMsg Auth.Msg
  | NotificationMsg Notification.Msg
  | GetAuthorization Bool
  | GetNotificationPermission
  | Tick Time
  | ChannelFetchComplete
  | AddSubscribedChannels ( List Channel )
  | VideoFetchComplete
  | AddVideos ( List Video )
  | FilterChannels String
  | FilterVideos String
  | SelectTab String

-- Outgoing
port setStorage : StoredState -> Cmd msg
port requestYTSubscribedChannels : () -> Cmd msg
port requestYTVideos : List String -> Cmd msg
-- Incoming
port subscribedChannels : (List Channel -> msg) -> Sub msg
port channelFetchComplete : (() -> msg) -> Sub msg
port channelVideos : (List Video -> msg) -> Sub msg
port videoFetchComplete : (() -> msg) -> Sub msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Mdl msg' -> Material.update msg' model
    AuthMsg authMsg ->
      let
        (authModel, authCmd) = Auth.update authMsg model.authorization
      in
        { model | authorization = authModel
        } ! [ Cmd.map AuthMsg authCmd ]
    NotificationMsg notificationMsg ->
      let
        (notificationModel, notificationCmd) = Notification.update notificationMsg model.notification
      in
        { model | notification = notificationModel
        } ! [ Cmd.map NotificationMsg notificationCmd ]

    GetAuthorization immediate ->
      model ! [ Cmd.map AuthMsg <| authorize (model.authorization, immediate) ]

    GetNotificationPermission ->
      model ! [ Cmd.map NotificationMsg <| requestPermission () ]

    Tick now ->
      let
        isAuthorized = model.authorization.isAuthorized
        timeTillNextVideosFetch = (model.lastVideosFetch + Time.minute * 20) - now
      in
        if isAuthorized && (now - model.lastChannelFetch) > Time.hour * 16 then
          { model
            | lastChannelFetch = now
            , isFetchingChannels = True
            , timeTillNextVideosFetch = timeTillNextVideosFetch
          } ! [ requestYTSubscribedChannels () ]
        else if isAuthorized && not model.isFetchingChannels && timeTillNextVideosFetch <= 0 then
          let
            channelPlaylistIds = model.channels
              |> Dict.values
              |> List.map .uploadPlaylist
          in
            { model
              | lastVideosFetch = now
              , isFetchingVideos = True
              , timeTillNextVideosFetch = timeTillNextVideosFetch
            } ! [ requestYTVideos channelPlaylistIds ]
        else
          { model
          | timeTillNextVideosFetch = timeTillNextVideosFetch
          } ! []

    ChannelFetchComplete ->
      { model | isFetchingChannels = False } ! []

    AddSubscribedChannels channels ->
      { model
        | channels = addChannelsToChannels channels model.channels
        , lastVideosFetch = 0
      } ! []

    VideoFetchComplete ->
      { model | isFetchingVideos = False
      } ! (if List.length model.newVideos == 0 then
        []
      else
        [ notify
          ( (toString <| List.length model.newVideos) ++ " new videos"
          , List.foldl
            (\video message ->
              message ++ " " ++ video.title ++ "."
            )
            "New videos:"
            model.newVideos
          , "newVideos"
          )
        ]
      )

    AddVideos videos ->
      let
        newVideos =
          List.filter
            (\video ->
              case Dict.get video.channelId model.channels of
                Nothing -> True
                Just channel -> (findById video.id channel.videos) == Nothing
            ) videos
          ++ model.newVideos
      in
        { model
          | channels = addVideosToChannels videos model.channels
          , newVideos = newVideos
        } ! []

    FilterChannels channelFilter ->
      { model | channelFilter = channelFilter } ! []

    FilterVideos videoFilter ->
      { model | videoFilter = videoFilter } ! []

    SelectTab name ->
      { model
      | selectedTab = name
      , newVideos = if name == "main" then [] else model.newVideos
      } ! []


addChannelsToChannels : List Channel -> Dict String Channel -> Dict String Channel
addChannelsToChannels newChannels channels =
  List.foldl
    (\newChannel channels -> case Dict.get newChannel.id channels of
      Nothing -> Dict.insert newChannel.id newChannel channels
      Just existingChannel ->
        Dict.insert
          newChannel.id
          { newChannel | videos = existingChannel.videos }
          channels
    )
    channels
    newChannels

addVideosToChannels : List Video -> Dict String Channel -> Dict String Channel
addVideosToChannels videos channels =
  List.foldl
    (\newVideo channels -> case Dict.get newVideo.channelId channels of
      Nothing -> channels
      Just channel ->
        Dict.insert
          newVideo.channelId
          { channel
            | videos = case findById newVideo.id channel.videos of
              Nothing -> newVideo :: channel.videos
              Just _ ->
                List.map
                  (\video -> if video.id == newVideo.id then newVideo else video)
                  channel.videos
          }
          channels
    )
    channels
    videos

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map AuthMsg <| Auth.subscriptions model.authorization
    , Sub.map NotificationMsg <| Notification.subscriptions model.notification
    , Time.every Time.second Tick
    , subscribedChannels AddSubscribedChannels
    , channelFetchComplete (\_ -> ChannelFetchComplete)
    , channelVideos AddVideos
    , videoFetchComplete (\_ -> VideoFetchComplete)
    ]
