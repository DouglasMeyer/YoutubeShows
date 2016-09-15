port module State exposing
  ( init, update, subscriptions
  , indexToTab, tabToIndex
  , Msg(..), setStorage
  )

import Types exposing (Model, StoredState, Authorization, Channel, Video, findById)

import Array
import Dict exposing (Dict)
import Time exposing (Time)
import Material

init : Maybe StoredState -> ( Model, Cmd Msg )
init startingState =
  case startingState of
    Nothing ->
      emptyModel ! []

    Just stored ->
      { emptyModel
        | authorization = Nothing
        , channels = addChannelsToChannels stored.channels emptyModel.channels
      } ! []

emptyModel : Model
emptyModel =
  { authorization = Nothing
  , lastAuthCheck = 0
  , isFetchingChannels = False
  , lastChannelFetch = 0
  , isFetchingVideos = False
  , lastVideosFetch = 0
  , timeTillNextVideosFetch = 0
  , channels = Dict.empty
  , channelFilter = ""
  , videoFilter = ""
  , mdl = Material.model
  , selectedTab = "main"
  }

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
  | Tick Time
  | AuthWithToken (Maybe Authorization) Time
  | SetAuthorization ( Maybe Authorization )
  | ChannelFetchComplete
  | AddSubscribedChannels ( List Channel )
  | VideoFetchComplete
  | AddVideos ( List Video )
  | FilterChannels String
  | FilterVideos String
  | SelectTab String

-- Outgoing
port setStorage : StoredState -> Cmd msg
port authorize : (Maybe Authorization) -> Cmd msg
port requestYTSubscribedChannels : () -> Cmd msg
port requestYTVideos : List String -> Cmd msg
-- Incoming
port gotAuthorization : (Authorization -> msg) -> Sub msg
port failedAuthorization : (() -> msg) -> Sub msg
port subscribedChannels : (List Channel -> msg) -> Sub msg
port channelFetchComplete : (() -> msg) -> Sub msg
port channelVideos : (List Video -> msg) -> Sub msg
port videoFetchComplete : (() -> msg) -> Sub msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Mdl msg' ->
      Material.update msg' model

    Tick now ->
      let
        isAuthorized = model.authorization /= Nothing
        timeTillNextVideosFetch = (model.lastVideosFetch + Time.minute * 20) - now
      in
        if not isAuthorized && (now - model.lastAuthCheck) > Time.minute then
          { model
            | lastAuthCheck = now
            , timeTillNextVideosFetch = timeTillNextVideosFetch
          } ! [ authorize model.authorization ]
        else if isAuthorized && (now - model.lastChannelFetch) > Time.hour * 16 then
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

    AuthWithToken authorization now ->
      { model
        | lastAuthCheck = now
      } ! [ authorize authorization ]

    SetAuthorization authorization ->
      { model | authorization = authorization } ! []

    ChannelFetchComplete ->
      { model | isFetchingChannels = False } ! []

    AddSubscribedChannels channels ->
      { model
        | channels = addChannelsToChannels channels model.channels
        , lastVideosFetch = 0
      } ! []

    VideoFetchComplete ->
      { model | isFetchingVideos = False } ! []

    AddVideos videos ->
      { model
        | channels = addVideosToChannels videos model.channels
      } ! []

    FilterChannels channelFilter ->
      { model | channelFilter = channelFilter } ! []

    FilterVideos videoFilter ->
      { model | videoFilter = videoFilter } ! []

    SelectTab name -> { model | selectedTab = name } ! []


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
    [ Time.every Time.second Tick
    , gotAuthorization (\authorization -> SetAuthorization (Just authorization))
    , failedAuthorization (\_ -> SetAuthorization Nothing)
    , subscribedChannels AddSubscribedChannels
    , channelFetchComplete (\_ -> ChannelFetchComplete)
    , channelVideos AddVideos
    , videoFetchComplete (\_ -> VideoFetchComplete)
    ]
