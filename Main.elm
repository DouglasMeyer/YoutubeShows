port module YoutubeShows exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Lazy exposing (lazy2, lazy3)
import Time exposing (Time)
import Dict exposing (Dict)
import Regex

main : Program (Maybe StoredState)
main =
  App.programWithFlags
    { init = init
    , view = view
    , update = updateWithStorage
    , subscriptions = subscriptions
    }

port setStorage : StoredState -> Cmd msg
port authorize : (Maybe Authorization) -> Cmd msg
port requestYTSubscribedChannels : () -> Cmd msg
port requestYTVideos : List String -> Cmd msg

updateWithStorage : Msg -> Model -> (Model, Cmd Msg)
updateWithStorage msg model =
  let
    (newModel, cmds) = update msg model
  in
    case msg of
      SetAuthorization      _ -> doSetStorage newModel cmds
      AddSubscribedChannels _ -> doSetStorage newModel cmds
      AddVideos             _ -> doSetStorage newModel cmds
      _ -> ( newModel, cmds )

doSetStorage : Model -> Cmd Msg -> (Model, Cmd Msg)
doSetStorage model cmds =
  let
    storedState = StoredState
      model.authorization
      (Dict.values model.channels)
  in
    ( model
    , Cmd.batch [ setStorage storedState,  cmds ]
    )


-- MODEL
type alias Model =
  { authorization : Maybe Authorization
  , lastAuthCheck : Time
  , isFetchingChannels : Bool
  , lastChannelFetch : Time
  , isFetchingVideos : Bool
  , lastVideosFetch : Time
  , timeTillNextVideosFetch : Float
  , channels : Dict String Channel
  , channelFilter : String
  , videoFilter : String
  }

type alias Authorization =
  { token : String
  , expiresAt : Float
  }

type alias Channel =
  { id : String
  , title : String
  , thumbnailUrl : String
  , uploadPlaylist : String
  , videos : List Video
  , shows : List String
  }

type alias Video =
  { id : String
  , channelId : String
  , title : String
  , thumbnailUrl : String
  , publishedAt : String
  }

type alias StoredState =
  { authorization : Maybe Authorization
  , channels : List Channel
  }

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
  }

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


-- UPDATE

type Msg
  = NoOp
  | Tick Time
  | AuthWithToken (Maybe Authorization) Time
  | SetAuthorization ( Maybe Authorization )
  | ChannelFetchComplete
  | AddSubscribedChannels ( List Channel )
  | VideoFetchComplete
  | AddVideos ( List Video )
  | FilterChannels String
  | FilterVideos String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []

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


-- SUBSCRIPTIONS

port gotAuthorization : (Authorization -> msg) -> Sub msg
port failedAuthorization : (() -> msg) -> Sub msg

port subscribedChannels : (List Channel -> msg) -> Sub msg
port channelFetchComplete : (() -> msg) -> Sub msg
port channelVideos : (List Video -> msg) -> Sub msg
port videoFetchComplete : (() -> msg) -> Sub msg

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



-- VIEW

view : Model -> Html Msg
view model =
  let
    filterRegex = model.channelFilter |> Regex.regex |> Regex.caseInsensitive
    filteredChannels =
      model.channels
        |> Dict.values
        |> List.filter (\channel -> Regex.contains filterRegex channel.title)
  in
    div
      [ class "horizontal-layout"
      ]
      [ lazy2 viewChannelList model.channelFilter filteredChannels
      , lazy3
        viewVideosList
        filteredChannels
        ( model.isFetchingChannels || model.isFetchingVideos )
        model.videoFilter
      ]

viewChannelList : String -> List Channel -> Html Msg
viewChannelList channelFilter channels =
  ul [ class "no-list horizontal-margin" ] <|
    ( li
      []
      [ input
        [ type' "search"
        , placeholder "channel filter"
        , onInput FilterChannels
        , value channelFilter
        , style [ ("width", "100%") ]
        ]
        []
      ]
    ) :: List.map
      (\channel ->
        li
          [ onClick (FilterChannels channel.title), class (if channel.title == channelFilter then "selected vertical-margin" else "clickable vertical-margin")
          , style
            [ ("background", "url(" ++ channel.thumbnailUrl ++ ") no-repeat center / cover")
            , ("padding-top", "calc(88px - 1em)")
            , ("width", "10em")
            ]
          ]
          [ div
            [ style
              [ ("color", "#EEE")
              , ("background-image", "-webkit-linear-gradient(bottom, black, black, transparent)")
              , ("padding", "2px 5px")
              ]
            ]
            [ text channel.title ]
          ]
      )
      channels

viewVideosList : List Channel -> Bool -> String -> Html Msg
viewVideosList channels isFetching videoFilter =
  let
    filterRegex = videoFilter |> Regex.regex |> Regex.caseInsensitive
    videos = channels
      |> List.concatMap .videos
      |> List.filter (\video -> Regex.contains filterRegex video.title)
      |> List.sortBy .publishedAt
      |> List.reverse
  in
    ul [ class "expand-layout no-list horizontal-margin"] <|
      ( li
        []
        [ input
          [ type' "search"
          , placeholder "video filter"
          , onInput FilterVideos
          , value videoFilter
          ]
          []
        ]
      ) :: ( li
        [ style (if isFetching then [] else [("display","none")]) ]
        [ text "Checking for new videos" ]
      ) :: List.map (\v -> videoView v channels) videos

videoView : Video -> List Channel -> Html Msg
videoView video channels =
  let
    channel = case findById video.channelId channels of
      Nothing ->
        Channel "" "unknown channel" "//s.ytimg.com/yts/img/avatar_720-vflYJnzBZ.png" "" [] []
      Just channel ->
        channel
  in
    li
      [ class "vertical-margin"
      , style
        [ ("display", "flex")
        , ("flex-direction", "column")
        ]
      ]
      [ div
        [ style [ ("display", "flex"), ("align-items","center"), ("margin-bottom","3px") ]
        , onClick (FilterChannels channel.title)
        , class "clickable"
        ]
        [ img
          [ style
            [ ("height","1.2em")
            , ("margin-right","1ex")
            ]
          , src channel.thumbnailUrl
          , alt channel.title
          ] []
        , text channel.title
        ]
      , a
        [ style [ ("display", "flex"), ("align-items","center"), ("font-size","1.1rem"), ("font-weight","bold") ]
        , href ("http://youtube.com/watch?v=" ++ video.id)
        , target "_new"
        ]
        [ img
          [ width 120
          , height 90
          , style [ ("margin-right","1em") ]
          , src video.thumbnailUrl
          ]
          []
        , text video.title
        ]
      ]

findById : String -> List { a | id : String } -> Maybe { a | id : String }
findById id list =
  list
    |> List.filter (\item -> item.id == id)
    |> List.head
