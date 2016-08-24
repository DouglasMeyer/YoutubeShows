port module YoutubeShows exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Lazy exposing (lazy2)
import Time exposing (Time)
import Dict exposing (Dict)
import Task
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
  , lastChannelFetch : Time
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
  , lastChannelFetch = 0
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
      } ! [ Time.now
        |> Task.perform (\now -> NoOp) (\now -> AuthWithToken stored.authorization now)
      ]


-- UPDATE

type Msg
  = NoOp
  | Tick Time
  | AuthWithToken (Maybe Authorization) Time
  | SetAuthorization ( Maybe Authorization )
  | AddSubscribedChannels ( List Channel )
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
            , timeTillNextVideosFetch = timeTillNextVideosFetch
          } ! [ requestYTSubscribedChannels () ]
        else if isAuthorized && timeTillNextVideosFetch <= 0 then
          let
            channelPlaylistIds = model.channels
              |> Dict.values
              |> List.map .uploadPlaylist
          in
            { model
              | lastVideosFetch = now
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

    AddSubscribedChannels channels ->
      { model
        | channels = addChannelsToChannels channels model.channels
        , lastVideosFetch = 0
      } ! []

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
port channelVideos : (List Video -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every Time.second Tick
    , gotAuthorization (\authorization -> SetAuthorization (Just authorization))
    , failedAuthorization (\_ -> SetAuthorization Nothing)
    , subscribedChannels AddSubscribedChannels
    , channelVideos AddVideos
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
      [ class "vertical-layout" ]
      [ viewHeader model
      , div
        [ class "expand-layout horizontal-layout scroll-vertically"
        ]
        [ lazy2 viewChannelList model.channelFilter filteredChannels
        , lazy2 viewVideosList filteredChannels model.videoFilter
        ]
      , footer [] [ text "by Douglas Meyer" ]
      ]

viewHeader : Model -> Html Msg
viewHeader model =
  div
    [ class "horizontal-layout do-not-shrink" ]
    [ header [] [ text "YoutubeShows" ]
    , div [ class "expanded-layout align-text-center" ] [ text ("Next fetch " ++ toString (round (model.timeTillNextVideosFetch / 1000))) ]
    , div [ class "expand-layout align-text-right" ] [ text (if model.authorization /= Nothing then "Authorized" else "Not Authorized") ]
    ]

viewChannelList : String -> List Channel -> Html Msg
viewChannelList channelFilter channels =
  ul [ class "no-list horizontal-margin" ] <|
    ( li
      [ class "vertical-margin" ]
      [ input
        [ type' "search"
        , placeholder "channel filter"
        , onInput FilterChannels
        , value channelFilter
        ]
        []
      ]
    ) :: List.map
      (\channel ->
        li
          [ onClick (FilterChannels channel.title), class (if channel.title == channelFilter then "selected vertical-margin" else "clickable vertical-margin")
          , style [("font-size", "0")]
          ]
          [ img [ src channel.thumbnailUrl, title channel.title, alt channel.title, width 88, height 88 ] []
          , div [ style [("font-size", "1rem")] ] [ text channel.title ]
          ]
      )
      channels

viewVideosList : List Channel -> String -> Html Msg
viewVideosList channels videoFilter =
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
        [ class "vertical-margin" ]
        [ input
          [ type' "search"
          , placeholder "video filter"
          , onInput FilterVideos
          , value videoFilter
          ]
          []
        ]
      ) :: List.map
        (\video ->
          let
            defaultChannelImgAttrs = [ width 88, height 88 ]
            channelImgAttrs = case findById video.channelId channels of
              Nothing ->
                src "//s.ytimg.com/yts/img/avatar_720-vflYJnzBZ.png" :: class "horizontal-margin" :: defaultChannelImgAttrs -- FIXME: need a better way to get default channel
              Just channel ->
                src channel.thumbnailUrl :: onClick (FilterChannels channel.title) :: class "horizontal-margin clickable" :: alt channel.title :: defaultChannelImgAttrs
          in
            li
              [ class "horizontal-layout vertical-margin" ]
              [ img channelImgAttrs []
              , img [ src video.thumbnailUrl, width 120, height 90, class "horizontal-margin" ] []
              , h3 [ class "horizontal-margin" ] [ text video.title ]
              ]
        )
        videos

findById : String -> List { a | id : String } -> Maybe { a | id : String }
findById id list =
  list
    |> List.filter (\item -> item.id == id)
    |> List.head
