port module YoutubeShows exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Lazy exposing (lazy2, lazy3)
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
      SelectChannel         _ -> doSetStorage newModel cmds
      _ -> ( newModel, cmds )

doSetStorage : Model -> Cmd Msg -> (Model, Cmd Msg)
doSetStorage model cmds =
  let
    storedState = StoredState
      model.authorization
      (Dict.values model.channels)
      model.selectedChannelId
      model.selectedShow
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
  , selectedChannelId : Maybe String
  , selectedShow : Maybe String
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
  , selectedChannelId : Maybe String
  , selectedShow : Maybe String
  }

emptyModel : Model
emptyModel =
  { authorization = Nothing
  , lastAuthCheck = 0
  , lastChannelFetch = 0
  , lastVideosFetch = 0
  , timeTillNextVideosFetch = 0
  , channels = Dict.empty
  , selectedChannelId = Nothing
  , selectedShow = Nothing
  }

init : Maybe StoredState -> ( Model, Cmd Msg )
init startingState =
  case startingState of
    Nothing ->
      emptyModel ! []

    Just stored ->
      { emptyModel
        | authorization = Nothing
        , selectedChannelId = stored.selectedChannelId
        , channels = addChannelsToChannels stored.channels emptyModel.channels
        , selectedShow = stored.selectedShow
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
  | SelectChannel ( Maybe String )
  | SelectShow ( Maybe String )

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

    SelectChannel channelId ->
      { model
        | selectedChannelId = channelId
        , selectedShow = Nothing
      } ! []

    SelectShow showName ->
      { model | selectedShow = showName } ! []


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
    channel : Maybe Channel
    channel = model.selectedChannelId `Maybe.andThen` (\channelId -> Dict.get channelId model.channels)
  in
    div
      [ class "vertical-layout" ]
      [ viewHeader model
      , div
        [ class "expand-layout horizontal-layout scroll-vertically"
        ]
        [ lazy2 viewChannelList model.selectedChannelId model.channels
        , lazy2 viewShowsList model.selectedShow channel
        , lazy3 viewVideosList model.selectedChannelId model.channels model.selectedShow
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

viewChannelList : Maybe String -> Dict String Channel -> Html Msg
viewChannelList selectedChannelId channels =
  ul [ class "no-list horizontal-margin" ] <|
    ( li
      [ onClick (SelectChannel Nothing), class (if Nothing == selectedChannelId then "selected vertical-margin" else "clickable vertical-margin") ]
      [ text "All Channels" ]
    ) :: List.map
      (\channel ->
        li
          [ onClick (SelectChannel (Just channel.id)), class (if Just channel.id == selectedChannelId then "selected vertical-margin" else "clickable vertical-margin")
          , style [("font-size", "0")]
          ]
          [ img [ src channel.thumbnailUrl, title channel.title, alt channel.title, width 88, height 88 ] []
          ]
      )
      (Dict.values channels)

viewShowsList : Maybe String -> Maybe Channel -> Html Msg
viewShowsList selectedShow channel =
  ul
    [ class "no-list horizontal-margin" ]
    [ li
        [ class "horizontal-layout" ]
        [ input
          [ type' "search", placeholder "new show name", onInput (\showName -> SelectShow <| Just showName ), value (Maybe.withDefault "" selectedShow) ]
          []
        ]
    ]

viewVideosList : Maybe String -> Dict String Channel -> Maybe String -> Html Msg
viewVideosList selectedChannelId channels selectedShow =
  let
    selectedChannels = case selectedChannelId `Maybe.andThen` (\channelId -> Dict.get channelId channels) of
      Nothing ->
        channels
          |> Dict.values
          |> List.concatMap .videos
      Just channel ->
        channel.videos
    videos = selectedChannels
      |> List.sortBy .publishedAt
      |> List.reverse
    filteredVideos = case selectedShow of
      Nothing -> videos
      Just showName -> List.filter (\v -> Regex.contains (Regex.regex showName) v.title) videos
  in
    ul [ class "expand-layout no-list horizontal-margin"]
      <| List.map
        (\video ->
          let
            defaultChannelImgAttrs = [ width 88, height 88 ]
            channelImgAttrs = case Dict.get video.channelId channels of
              Nothing ->
                src "//s.ytimg.com/yts/img/avatar_720-vflYJnzBZ.png" :: class "horizontal-margin" :: defaultChannelImgAttrs -- FIXME: need a better way to get default channel
              Just channel ->
                src channel.thumbnailUrl :: onClick (SelectChannel (Just channel.id)) :: class "horizontal-margin clickable" :: alt channel.title :: defaultChannelImgAttrs
          in
            li
              [ class "horizontal-layout vertical-margin" ]
              [ img channelImgAttrs []
              , img [ src video.thumbnailUrl, width 120, height 90, class "horizontal-margin" ] []
              , h3 [ class "horizontal-margin" ] [ text video.title ]
              ]
        )
        filteredVideos

findById : String -> List Video -> Maybe Video
findById id list =
  list
    |> List.filter (\item -> item.id == id)
    |> List.head
