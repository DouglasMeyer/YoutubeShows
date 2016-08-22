port module YoutubeShows exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy2)
import Time exposing (Time)
import Dict exposing (Dict)
import Task

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
  , channels: Dict String Channel
  , selectedChannelId: Maybe String
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
        , channels = List.foldr updateChannelsWithChannels emptyModel.channels stored.channels
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
            channelIds = model.channels
              |> Dict.values
              |> List.map .uploadPlaylist
          in
            { model
              | lastVideosFetch = now
              , timeTillNextVideosFetch = timeTillNextVideosFetch
            } ! [ requestYTVideos channelIds ]
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
        | channels = List.foldr updateChannelsWithChannels model.channels channels
        , lastVideosFetch = 0
      } ! []

    AddVideos videos ->
      { model
        | channels = List.foldr updateChannelsWithVideo model.channels videos
      } ! []

    SelectChannel channelId ->
      { model | selectedChannelId = channelId } ! []


updateChannelsWithChannels : Channel -> Dict String Channel -> Dict String Channel
updateChannelsWithChannels channel channels =
  case Dict.get channel.id channels of
    Nothing ->
      Dict.insert channel.id channel channels
    Just existingChannel ->
      Dict.insert channel.id ({ channel
        | videos = existingChannel.videos
      }) channels

updateChannelsWithVideo : Video -> Dict String Channel -> Dict String Channel
updateChannelsWithVideo video channels =
  case Dict.get video.channelId channels of
    Nothing ->
      channels

    Just channel ->
      Dict.insert channel.id (
        { channel
          | videos = if List.member video channel.videos then
              channel.videos
            else
              video :: channel.videos
          }
        ) channels


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
  div
    [ class "vertical-layout" ]
    [ viewHeader model
    , div
      [ class "expand-layout horizontal-layout scroll-vertically"
      ]
      [ lazy2 viewChannelList model.selectedChannelId model.channels
      , lazy2 viewVideosList model.selectedChannelId model.channels
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
          [ img [ src channel.thumbnailUrl, title channel.title, width 88, height 88 ] []
          ]
      )
      (Dict.values channels)

viewVideosList : Maybe String -> Dict String Channel -> Html Msg
viewVideosList selectedChannelId channels =
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
                src channel.thumbnailUrl :: onClick (SelectChannel (Just channel.id)) :: class "horizontal-margin clickable" :: defaultChannelImgAttrs
          in
            li
              [ class "horizontal-layout vertical-margin" ]
              [ img channelImgAttrs []
              , img [ src video.thumbnailUrl, width 120, height 90, class "horizontal-margin" ] []
              , h3 [ class "horizontal-margin" ] [ text video.title ]
              ]
        )
        videos
