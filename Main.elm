port module YoutubeShows exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Time exposing (Time)
import Dict exposing (Dict)

main : Program Never
main =
  App.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

port authorize : () -> Cmd msg
port requestYTSubscribedChannels : () -> Cmd msg
port requestYTVideos : List String -> Cmd msg


-- MODEL
type alias Model =
  { isAuthorized : Bool
  , lastAuthCheck : Time
  , lastChannelFetch : Time
  , lastVideosFetch : Time
  , channels: Dict String Channel
  , selectedChannelId: Maybe String
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

emptyModel : Model
emptyModel =
  { isAuthorized = False
  , lastAuthCheck = 0
  , lastChannelFetch = 0
  , lastVideosFetch = 0
  , channels = Dict.empty
  , selectedChannelId = Nothing
  }

init : a -> ( Model, Cmd Msg )
init _ = emptyModel ! []

-- UPDATE

type Msg
  = NoOp
  | Tick Time
  | SetAuthorization Bool
  | AddSubscribedChannels ( List Channel )
  | AddVideos ( List Video )
  | SelectChannel ( Maybe String )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []

    Tick now ->
      if (model.isAuthorized == False) && (now - model.lastAuthCheck) > Time.minute then
        { model
          | lastAuthCheck = now
        } ! [ authorize () ]
      else if model.isAuthorized && (now - model.lastChannelFetch) > Time.hour * 16 then
        { model
          | lastChannelFetch = now
        } ! [ requestYTSubscribedChannels () ]
      else if model.isAuthorized && (now - model.lastVideosFetch) > Time.minute * 20 then
        let
          channelIds = model.channels
            |> Dict.values
            |> List.map .uploadPlaylist
        in
          { model
            | lastVideosFetch = now
          } ! [ requestYTVideos channelIds ]
      else
        model ! []

    SetAuthorization isAuthorized ->
      { model | isAuthorized = isAuthorized } ! [ requestYTSubscribedChannels () ]

    AddSubscribedChannels channels ->
      { model
        | channels = List.foldr updateChannelsWithChannels model.channels channels
      } ! []

    AddVideos videos ->
      { model
        | channels = List.foldr updateChannelsWithVideo model.channels videos
      } ! []

    SelectChannel channelId ->
      { model | selectedChannelId = channelId } ! []


updateChannelsWithChannels : Channel -> Dict String Channel -> Dict String Channel
updateChannelsWithChannels channel channels =
  Dict.insert channel.id channel channels

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

port gotAuthorization : (() -> msg) -> Sub msg
port failedAuthorization : (() -> msg) -> Sub msg

port subscribedChannels : (List Channel -> msg) -> Sub msg
port channelVideos : (List Video -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every Time.second Tick
    , gotAuthorization (\_ -> SetAuthorization True)
    , failedAuthorization (\_ -> SetAuthorization False)
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
      [ viewChannelList model.selectedChannelId model.channels
      , viewVideosList model
      ]
    , footer [] [ text "by Douglas Meyer" ]
    ]

viewHeader : Model -> Html Msg
viewHeader model =
  div
    [ class "horizontal-layout do-not-shrink" ]
    [ header [] [ text "YoutubeShows" ]
    , div [ class "expand-layout align-text-right" ] [ text (if model.isAuthorized then "Authorized" else "Not Authorized") ]
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

viewVideosList : Model -> Html Msg
viewVideosList model =
  let
    channels = case model.selectedChannelId `Maybe.andThen` (\channelId -> Dict.get channelId model.channels) of
      Nothing ->
        model.channels
          |> Dict.values
          |> List.concatMap .videos
      Just channel ->
        channel.videos
    videos = channels
      |> List.sortBy .publishedAt
      |> List.reverse
  in
    ul [ class "expand-layout no-list horizontal-margin"]
      <| List.map
        (\video ->
          let
            defaultChannelImgAttrs = [ width 88, height 88 ]
            channelImgAttrs = case Dict.get video.channelId model.channels of
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
