port module YoutubeShows exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
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
      [ viewChannelList model.channels
      , viewVideosList model
      ]
    , footer [] [ text "by Douglas Meyer" ]
    ]

viewHeader : Model -> Html Msg
viewHeader model =
  div
    [ class "horizontal-layout" ]
    [ header [] [ text "YoutubeShows" ]
    , div [ class "expand-layout align-text-right" ] [ text (if model.isAuthorized then "Authorized" else "Not Authorized") ]
    ]

viewChannelList : Dict String Channel -> Html Msg
viewChannelList channels =
  ul [ class "no-list" ] <|
    List.map
      (\channel ->
        li
          []
          [ img [ src channel.thumbnailUrl, title channel.title, width 88, height 88 ] []
          ]
      )
      (Dict.values channels)

viewVideosList : Model -> Html Msg
viewVideosList model =
  let
    videos = model.channels
      |> Dict.values
      |> List.concatMap .videos
      |> List.sortBy .publishedAt
      |> List.reverse
  in
    ul [ class "expand-layout no-list"]
      <| List.map
        (\video ->
          li
            []
            [ h3 [] [ text video.title ]
            , img [ src video.thumbnailUrl, width 120, height 90 ] []
            ]
        )
        videos
