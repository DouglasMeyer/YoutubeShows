port module YoutubeShows exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)

main : Program (Maybe Model)
main =
  App.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

port authorize : String -> Cmd msg

port requestYTSubscribedChannels : String -> Cmd msg

port requestYTVideos : String -> Cmd msg


-- MODEL
type alias Model =
  { authorized : Bool
  , subscriptions : Subscriptions
  , channels: List Channel
  }

type alias Subscriptions =
  { subscriptions : List Subscription
  , isFetching : Bool
  }

type alias Subscription =
  { title : String
  , channelId : String
  , thumbnailUrl : String
  , uploadPlaylist : String
  }

type alias Channel =
  { id : String
  , videos : List Video
  , isFetching : Bool
  }

type alias Video =
  { id : String
  , title : String
  , thumbnailUrl : String
  }

emptyModel : Model
emptyModel =
  { authorized = False
  , subscriptions =
    { subscriptions = []
    , isFetching = True
    }
  , channels = []
  }

init : a -> ( Model, Cmd Msg )
init _ =
  ( emptyModel, Cmd.batch [ authorize "Blah" ] )

-- UPDATE

type Msg
  = NoOp
  | SetAuthorization Bool
  | SetSubscribedChannels ( List Subscription )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []

    SetAuthorization authorized ->
      { model | authorized = authorized } ! [ requestYTSubscribedChannels "Blah" ]

    SetSubscribedChannels subscriptions ->
      { model
        | subscriptions =
        { subscriptions = subscriptions
        , isFetching = False
        }
      } ! []


-- SUBSCRIPTIONS

port gotAuthorization : (() -> msg) -> Sub msg
port failedAuthorization : (() -> msg) -> Sub msg

port subscribedChannels : (List Subscription -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ gotAuthorization (\_ -> SetAuthorization True)
    , failedAuthorization (\_ -> SetAuthorization False)
    , subscribedChannels SetSubscribedChannels
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
      [ viewChannelList model.subscriptions
      , viewVideosList
      ]
    , footer [] [ text "by Douglas Meyer" ]
    ]

viewHeader : Model -> Html Msg
viewHeader model =
  div
    [ class "horizontal-layout" ]
    [ header [] [ text "YoutubeShows" ]
    , div [ class "expand-layout align-text-right" ] [ text (if model.authorized then "Authorized" else "Not Authorized") ]
    ]

viewChannelList : Subscriptions -> Html Msg
viewChannelList subscriptions =
  ul [ class "no-list" ] <|
    List.map
      (\sub ->
        li
          []
          [ img [ src sub.thumbnailUrl, title sub.title, width 88, height 88 ] []
          ]
      )
      subscriptions.subscriptions

viewVideosList : Html Msg
viewVideosList =
  ul
    [ class "expand-layout no-list"]
    [ text "Videos" ]
