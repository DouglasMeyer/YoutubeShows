port module Notification exposing
  ( Model, Permission(..)
  , init, Msg, update, subscriptions
  , requestPermission, notify
  )

type Permission
  = Default
  | Granted
  | Denied
  | Unsupported

type alias Model =
  { permission : Permission
  }

init : ( Model, Cmd Msg )
init =
  Model
    Default
  ! [ checkPermission () ]

type Msg
  = UpdatePermission String
  | GetNotificationPermission

-- Outgoing
port checkPermission : () -> Cmd msg
port requestPermission : () -> Cmd msg
port notify : (String, String, String) -> Cmd msg
-- Incoming
port updatePermission : (String -> msg) -> Sub msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdatePermission permission -> onUpdatePermission model permission
    GetNotificationPermission -> model ! [ requestPermission () ]

onUpdatePermission : Model -> String -> (Model, Cmd Msg)
onUpdatePermission model permission =
  { model | permission = stringToPermission permission
  } ! []

stringToPermission : String -> Permission
stringToPermission permission =
  case permission of
    "granted" -> Granted
    "denied" -> Denied
    "unsupported" -> Unsupported
    _ -> Default

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ updatePermission UpdatePermission
    ]
