port module Auth exposing
  ( Model
  , init, Msg, update, subscriptions
  , authorize
  )

type alias Model =
  { isAuthorized : Bool
  , isAuthorizing : Bool
  , token : Maybe String
  }

init : Maybe Model -> ( Model, Cmd Msg )
init storedModel =
  let
    model = Model
      False
      True
      <| Maybe.andThen storedModel .token
  in
    model ! [ authorize (model, True) ]

type Msg
  = GetAuthorization Bool
  | GotAuthorization { token : String }
  | FailedAuthorization

-- Outgoing
port authorize : (Model, Bool) -> Cmd msg
-- Incoming
port gotAuthorization : ({ token : String } -> msg) -> Sub msg
port failedAuthorization : (() -> msg) -> Sub msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GetAuthorization immediate ->
      model ! [ authorize (model, immediate) ]

    GotAuthorization authorization ->
      { model
      | isAuthorized = True
      , isAuthorizing = False
      , token = Just authorization.token
      } ! []

    FailedAuthorization ->
      { model
      | isAuthorized = False
      , isAuthorizing = False
      , token = Nothing
      } ! []

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ gotAuthorization GotAuthorization
    , failedAuthorization <| always FailedAuthorization
    ]
