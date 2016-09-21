port module Auth exposing
  ( Model
  , init, Msg, update, subscriptions
  , authorize
  )

import Time exposing (Time)

type alias Model =
  { isAuthorized : Bool
  , isAuthorizing : Bool
  , token : Maybe String
  , expiresAt : Maybe Time
  }

init : Maybe Model -> ( Model, Cmd Msg )
init storedModel =
  let
    model = Model
      False
      True
      (Maybe.andThen storedModel .token)
      Nothing
  in
    model ! [ authorize (model, True) ]

type Msg
  = Tick Time
  | GetAuthorization Bool
  | GotAuthorization { token : String, expiresAt : Time }
  | FailedAuthorization

-- Outgoing
port authorize : (Model, Bool) -> Cmd msg
-- Incoming
port gotAuthorization : ({ token : String, expiresAt : Time } -> msg) -> Sub msg
port failedAuthorization : (() -> msg) -> Sub msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Tick now ->
      let
        expiredToken = Maybe.andThen model.expiresAt (\expiresAt -> expiresAt < now |> Just)
          |> Maybe.withDefault False
        tokenNeedsRefresh
          = model.isAuthorized
          && not model.isAuthorizing
          && expiredToken
      in
        if tokenNeedsRefresh then
          { model | isAuthorizing = True } ! [ authorize (model, True) ]
        else
          model ! []

    GetAuthorization immediate ->
      { model
      | isAuthorizing = True
      } ! [ authorize (model, immediate) ]

    GotAuthorization authorization ->
      { model
      | isAuthorized = True
      , isAuthorizing = False
      , token = Just authorization.token
      , expiresAt = Just authorization.expiresAt
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
    [ Time.every Time.second Tick
    , gotAuthorization GotAuthorization
    , failedAuthorization <| always FailedAuthorization
    ]
