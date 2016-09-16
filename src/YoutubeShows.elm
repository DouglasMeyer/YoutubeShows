port module YoutubeShows exposing (..)

import State exposing (setStorage, Msg(SelectTab))
import View
import Types exposing (Model, StoredState)

import RouteUrl as Routing
import Navigation
import Dict exposing (Dict)
import String

main : Program (Maybe StoredState)
main =
  Routing.programWithFlags
    { init = State.init
    , view = View.root
    , update = updateWithStorage
    , subscriptions = State.subscriptions
    , delta2url = delta2url
    , location2messages = location2messages
    }

delta2url : Model -> Model -> Maybe Routing.UrlChange
delta2url model1 model2 =
  if model1.selectedTab /= model2.selectedTab then
    { entry = Routing.NewEntry
    , url = model2.selectedTab
      |> String.cons '#'
    } |> Just
  else
    Nothing

location2messages : Navigation.Location -> List Msg
location2messages location =
  let
    possibleTab = location.hash |> String.dropLeft 1
    tab = if String.isEmpty possibleTab then "about" else possibleTab
  in
    [ SelectTab tab ]

updateWithStorage : Msg -> Model -> (Model, Cmd Msg)
updateWithStorage msg model =
  let
    (newModel, cmds) = State.update msg model
  in
    doSetStorage newModel cmds

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
