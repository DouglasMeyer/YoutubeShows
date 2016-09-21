module View exposing (root)

import Types exposing (Model, Channel, Video, findById)
import State exposing (indexToTab, tabToIndex, Msg(..))
import Notification

import Regex
import Dict

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Lazy exposing (lazy2, lazy3)

import Material.Layout as Layout
import Material.Badge
import Material.Options as Options
import Material.Grid as Grid exposing (Device(..))
import Material.Button as Button
import Material.Progress as Progress
import Material.Typography as Typo

root : Model -> Html State.Msg
root model =
  let
    needsAuthorization = not model.authorization.isAuthorized && not model.authorization.isAuthorizing
    needsNotificationPermission = Notification.needsNotificationPermission model.notification
  in
    Layout.render Mdl
      model.mdl
      [ Layout.fixedHeader
      , Layout.selectedTab <| tabToIndex model.selectedTab
      , Layout.onSelectTab <| indexToTab >> SelectTab
      ]
      { header =
        [ Layout.row []
          [ Layout.title [] [ text "YoutubeShows" ]
          ]
        ]
      , drawer = []
      , tabs =
        ( [ Options.span
            ( if List.isEmpty model.newVideos then
              []
            else
              [ Material.Badge.add <| toString <| List.length model.newVideos
              ]
            )
            [ text "Videos" ]
          , Options.span
            ( if needsAuthorization || needsNotificationPermission then
              [ Material.Badge.add "•"]
            else
              []
            )
            [ text "About" ]
          ],
          []
        )
      , main =
        [ case model.selectedTab of
          "about" -> about model
          _ -> mainView model
        ]
      }

about : Model -> Html State.Msg
about model =
  let
    signInRow =
      if model.authorization.isAuthorized then
        [ text "You are "
        , em [] [ text "signed in"]
        , text " through Google"
        ]
      else if model.authorization.isAuthorizing then
        [ text "Signing in"
        , Progress.indeterminate
        ]
      else
        [ text "You are "
        , em [] [ text "not signed in" ]
        , text " through Google "
        , Button.render Mdl [0] model.mdl
          [ Button.raised
          , Button.colored
          , Button.onClick <| GetAuthorization False
          ]
          [ text "Sign in" ]
        ]
    notificationRow =
      if Notification.needsNotificationPermission model.notification then
        [ text "In order to receive notifications, you will need to "
        , Button.render Mdl [1] model.mdl
          [ Button.raised
          , Button.colored
          , Button.onClick <| GetNotificationPermission
          ]
          [ text "grant permission" ]
        ]
      else
        [ text "You will receive notifications " ]
  in
    Grid.grid []
      [ Grid.cell [ Grid.offset All 3, Grid.size All 6 ]
        [ p []
          [ text """
            With YoutubeShows you will never miss a video released from your subscriptions. When a video is
            published to a channel you follow, you will get a notification. Simple
            as that.
            More features to follow.
          """
          ]
        , Options.styled h3
          [ Typo.title ]
          [ text "Google sign-in" ]
        , p [] signInRow
        , a
          [ href "https://support.google.com/accounts/answer/112802?hl=en"
          , target "_blank"
          ]
          [ text "Learn more about using your Google Account to Sign in to other sites" ]
        , Options.styled h3
          [ Typo.title ]
          [ text "Notifications" ]
        , p [] notificationRow
        ]
      ]

mainView : Model -> Html State.Msg
mainView model =
  let
    filterRegex = model.channelFilter |> Regex.regex |> Regex.caseInsensitive
    filteredChannels =
      model.channels
        |> Dict.values
        |> List.filter (\channel -> Regex.contains filterRegex channel.title)
  in
    div
      [ class "horizontal-layout"
      ]
      [ lazy2 channelList model.channelFilter filteredChannels
      , lazy3
        videosList
        model
        filteredChannels
        model.videoFilter
      ]

channelList : String -> List Channel -> Html State.Msg
channelList channelFilter channels =
  ul [ class "no-list horizontal-margin" ] <|
    ( li
      []
      [ input
        [ type' "search"
        , placeholder "channel filter"
        , onInput FilterChannels
        , value channelFilter
        , style [ ("width", "100%") ]
        ]
        []
      ]
    ) :: List.map
      (\channel ->
        li
          [ onClick (FilterChannels channel.title), class (if channel.title == channelFilter then "selected vertical-margin" else "clickable vertical-margin")
          , style
            [ ("background", "url(" ++ channel.thumbnailUrl ++ ") no-repeat center / cover")
            , ("padding-top", "calc(88px - 1em)")
            , ("width", "10em")
            ]
          ]
          [ div
            [ style
              [ ("color", "#EEE")
              , ("background-image", "-webkit-linear-gradient(bottom, black, black, transparent)")
              , ("padding", "2px 5px")
              ]
            ]
            [ text channel.title ]
          ]
      )
      channels

videosList : Model -> List Channel -> String -> Html State.Msg
videosList model channels videoFilter =
  let
    filterRegex = videoFilter |> Regex.regex |> Regex.caseInsensitive
    videos = channels
      |> List.concatMap .videos
      |> List.filter (\video -> Regex.contains filterRegex video.title)
      |> List.sortBy .publishedAt
      |> List.reverse
      |> List.take 300
  in
    ul [ class "expand-layout no-list horizontal-margin"] <|
      ( li
        []
        [ input
          [ type' "search"
          , placeholder "video filter"
          , onInput FilterVideos
          , value videoFilter
          ]
          []
        ]
      )
      :: videosStatus model
      :: List.map (\v -> videoView v channels) videos

videosStatus : Model -> Html State.Msg
videosStatus model =
  let
    isFetching = model.isFetchingChannels || model.isFetchingVideos
  in
    if isFetching then
      li [ class "VideosStatus" ]
        [ text "Checking for new videos" ]
    else
      li [] []

videoView : Video -> List Channel -> Html State.Msg
videoView video channels =
  let
    channel = case findById video.channelId channels of
      Nothing ->
        Channel "" "unknown channel" "//s.ytimg.com/yts/img/avatar_720-vflYJnzBZ.png" "" [] []
      Just channel ->
        channel
  in
    li
      [ class "vertical-margin"
      , style [ ("display", "flex"), ("flex-direction", "column") ]
      ]
      [ div
        [ style [ ("display", "flex"), ("align-items","center"), ("margin-bottom","3px") ]
        , onClick (FilterChannels channel.title)
        , class "clickable"
        ]
        [ img
          [ style [ ("height","1.2em"), ("margin-right","1ex") ]
          , src channel.thumbnailUrl
          , alt channel.title
          ] []
        , text channel.title
        ]
      , a
        [ style [ ("display", "flex"), ("align-items","center"), ("font-size","1.1rem"), ("font-weight","bold") ]
        , href ("http://youtube.com/watch?v=" ++ video.id)
        , target "_new"
        ]
        [ img
          [ width 120
          , height 90
          , style [ ("margin-right","1em") ]
          , src video.thumbnailUrl
          ]
          []
        , text video.title
        ]
      ]
