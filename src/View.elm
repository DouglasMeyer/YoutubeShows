module View exposing (root)

import Types exposing (Model, Channel, Video, findById)
import State exposing (indexToTab, tabToIndex, Msg(..))

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

root : Model -> Html Msg
root model =
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
      ( [ text "Videos"
        , Options.span
          ( if not model.authorization.isAuthorized && not model.authorization.isAuthorizing then
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

about : Model -> Html Msg
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
        ]
      ]

mainView : Model -> Html Msg
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
        filteredChannels
        ( model.isFetchingChannels || model.isFetchingVideos )
        model.videoFilter
      ]

channelList : String -> List Channel -> Html Msg
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

videosList : List Channel -> Bool -> String -> Html Msg
videosList channels isFetching videoFilter =
  let
    filterRegex = videoFilter |> Regex.regex |> Regex.caseInsensitive
    videos = channels
      |> List.concatMap .videos
      |> List.filter (\video -> Regex.contains filterRegex video.title)
      |> List.sortBy .publishedAt
      |> List.reverse
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
      ) :: ( li
        [ style (if isFetching then [] else [("display","none")]) ]
        [ text "Checking for new videos" ]
      ) :: List.map (\v -> videoView v channels) videos

videoView : Video -> List Channel -> Html Msg
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
