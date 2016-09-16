module Types exposing (..)

import Auth

import Time exposing (Time)
import Dict exposing (Dict)
import Material

type alias Model =
  { authorization : Auth.Model
  , mdl : Material.Model
  , isFetchingChannels : Bool
  , lastChannelFetch : Time
  , isFetchingVideos : Bool
  , lastVideosFetch : Time
  , timeTillNextVideosFetch : Float
  , channels : Dict String Channel
  , channelFilter : String
  , videoFilter : String
  , selectedTab : String
  }

type alias Channel =
  { id : String
  , title : String
  , thumbnailUrl : String
  , uploadPlaylist : String
  , videos : List Video
  , shows : List String
  }

type alias Video =
  { id : String
  , channelId : String
  , title : String
  , thumbnailUrl : String
  , publishedAt : String
  }

type alias StoredState =
  { authorization : Auth.Model
  , channels : List Channel
  }

findById : String -> List { a | id : String } -> Maybe { a | id : String }
findById id list =
  list
    |> List.filter (\item -> item.id == id)
    |> List.head
