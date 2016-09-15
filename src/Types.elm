module Types exposing (..)

import Time exposing (Time)
import Dict exposing (Dict)
import Material

type alias Model =
  { authorization : Maybe Authorization
  , lastAuthCheck : Time
  , isFetchingChannels : Bool
  , lastChannelFetch : Time
  , isFetchingVideos : Bool
  , lastVideosFetch : Time
  , timeTillNextVideosFetch : Float
  , channels : Dict String Channel
  , channelFilter : String
  , videoFilter : String
  , mdl : Material.Model
  , selectedTab : String
  }

type alias Authorization =
  { token : String
  , expiresAt : Float
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
  { authorization : Maybe Authorization
  , channels : List Channel
  }

findById : String -> List { a | id : String } -> Maybe { a | id : String }
findById id list =
  list
    |> List.filter (\item -> item.id == id)
    |> List.head
