port module Ports exposing(..)
--import Json.Decode exposing(..)
import Json.Encode exposing (Value)

port configureAce : String -> Cmd m


port save : Json.Encode.Value -> Cmd m


port updateEditorContent : (String -> msg) -> Sub msg