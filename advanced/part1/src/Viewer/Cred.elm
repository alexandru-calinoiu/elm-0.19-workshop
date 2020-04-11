module Viewer.Cred exposing (Cred, addHeader, addHeaderIfAvailable, decoder, encodeToken, username)

import HttpBuilder exposing (RequestBuilder, withHeader)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)
import Username exposing (Username)



-- TYPES


type Cred
    = Cred Internals


type alias Internals =
    { username : Username
    , token : String
    }


username : Cred -> Username
username (Cred internal) =
    internal.username


decoder : Decoder Cred
decoder =
    Decode.succeed Internals
        |> required "username" Username.decoder
        |> required "token" Decode.string
        |> Decode.map Cred



-- TRANSFORM


encodeToken : Cred -> Value
encodeToken (Cred internals) =
    Encode.string internals.token


addHeader : Cred -> RequestBuilder a -> RequestBuilder a
addHeader (Cred internal) builder =
    builder
        |> withHeader "authorization" ("Token " ++ internal.token)


addHeaderIfAvailable : Maybe Cred -> RequestBuilder a -> RequestBuilder a
addHeaderIfAvailable maybeCred builder =
    case maybeCred of
        Just cred ->
            addHeader cred builder

        Nothing ->
            builder
