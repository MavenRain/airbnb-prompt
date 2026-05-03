port module Embedding.MiniLm exposing
    ( Response
    , embed
    , responseSubscription
    )

import Json.Decode as D
import Json.Encode as E


port embedRequest : E.Value -> Cmd msg


port embedResponse : (D.Value -> msg) -> Sub msg


type alias Response =
    { id : Int
    , result : Result String (List Float)
    }


embed : { id : Int, text : String } -> Cmd msg
embed { id, text } =
    embedRequest
        (E.object
            [ ( "id", E.int id )
            , ( "text", E.string text )
            ]
        )


responseSubscription : (Result String Response -> msg) -> Sub msg
responseSubscription toMsg =
    embedResponse
        (\value ->
            D.decodeValue responseDecoder value
                |> Result.mapError D.errorToString
                |> toMsg
        )


responseDecoder : D.Decoder Response
responseDecoder =
    D.map2 Response
        (D.field "id" D.int)
        (D.oneOf
            [ D.field "error" D.string |> D.map Err
            , D.field "vector" (D.list D.float) |> D.map Ok
            ]
        )
