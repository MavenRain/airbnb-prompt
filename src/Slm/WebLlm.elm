port module Slm.WebLlm exposing
    ( Response
    , Status(..)
    , generate
    , init
    , responseSubscription
    , statusSubscription
    )

import Json.Decode as D
import Json.Encode as E


port slmInit : E.Value -> Cmd msg


port slmGenerate : E.Value -> Cmd msg


port slmStatus : (D.Value -> msg) -> Sub msg


port slmResponse : (D.Value -> msg) -> Sub msg


type Status
    = Loading { progress : Float, text : String }
    | Ready
    | LoadError String


type alias Response =
    { id : Int
    , result : Result String D.Value
    }


init : { modelId : String } -> Cmd msg
init { modelId } =
    slmInit (E.object [ ( "modelId", E.string modelId ) ])


generate :
    { id : Int
    , system : String
    , prompt : String
    , schema : E.Value
    , maxTokens : Int
    }
    -> Cmd msg
generate args =
    slmGenerate
        (E.object
            [ ( "id", E.int args.id )
            , ( "system", E.string args.system )
            , ( "prompt", E.string args.prompt )
            , ( "schema", args.schema )
            , ( "maxTokens", E.int args.maxTokens )
            ]
        )


statusSubscription : (Result String Status -> msg) -> Sub msg
statusSubscription toMsg =
    slmStatus
        (\value ->
            D.decodeValue statusDecoder value
                |> Result.mapError D.errorToString
                |> toMsg
        )


responseSubscription : (Result String Response -> msg) -> Sub msg
responseSubscription toMsg =
    slmResponse
        (\value ->
            D.decodeValue responseDecoder value
                |> Result.mapError D.errorToString
                |> toMsg
        )


statusDecoder : D.Decoder Status
statusDecoder =
    D.field "event" D.string
        |> D.andThen
            (\event ->
                case event of
                    "loading" ->
                        D.map2 (\p t -> Loading { progress = p, text = t })
                            (D.field "progress" D.float)
                            (D.field "text" D.string)

                    "ready" ->
                        D.succeed Ready

                    "error" ->
                        D.field "message" D.string |> D.map LoadError

                    other ->
                        D.fail ("unknown event: " ++ other)
            )


responseDecoder : D.Decoder Response
responseDecoder =
    D.map2 Response
        (D.field "id" D.int)
        (D.oneOf
            [ D.field "error" D.string |> D.map Err
            , D.field "result" D.value |> D.map Ok
            ]
        )
