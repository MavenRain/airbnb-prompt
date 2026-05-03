module Parse.Pipeline exposing
    ( Exemplar
    , OntologyEntry
    , buildSystemPrompt
    , decodeExemplars
    , decodeOntology
    , decodeSlmResult
    , isComplete
    , mergePartials
    , schemaJson
    )

import Date exposing (Date)
import Domain.Catalog as Catalog exposing (Catalog)
import Domain.Listing as Listing exposing (Tag)
import Json.Decode as D
import Json.Encode as E
import Parse.Heuristic exposing (PartialQuery)


type alias Exemplar =
    { prompt : String, result : PartialQuery }


type alias OntologyEntry =
    { text : String, hint : String }


isComplete : PartialQuery -> Bool
isComplete partial =
    isJust partial.destination
        && isJust partial.checkIn
        && isJust partial.checkOut


isJust : Maybe a -> Bool
isJust m =
    case m of
        Just _ ->
            True

        Nothing ->
            False



-- SYSTEM PROMPT


buildSystemPrompt :
    { today : Date
    , catalog : Catalog
    , exemplars : List Exemplar
    , ontology : List OntologyEntry
    , partial : PartialQuery
    }
    -> String
buildSystemPrompt args =
    let
        cities =
            Catalog.all args.catalog
                |> List.map .city
                |> uniqueStrings
                |> String.join ", "

        tagsList =
            "beachfront, pet-friendly, wifi, workspace, pool, mountain, city-center, quiet"

        examplesText =
            args.exemplars
                |> List.map exemplarToText
                |> String.join "\n\n"

        ontologyText =
            args.ontology
                |> List.map (\e -> "- " ++ e.text)
                |> String.join "\n"

        partialText =
            encodePartial args.partial
    in
    String.join "\n\n"
        [ "You extract structured booking queries from natural-language prompts. Output ONLY valid JSON, no prose, no markdown."
        , "TODAY: " ++ Date.toIsoString args.today
        , "AVAILABLE CITIES: " ++ cities
        , "AVAILABLE TAGS: " ++ tagsList
        , "OUTPUT SHAPE: {\"destination\": string|null, \"checkIn\": YYYY-MM-DD|null, \"checkOut\": YYYY-MM-DD|null, \"adults\": int|null, \"children\": int|null, \"infants\": int|null, \"pets\": int|null, \"tags\": [string,...]}"
        , "ONTOLOGY HINTS:\n" ++ ontologyText
        , "EXAMPLES:\n" ++ examplesText
        , "INITIAL EXTRACTION (from heuristic, may be partial; preserve correct fields, fill nulls):\n" ++ partialText
        , "Use null for unknown fields. Tags array can be empty."
        ]


exemplarToText : Exemplar -> String
exemplarToText e =
    "PROMPT: " ++ e.prompt ++ "\nRESULT: " ++ encodePartial e.result


encodePartial : PartialQuery -> String
encodePartial p =
    E.encode 0 (encodePartialJson p)


encodePartialJson : PartialQuery -> E.Value
encodePartialJson p =
    E.object
        [ ( "destination", maybeOr E.string p.destination )
        , ( "checkIn", maybeOr (Date.toIsoString >> E.string) p.checkIn )
        , ( "checkOut", maybeOr (Date.toIsoString >> E.string) p.checkOut )
        , ( "adults", maybeOr E.int p.adults )
        , ( "children", maybeOr E.int p.children )
        , ( "infants", maybeOr E.int p.infants )
        , ( "pets", maybeOr E.int p.pets )
        , ( "tags", E.list (Listing.tagToString >> E.string) p.tags )
        ]


maybeOr : (a -> E.Value) -> Maybe a -> E.Value
maybeOr enc m =
    case m of
        Just x ->
            enc x

        Nothing ->
            E.null


uniqueStrings : List String -> List String
uniqueStrings =
    List.foldr
        (\x acc ->
            if List.member x acc then
                acc

            else
                x :: acc
        )
        []



-- JSON SCHEMA (passed to WebLLM for response_format hinting)


schemaJson : E.Value
schemaJson =
    E.object
        [ ( "type", E.string "object" )
        , ( "properties"
          , E.object
                [ ( "destination", typeOrNull "string" )
                , ( "checkIn", typeOrNull "string" )
                , ( "checkOut", typeOrNull "string" )
                , ( "adults", typeOrNull "integer" )
                , ( "children", typeOrNull "integer" )
                , ( "infants", typeOrNull "integer" )
                , ( "pets", typeOrNull "integer" )
                , ( "tags"
                  , E.object
                        [ ( "type", E.string "array" )
                        , ( "items", E.object [ ( "type", E.string "string" ) ] )
                        ]
                  )
                ]
          )
        , ( "required"
          , E.list E.string
                [ "destination"
                , "checkIn"
                , "checkOut"
                , "adults"
                , "children"
                , "infants"
                , "pets"
                , "tags"
                ]
          )
        , ( "additionalProperties", E.bool False )
        ]


typeOrNull : String -> E.Value
typeOrNull t =
    E.object [ ( "type", E.list E.string [ t, "null" ] ) ]



-- DECODERS (for SLM response and JSON corpus loads)


decodeSlmResult : D.Value -> Result String PartialQuery
decodeSlmResult value =
    D.decodeValue partialDecoder value
        |> Result.mapError D.errorToString


partialDecoder : D.Decoder PartialQuery
partialDecoder =
    D.map8 PartialQuery
        (optionalField "destination" D.string)
        (optionalField "checkIn" dateField)
        (optionalField "checkOut" dateField)
        (optionalField "adults" D.int)
        (optionalField "children" D.int)
        (optionalField "infants" D.int)
        (optionalField "pets" D.int)
        tagsField


optionalField : String -> D.Decoder a -> D.Decoder (Maybe a)
optionalField name decoder =
    D.oneOf
        [ D.field name (D.nullable decoder)
        , D.succeed Nothing
        ]


tagsField : D.Decoder (List Tag)
tagsField =
    D.oneOf
        [ D.field "tags" (D.list D.string |> D.map (List.filterMap Listing.tagFromString))
        , D.succeed []
        ]


dateField : D.Decoder Date
dateField =
    D.string
        |> D.andThen
            (\s ->
                case Date.fromIsoString s of
                    Ok d ->
                        D.succeed d

                    Err msg ->
                        D.fail msg
            )


decodeExemplars : D.Decoder (List Exemplar)
decodeExemplars =
    D.field "exemplars" (D.list exemplarDecoder)


exemplarDecoder : D.Decoder Exemplar
exemplarDecoder =
    D.map2 Exemplar
        (D.field "prompt" D.string)
        (D.field "result" partialDecoder)


decodeOntology : D.Decoder (List OntologyEntry)
decodeOntology =
    D.field "entries" (D.list ontologyDecoder)


ontologyDecoder : D.Decoder OntologyEntry
ontologyDecoder =
    D.map2 OntologyEntry
        (D.field "text" D.string)
        (D.field "hint" D.string)



-- MERGE


mergePartials : PartialQuery -> PartialQuery -> PartialQuery
mergePartials heuristic slm =
    { destination = heuristic.destination |> orElse slm.destination
    , checkIn = heuristic.checkIn |> orElse slm.checkIn
    , checkOut = heuristic.checkOut |> orElse slm.checkOut
    , adults = heuristic.adults |> orElse slm.adults
    , children = heuristic.children |> orElse slm.children
    , infants = heuristic.infants |> orElse slm.infants
    , pets = heuristic.pets |> orElse slm.pets
    , tags = uniqueTags (heuristic.tags ++ slm.tags)
    }


orElse : Maybe a -> Maybe a -> Maybe a
orElse m fallback =
    case m of
        Just _ ->
            m

        Nothing ->
            fallback


uniqueTags : List Tag -> List Tag
uniqueTags =
    List.foldr
        (\t acc ->
            if List.member t acc then
                acc

            else
                t :: acc
        )
        []
