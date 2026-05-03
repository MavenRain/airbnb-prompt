module Main exposing (main)

import Browser
import Date exposing (Date)
import Dict exposing (Dict)
import Domain.Booking as Booking exposing (BookingIntent, DateRange, GuestCount)
import Domain.Catalog as Catalog exposing (Catalog)
import Domain.Listing as Listing exposing (Listing, ListingId, Tag)
import Embedding.MiniLm as Embed
import Html exposing (Html, a, button, code, div, h1, h2, h3, input, label, p, span, text, textarea)
import Html.Attributes as Attr exposing (class, classList, href, placeholder, rel, rows, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Parse.Heuristic as Heuristic exposing (PartialQuery)
import Parse.Pipeline as Pipeline exposing (Exemplar, OntologyEntry)
import Rag.Index as Rag
import Slm.WebLlm as Slm
import Task
import Url.AirbnbDeepLink as DeepLink


modelId : String
modelId =
    "Qwen2.5-0.5B-Instruct-q4f16_1-MLC"


maxCards : Int
maxCards =
    5


topKExemplars : Int
topKExemplars =
    4


topKOntology : Int
topKOntology =
    6



-- LOADABLE / SLM STATE


type Loadable a
    = LoadingNow
    | LoadFailed String
    | LoadDone a


type SlmState
    = SlmIdle
    | SlmLoading { progress : Float, text : String }
    | SlmReady
    | SlmFailed String


type Corpus a
    = CorpusEmpty
    | CorpusBuilding { pending : Dict Int a, vectors : List ( Rag.Vector, a ) }
    | CorpusReady (Rag.Index a)
    | CorpusFailed String


type PromptEmbed
    = PromptEmbedIdle
    | PromptEmbedPending { id : Int, prompt : String, partial : PartialQuery }



-- MODEL


type alias Model =
    { catalog : Loadable Catalog
    , today : Maybe Date
    , exemplars : List Exemplar
    , ontology : List OntologyEntry
    , prompt : String
    , selectedListing : Maybe ListingId
    , checkIn : String
    , checkOut : String
    , adults : Int
    , children : Int
    , infants : Int
    , pets : Int
    , datesFromSlm : Bool
    , slmState : SlmState
    , slmLastError : Maybe String
    , heuristicPartial : Maybe PartialQuery
    , mergedPartial : Maybe PartialQuery
    , slmRequestId : Maybe Int
    , slmCounter : Int
    , pendingPrompt : Maybe String
    , exemplarCorpus : Corpus Exemplar
    , ontologyCorpus : Corpus OntologyEntry
    , listingCorpus : Corpus Listing
    , promptEmbed : PromptEmbed
    , promptVector : Maybe Rag.Vector
    , embedCounter : Int
    }


type Msg
    = GotCatalog (Result Http.Error Catalog)
    | GotToday Date
    | GotExemplars (Result Http.Error (List Exemplar))
    | GotOntology (Result Http.Error (List OntologyEntry))
    | GotSlmStatus (Result String Slm.Status)
    | GotSlmResponse (Result String Slm.Response)
    | GotEmbedResponse (Result String Embed.Response)
    | ChangedPrompt String
    | SelectedListing ListingId
    | ChangedCheckIn String
    | ChangedCheckOut String
    | ChangedAdults String
    | ChangedChildren String
    | ChangedInfants String
    | ChangedPets String
    | ShiftDates Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( { catalog = LoadingNow
      , today = Nothing
      , exemplars = []
      , ontology = []
      , prompt = ""
      , selectedListing = Nothing
      , checkIn = ""
      , checkOut = ""
      , adults = 1
      , children = 0
      , infants = 0
      , pets = 0
      , datesFromSlm = False
      , slmState = SlmIdle
      , slmLastError = Nothing
      , heuristicPartial = Nothing
      , mergedPartial = Nothing
      , slmRequestId = Nothing
      , slmCounter = 0
      , pendingPrompt = Nothing
      , exemplarCorpus = CorpusEmpty
      , ontologyCorpus = CorpusEmpty
      , listingCorpus = CorpusEmpty
      , promptEmbed = PromptEmbedIdle
      , promptVector = Nothing
      , embedCounter = 0
      }
    , Cmd.batch
        [ Http.get
            { url = "catalog.json"
            , expect = Http.expectJson GotCatalog Catalog.decoder
            }
        , Http.get
            { url = "exemplars.json"
            , expect = Http.expectJson GotExemplars Pipeline.decodeExemplars
            }
        , Http.get
            { url = "ontology.json"
            , expect = Http.expectJson GotOntology Pipeline.decodeOntology
            }
        , Task.perform GotToday Date.today
        ]
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCatalog (Ok catalog) ->
            startCorpusBuild
                (Catalog.all catalog)
                listingToText
                setListingCorpus
                { model | catalog = LoadDone catalog }

        GotCatalog (Err err) ->
            ( { model | catalog = LoadFailed (httpErrorToString err) }, Cmd.none )

        GotToday today ->
            ( { model | today = Just today }, Cmd.none )

        GotExemplars (Ok xs) ->
            startCorpusBuild xs .prompt setExemplarCorpus { model | exemplars = xs }

        GotExemplars (Err _) ->
            ( model, Cmd.none )

        GotOntology (Ok xs) ->
            startCorpusBuild xs .text setOntologyCorpus { model | ontology = xs }

        GotOntology (Err _) ->
            ( model, Cmd.none )

        GotSlmStatus result ->
            handleSlmStatus result model

        GotSlmResponse result ->
            handleSlmResponse result model

        GotEmbedResponse result ->
            handleEmbedResponse result model

        ChangedPrompt raw ->
            handlePromptChange raw model

        SelectedListing id ->
            ( { model | selectedListing = Just id }, Cmd.none )

        ChangedCheckIn raw ->
            ( { model | checkIn = raw, datesFromSlm = False }, Cmd.none )

        ChangedCheckOut raw ->
            ( { model | checkOut = raw, datesFromSlm = False }, Cmd.none )

        ChangedAdults raw ->
            ( { model | adults = parseIntOr model.adults raw }, Cmd.none )

        ChangedChildren raw ->
            ( { model | children = parseIntOr model.children raw }, Cmd.none )

        ChangedInfants raw ->
            ( { model | infants = parseIntOr model.infants raw }, Cmd.none )

        ChangedPets raw ->
            ( { model | pets = parseIntOr model.pets raw }, Cmd.none )

        ShiftDates delta ->
            ( shiftDates delta model, Cmd.none )



-- DATE SHIFT


shiftDates : Int -> Model -> Model
shiftDates delta model =
    { model
        | checkIn = shiftIso delta model.checkIn
        , checkOut = shiftIso delta model.checkOut
    }


shiftIso : Int -> String -> String
shiftIso delta raw =
    Date.fromIsoString raw
        |> Result.toMaybe
        |> Maybe.map (Date.add Date.Days delta >> Date.toIsoString)
        |> Maybe.withDefault raw



-- PROMPT CHANGE


handlePromptChange : String -> Model -> ( Model, Cmd Msg )
handlePromptChange raw model =
    let
        modelWithPrompt =
            { model | prompt = raw }

        heuristicResult =
            Maybe.map2 Tuple.pair model.today (catalogValue model.catalog)
                |> Maybe.map (\( today, catalog ) -> Heuristic.parse today catalog raw)
    in
    heuristicResult
        |> Maybe.map (\partial -> applyHeuristicAndDispatch raw partial modelWithPrompt)
        |> Maybe.withDefault ( modelWithPrompt, Cmd.none )


applyHeuristicAndDispatch : String -> PartialQuery -> Model -> ( Model, Cmd Msg )
applyHeuristicAndDispatch raw partial model =
    let
        heuristicHasDates =
            isJust partial.checkIn && isJust partial.checkOut

        applied =
            applyPartial model partial

        withFlags =
            { applied
                | heuristicPartial = Just partial
                , mergedPartial = Just partial
                , datesFromSlm =
                    if heuristicHasDates then
                        False

                    else
                        applied.datesFromSlm
            }
    in
    if String.isEmpty (String.trim raw) then
        ( { withFlags
            | pendingPrompt = Nothing
            , datesFromSlm = False
            , slmLastError = Nothing
            , mergedPartial = Nothing
            , promptVector = Nothing
          }
        , Cmd.none
        )

    else if Pipeline.isComplete partial then
        ( { withFlags | pendingPrompt = Nothing }, Cmd.none )

    else
        triggerSlm raw partial withFlags


isJust : Maybe a -> Bool
isJust m =
    case m of
        Just _ ->
            True

        Nothing ->
            False


triggerSlm : String -> PartialQuery -> Model -> ( Model, Cmd Msg )
triggerSlm prompt partial model =
    case model.slmState of
        SlmIdle ->
            ( { model
                | slmState = SlmLoading { progress = 0, text = "starting" }
                , pendingPrompt = Just prompt
              }
            , Slm.init { modelId = modelId }
            )

        SlmLoading _ ->
            ( { model | pendingPrompt = Just prompt }, Cmd.none )

        SlmReady ->
            dispatchToSlm prompt partial model

        SlmFailed _ ->
            ( model, Cmd.none )


dispatchToSlm : String -> PartialQuery -> Model -> ( Model, Cmd Msg )
dispatchToSlm prompt partial model =
    case ( model.exemplarCorpus, model.ontologyCorpus ) of
        ( CorpusReady _, CorpusReady _ ) ->
            embedPromptThenFire prompt partial model

        _ ->
            fireSlmRequestWith model.exemplars model.ontology prompt partial model


embedPromptThenFire : String -> PartialQuery -> Model -> ( Model, Cmd Msg )
embedPromptThenFire prompt partial model =
    let
        nextId =
            model.embedCounter + 1
    in
    ( { model
        | embedCounter = nextId
        , promptEmbed = PromptEmbedPending { id = nextId, prompt = prompt, partial = partial }
        , pendingPrompt = Nothing
      }
    , Embed.embed { id = nextId, text = prompt }
    )


fireSlmRequestWith : List Exemplar -> List OntologyEntry -> String -> PartialQuery -> Model -> ( Model, Cmd Msg )
fireSlmRequestWith exemplars ontology prompt partial model =
    Maybe.map2 Tuple.pair model.today (catalogValue model.catalog)
        |> Maybe.map
            (\( today, catalog ) ->
                let
                    nextId =
                        model.slmCounter + 1

                    systemPrompt =
                        Pipeline.buildSystemPrompt
                            { today = today
                            , catalog = catalog
                            , exemplars = exemplars
                            , ontology = ontology
                            , partial = partial
                            }
                in
                ( { model
                    | slmCounter = nextId
                    , slmRequestId = Just nextId
                    , pendingPrompt = Nothing
                  }
                , Slm.generate
                    { id = nextId
                    , system = systemPrompt
                    , prompt = prompt
                    , schema = Pipeline.schemaJson
                    , maxTokens = 512
                    }
                )
            )
        |> Maybe.withDefault ( model, Cmd.none )



-- SLM STATUS / RESPONSE HANDLERS


handleSlmStatus : Result String Slm.Status -> Model -> ( Model, Cmd Msg )
handleSlmStatus result model =
    case result of
        Err err ->
            ( { model | slmState = SlmFailed err }, Cmd.none )

        Ok (Slm.Loading p) ->
            ( { model | slmState = SlmLoading p }, Cmd.none )

        Ok Slm.Ready ->
            let
                readyModel =
                    { model | slmState = SlmReady }
            in
            Maybe.map2 Tuple.pair readyModel.pendingPrompt readyModel.heuristicPartial
                |> Maybe.map (\( prompt, partial ) -> dispatchToSlm prompt partial readyModel)
                |> Maybe.withDefault ( readyModel, Cmd.none )

        Ok (Slm.LoadError msg) ->
            ( { model | slmState = SlmFailed msg }, Cmd.none )


handleSlmResponse : Result String Slm.Response -> Model -> ( Model, Cmd Msg )
handleSlmResponse result model =
    case result of
        Err err ->
            ( { model | slmRequestId = Nothing, slmLastError = Just err }, Cmd.none )

        Ok response ->
            if Just response.id /= model.slmRequestId then
                ( model, Cmd.none )

            else
                handleSlmJson response.result model


handleSlmJson : Result String D.Value -> Model -> ( Model, Cmd Msg )
handleSlmJson result model =
    case result of
        Err err ->
            ( { model | slmRequestId = Nothing, slmLastError = Just err }, Cmd.none )

        Ok value ->
            case Pipeline.decodeSlmResult value of
                Err err ->
                    ( { model | slmRequestId = Nothing, slmLastError = Just err }, Cmd.none )

                Ok slmPartial ->
                    let
                        heuristic =
                            model.heuristicPartial |> Maybe.withDefault slmPartial

                        merged =
                            Pipeline.mergePartials heuristic slmPartial

                        slmAddedDates =
                            (not (isJust heuristic.checkIn) && isJust slmPartial.checkIn)
                                || (not (isJust heuristic.checkOut) && isJust slmPartial.checkOut)

                        cleared =
                            { model
                                | slmRequestId = Nothing
                                , slmLastError = Nothing
                                , mergedPartial = Just merged
                            }

                        applied =
                            applyPartial cleared merged
                    in
                    ( { applied | datesFromSlm = slmAddedDates || applied.datesFromSlm }
                    , Cmd.none
                    )



-- EMBEDDING / CORPUS


startCorpusBuild :
    List a
    -> (a -> String)
    -> (Corpus a -> Model -> Model)
    -> Model
    -> ( Model, Cmd Msg )
startCorpusBuild items toText setCorpus model =
    case items of
        [] ->
            ( setCorpus (CorpusReady (Rag.build [])) model, Cmd.none )

        _ ->
            let
                ( nextCounter, idItems ) =
                    assignIds model.embedCounter items

                pending =
                    Dict.fromList idItems

                cmds =
                    idItems
                        |> List.map (\( id, item ) -> Embed.embed { id = id, text = toText item })
            in
            ( setCorpus (CorpusBuilding { pending = pending, vectors = [] })
                { model | embedCounter = nextCounter }
            , Cmd.batch cmds
            )


assignIds : Int -> List a -> ( Int, List ( Int, a ) )
assignIds start items =
    let
        step item ( c, acc ) =
            ( c + 1, ( c + 1, item ) :: acc )

        ( finalCounter, reversed ) =
            List.foldl step ( start, [] ) items
    in
    ( finalCounter, List.reverse reversed )


setExemplarCorpus : Corpus Exemplar -> Model -> Model
setExemplarCorpus c m =
    { m | exemplarCorpus = c }


setOntologyCorpus : Corpus OntologyEntry -> Model -> Model
setOntologyCorpus c m =
    { m | ontologyCorpus = c }


setListingCorpus : Corpus Listing -> Model -> Model
setListingCorpus c m =
    { m | listingCorpus = c }


listingToText : Listing -> String
listingToText l =
    l.title
        ++ " in "
        ++ l.city
        ++ ", "
        ++ l.country
        ++ ". Features: "
        ++ String.join ", " (List.map Listing.tagToString l.tags)


handleEmbedResponse : Result String Embed.Response -> Model -> ( Model, Cmd Msg )
handleEmbedResponse result model =
    case result of
        Err _ ->
            ( model, Cmd.none )

        Ok response ->
            applyEmbedResult response model


applyEmbedResult : Embed.Response -> Model -> ( Model, Cmd Msg )
applyEmbedResult { id, result } model =
    case result of
        Err err ->
            ( failEmbedById id err model, Cmd.none )

        Ok vector ->
            applyEmbedVector id vector model


applyEmbedVector : Int -> Rag.Vector -> Model -> ( Model, Cmd Msg )
applyEmbedVector id vector model =
    case promptEmbedMatch id model.promptEmbed of
        Just { prompt, partial } ->
            firePromptWithRetrieval vector prompt partial { model | promptEmbed = PromptEmbedIdle }

        Nothing ->
            ( advanceCorpora id vector model, Cmd.none )


promptEmbedMatch : Int -> PromptEmbed -> Maybe { prompt : String, partial : PartialQuery }
promptEmbedMatch id pe =
    case pe of
        PromptEmbedPending p ->
            if p.id == id then
                Just { prompt = p.prompt, partial = p.partial }

            else
                Nothing

        PromptEmbedIdle ->
            Nothing


firePromptWithRetrieval : Rag.Vector -> String -> PartialQuery -> Model -> ( Model, Cmd Msg )
firePromptWithRetrieval vector prompt partial model =
    let
        exemplars =
            retrieveFromCorpus topKExemplars vector model.exemplarCorpus model.exemplars

        ontology =
            retrieveFromCorpus topKOntology vector model.ontologyCorpus model.ontology
    in
    fireSlmRequestWith exemplars ontology prompt partial { model | promptVector = Just vector }


retrieveFromCorpus : Int -> Rag.Vector -> Corpus a -> List a -> List a
retrieveFromCorpus k vec corpus fallback =
    case corpus of
        CorpusReady idx ->
            Rag.query vec k idx

        CorpusEmpty ->
            fallback

        CorpusBuilding _ ->
            fallback

        CorpusFailed _ ->
            fallback


advanceCorpora : Int -> Rag.Vector -> Model -> Model
advanceCorpora id vector model =
    model
        |> advanceExemplarCorpus id vector
        |> advanceOntologyCorpus id vector
        |> advanceListingCorpus id vector


advanceExemplarCorpus : Int -> Rag.Vector -> Model -> Model
advanceExemplarCorpus id vector model =
    case model.exemplarCorpus of
        CorpusBuilding state ->
            setExemplarCorpus (advanceBuilding id vector state) model

        CorpusEmpty ->
            model

        CorpusReady _ ->
            model

        CorpusFailed _ ->
            model


advanceOntologyCorpus : Int -> Rag.Vector -> Model -> Model
advanceOntologyCorpus id vector model =
    case model.ontologyCorpus of
        CorpusBuilding state ->
            setOntologyCorpus (advanceBuilding id vector state) model

        CorpusEmpty ->
            model

        CorpusReady _ ->
            model

        CorpusFailed _ ->
            model


advanceListingCorpus : Int -> Rag.Vector -> Model -> Model
advanceListingCorpus id vector model =
    case model.listingCorpus of
        CorpusBuilding state ->
            setListingCorpus (advanceBuilding id vector state) model

        CorpusEmpty ->
            model

        CorpusReady _ ->
            model

        CorpusFailed _ ->
            model


advanceBuilding : Int -> Rag.Vector -> { pending : Dict Int a, vectors : List ( Rag.Vector, a ) } -> Corpus a
advanceBuilding id vector state =
    case Dict.get id state.pending of
        Nothing ->
            CorpusBuilding state

        Just entry ->
            let
                nextPending =
                    Dict.remove id state.pending

                nextVectors =
                    ( vector, entry ) :: state.vectors
            in
            if Dict.isEmpty nextPending then
                CorpusReady (Rag.build nextVectors)

            else
                CorpusBuilding { pending = nextPending, vectors = nextVectors }


failEmbedById : Int -> String -> Model -> Model
failEmbedById id err model =
    model
        |> clearPromptEmbedIfMatch id
        |> failExemplarCorpusIfPending id err
        |> failOntologyCorpusIfPending id err
        |> failListingCorpusIfPending id err


clearPromptEmbedIfMatch : Int -> Model -> Model
clearPromptEmbedIfMatch id model =
    case promptEmbedMatch id model.promptEmbed of
        Just _ ->
            { model | promptEmbed = PromptEmbedIdle }

        Nothing ->
            model


failExemplarCorpusIfPending : Int -> String -> Model -> Model
failExemplarCorpusIfPending id err model =
    case model.exemplarCorpus of
        CorpusBuilding state ->
            if Dict.member id state.pending then
                setExemplarCorpus (CorpusFailed err) model

            else
                model

        CorpusEmpty ->
            model

        CorpusReady _ ->
            model

        CorpusFailed _ ->
            model


failOntologyCorpusIfPending : Int -> String -> Model -> Model
failOntologyCorpusIfPending id err model =
    case model.ontologyCorpus of
        CorpusBuilding state ->
            if Dict.member id state.pending then
                setOntologyCorpus (CorpusFailed err) model

            else
                model

        CorpusEmpty ->
            model

        CorpusReady _ ->
            model

        CorpusFailed _ ->
            model


failListingCorpusIfPending : Int -> String -> Model -> Model
failListingCorpusIfPending id err model =
    case model.listingCorpus of
        CorpusBuilding state ->
            if Dict.member id state.pending then
                setListingCorpus (CorpusFailed err) model

            else
                model

        CorpusEmpty ->
            model

        CorpusReady _ ->
            model

        CorpusFailed _ ->
            model



-- PARTIAL APPLICATION


applyPartial : Model -> PartialQuery -> Model
applyPartial model partial =
    { model
        | selectedListing = bestListingId model.catalog partial |> orElse model.selectedListing
        , checkIn =
            partial.checkIn
                |> Maybe.map Date.toIsoString
                |> Maybe.withDefault model.checkIn
        , checkOut =
            partial.checkOut
                |> Maybe.map Date.toIsoString
                |> Maybe.withDefault model.checkOut
        , adults = partial.adults |> Maybe.withDefault model.adults
        , children = partial.children |> Maybe.withDefault model.children
        , infants = partial.infants |> Maybe.withDefault model.infants
        , pets = partial.pets |> Maybe.withDefault model.pets
    }


bestListingId : Loadable Catalog -> PartialQuery -> Maybe ListingId
bestListingId loadable partial =
    case partial.destination of
        Nothing ->
            Nothing

        Just _ ->
            catalogValue loadable
                |> Maybe.map (\c -> Catalog.matchAgainst (filtersFromPartial partial) c)
                |> Maybe.andThen List.head
                |> Maybe.map .id


filtersFromPartial : PartialQuery -> Catalog.Filters
filtersFromPartial p =
    { destination = p.destination
    , totalGuests = Maybe.withDefault 1 p.adults + Maybe.withDefault 0 p.children
    , petsRequested = Maybe.withDefault 0 p.pets > 0
    , tags = p.tags
    }


catalogValue : Loadable Catalog -> Maybe Catalog
catalogValue loadable =
    case loadable of
        LoadingNow ->
            Nothing

        LoadFailed _ ->
            Nothing

        LoadDone catalog ->
            Just catalog


orElse : Maybe a -> Maybe a -> Maybe a
orElse fallback m =
    case m of
        Just _ ->
            m

        Nothing ->
            fallback


parseIntOr : Int -> String -> Int
parseIntOr fallback raw =
    String.toInt raw |> Maybe.withDefault fallback


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl s ->
            "bad URL: " ++ s

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "network error"

        Http.BadStatus n ->
            "HTTP " ++ String.fromInt n

        Http.BadBody s ->
            "bad body: " ++ s



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ h1 [] [ text "Airbnb Prompt Booker" ]
        , p []
            [ text "Type a prompt to auto-fill the form and rank matching listings, or use the form directly. "
            , text "Click "
            , code [] [ text "Book on Airbnb" ]
            , text " to land on the listing's reservation page."
            ]
        , viewSlmBanner model
        , viewSlmError model
        , viewBody model
        ]


viewSlmBanner : Model -> Html Msg
viewSlmBanner model =
    case model.slmState of
        SlmIdle ->
            div [ class "slm-banner slm-idle" ]
                [ text "Heuristic-only mode. The 350MB on-device model loads only when a prompt needs it." ]

        SlmLoading p ->
            div [ class "slm-banner slm-loading" ]
                [ text ("Loading on-device model... " ++ progressText p ++ ". " ++ p.text) ]

        SlmReady ->
            div [ class "slm-banner slm-ready" ]
                [ text "On-device model ready. Prompts run locally; no data leaves your machine." ]

        SlmFailed reason ->
            div [ class "slm-banner slm-error" ]
                [ text ("On-device model unavailable (" ++ reason ++ "). Falling back to heuristic-only mode.") ]


viewSlmError : Model -> Html Msg
viewSlmError model =
    case model.slmLastError of
        Just err ->
            div [ class "slm-error-notice" ]
                [ text ("Last on-device parse failed: " ++ truncate 140 err ++ ". Form shows heuristic-only result.") ]

        Nothing ->
            text ""


truncate : Int -> String -> String
truncate n s =
    if String.length s <= n then
        s

    else
        String.left n s ++ "..."


progressText : { progress : Float, text : String } -> String
progressText p =
    String.fromInt (round (p.progress * 100)) ++ "%"


viewBody : Model -> Html Msg
viewBody model =
    model.today
        |> Maybe.map (viewWithToday model)
        |> Maybe.withDefault (p [] [ text "Reading today's date..." ])


viewWithToday : Model -> Date -> Html Msg
viewWithToday model today =
    case model.catalog of
        LoadingNow ->
            p [] [ text "Loading catalog..." ]

        LoadFailed reason ->
            p [ class "error" ] [ text ("Failed to load catalog: " ++ reason) ]

        LoadDone catalog ->
            viewForm catalog today model


viewForm : Catalog -> Date -> Model -> Html Msg
viewForm catalog today model =
    let
        selectedListing =
            model.selectedListing
                |> Maybe.andThen (\id -> Catalog.findById id catalog)

        intentResult =
            buildIntent today model selectedListing
    in
    div [ class "form" ]
        [ viewPromptArea model
        , viewExamplesRow
        , h2 [] [ text "Listings" ]
        , viewListingCards catalog model
        , h2 [] [ text "Stay" ]
        , label []
            [ text "Check in"
            , input [ type_ "date", value model.checkIn, onInput ChangedCheckIn ] []
            ]
        , label []
            [ text "Check out"
            , input [ type_ "date", value model.checkOut, onInput ChangedCheckOut ] []
            ]
        , viewDateShift model
        , numberInput "Adults" model.adults ChangedAdults
        , numberInput "Children" model.children ChangedChildren
        , numberInput "Infants" model.infants ChangedInfants
        , numberInput "Pets" model.pets ChangedPets
        , viewBookButton intentResult
        ]


viewListingCards : Catalog -> Model -> Html Msg
viewListingCards catalog model =
    let
        filters =
            model.mergedPartial |> Maybe.map filtersFromPartial

        ranked =
            rankListings catalog model filters

        showingFallback =
            isJust filters && List.isEmpty ranked

        toShow =
            if List.isEmpty ranked then
                Catalog.all catalog

            else
                ranked

        cards =
            List.take maxCards toShow

        notice =
            if showingFallback then
                div [ class "no-match-notice" ]
                    [ text "No catalog listing matches all the constraints in your prompt; showing the full catalog instead." ]

            else
                text ""
    in
    div [ class "listing-section" ]
        [ notice
        , div [ class "listing-cards" ]
            (List.map (viewResultCard model.selectedListing) cards)
        ]


rankListings : Catalog -> Model -> Maybe Catalog.Filters -> List Listing
rankListings catalog model maybeFilters =
    case maybeFilters of
        Nothing ->
            Catalog.all catalog

        Just f ->
            case ( model.listingCorpus, model.promptVector ) of
                ( CorpusReady idx, Just vec ) ->
                    Catalog.matchAgainstSemantic f vec idx catalog

                ( CorpusReady _, Nothing ) ->
                    Catalog.matchAgainst f catalog

                ( CorpusEmpty, _ ) ->
                    Catalog.matchAgainst f catalog

                ( CorpusBuilding _, _ ) ->
                    Catalog.matchAgainst f catalog

                ( CorpusFailed _, _ ) ->
                    Catalog.matchAgainst f catalog


viewResultCard : Maybe ListingId -> Listing -> Html Msg
viewResultCard maybeSelected listing =
    let
        isSelected =
            maybeSelected
                |> Maybe.map (\sid -> Listing.idToString sid == Listing.idToString listing.id)
                |> Maybe.withDefault False
    in
    button
        [ classList
            [ ( "listing-card", True )
            , ( "selected", isSelected )
            ]
        , type_ "button"
        , onClick (SelectedListing listing.id)
        ]
        [ h3 [ class "card-title" ] [ text listing.title ]
        , div [ class "card-meta" ] [ text (listing.city ++ ", " ++ listing.country) ]
        , div [ class "card-meta" ] [ text ("Sleeps " ++ String.fromInt listing.maxGuests ++ petsLabel listing.petsAllowed) ]
        , div [ class "card-tags" ] (List.map viewTagChip listing.tags)
        ]


petsLabel : Bool -> String
petsLabel allowed =
    if allowed then
        " - pets ok"

    else
        ""


viewTagChip : Tag -> Html Msg
viewTagChip t =
    span [ class "tag-chip" ] [ text (Listing.tagToString t) ]


viewDateShift : Model -> Html Msg
viewDateShift model =
    let
        canShift =
            not (String.isEmpty model.checkIn) && not (String.isEmpty model.checkOut)
    in
    if canShift then
        div [ class "date-shift" ]
            [ button [ onClick (ShiftDates -7), type_ "button", class "example" ]
                [ text "« 1 wk" ]
            , button [ onClick (ShiftDates 7), type_ "button", class "example" ]
                [ text "1 wk »" ]
            , if model.datesFromSlm then
                span [ class "tentative-hint" ]
                    [ text "Dates suggested from prompt; if booked, shift to try a different week." ]

              else
                text ""
            ]

    else
        text ""


viewPromptArea : Model -> Html Msg
viewPromptArea model =
    label []
        [ text "Describe your trip"
        , textarea
            [ placeholder "e.g. \"long weekend in Lisbon for two in October\""
            , value model.prompt
            , onInput ChangedPrompt
            , rows 3
            , class "prompt"
            ]
            []
        ]


examplePrompts : List String
examplePrompts =
    [ "2 adults May 5-9 in Tulum, dog-friendly"
    , "long weekend in Lisbon for two in October"
    , "BCN early June, family of 4"
    , "Christmas week in Chamonix with my dog"
    , "weekend trip Kyoto, just me, workspace needed"
    , "couple Easter Tulum"
    , "barca for two, mid-September, 5 nights"
    , "beach trip late August Tulum, dog OK"
    ]


viewExamplesRow : Html Msg
viewExamplesRow =
    div [ class "examples" ]
        (span [ class "examples-label" ] [ text "Try: " ]
            :: List.map exampleButton examplePrompts
        )


exampleButton : String -> Html Msg
exampleButton prompt =
    button
        [ onClick (ChangedPrompt prompt)
        , class "example"
        , type_ "button"
        ]
        [ text prompt ]


numberInput : String -> Int -> (String -> Msg) -> Html Msg
numberInput labelText current toMsg =
    label []
        [ text labelText
        , input
            [ type_ "number"
            , Attr.min "0"
            , value (String.fromInt current)
            , onInput toMsg
            ]
            []
        ]


viewBookButton : Result String BookingIntent -> Html Msg
viewBookButton intentResult =
    case intentResult of
        Ok intent ->
            a
                [ href (DeepLink.build intent)
                , target "_blank"
                , rel "noopener noreferrer"
                , class "book-button"
                ]
                [ text "Book on Airbnb" ]

        Err reason ->
            div [ class "book-disabled" ]
                [ text ("Cannot book yet: " ++ reason) ]


buildIntent : Date -> Model -> Maybe Listing -> Result String BookingIntent
buildIntent today model maybeSelected =
    maybeSelected
        |> Result.fromMaybe "select a listing"
        |> Result.andThen
            (\listing ->
                Result.map2 Tuple.pair
                    (parseDateRange today model.checkIn model.checkOut)
                    (parseGuestCount model)
                    |> Result.andThen
                        (\( dates, guests ) ->
                            Booking.makeBookingIntent listing dates guests
                                |> Result.mapError Booking.errorToString
                        )
            )


parseDateRange : Date -> String -> String -> Result String DateRange
parseDateRange today checkInRaw checkOutRaw =
    Result.map2 Tuple.pair
        (Date.fromIsoString checkInRaw |> Result.mapError (\_ -> "check-in date is invalid"))
        (Date.fromIsoString checkOutRaw |> Result.mapError (\_ -> "check-out date is invalid"))
        |> Result.andThen
            (\( ci, co ) ->
                Booking.makeDateRange { today = today, checkIn = ci, checkOut = co }
                    |> Result.mapError Booking.errorToString
            )


parseGuestCount : Model -> Result String GuestCount
parseGuestCount model =
    Booking.makeGuestCount
        { adults = model.adults
        , children = model.children
        , infants = model.infants
        , pets = model.pets
        }
        |> Result.mapError Booking.errorToString



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Slm.statusSubscription GotSlmStatus
        , Slm.responseSubscription GotSlmResponse
        , Embed.responseSubscription GotEmbedResponse
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
