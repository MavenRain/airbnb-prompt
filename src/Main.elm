module Main exposing (main)

import Browser
import Date exposing (Date)
import Domain.Booking as Booking exposing (BookingIntent, DateRange, GuestCount)
import Domain.Catalog as Catalog exposing (Catalog)
import Domain.Listing as Listing exposing (Listing, ListingId)
import Html exposing (Html, a, code, div, h1, input, label, option, p, select, text)
import Html.Attributes as Attr exposing (class, href, rel, selected, target, type_, value)
import Html.Events exposing (onInput)
import Http
import Task
import Url.AirbnbDeepLink as DeepLink


type Loadable a
    = LoadingNow
    | LoadFailed String
    | LoadDone a


type alias Model =
    { catalog : Loadable Catalog
    , today : Maybe Date
    , selectedListing : Maybe ListingId
    , checkIn : String
    , checkOut : String
    , adults : Int
    , children : Int
    , infants : Int
    , pets : Int
    }


type Msg
    = GotCatalog (Result Http.Error Catalog)
    | GotToday Date
    | SelectedListing String
    | ChangedCheckIn String
    | ChangedCheckOut String
    | ChangedAdults String
    | ChangedChildren String
    | ChangedInfants String
    | ChangedPets String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { catalog = LoadingNow
      , today = Nothing
      , selectedListing = Nothing
      , checkIn = ""
      , checkOut = ""
      , adults = 1
      , children = 0
      , infants = 0
      , pets = 0
      }
    , Cmd.batch
        [ Http.get
            { url = "catalog.json"
            , expect = Http.expectJson GotCatalog Catalog.decoder
            }
        , Task.perform GotToday Date.today
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCatalog (Ok catalog) ->
            ( { model | catalog = LoadDone catalog }, Cmd.none )

        GotCatalog (Err err) ->
            ( { model | catalog = LoadFailed (httpErrorToString err) }, Cmd.none )

        GotToday today ->
            ( { model | today = Just today }, Cmd.none )

        SelectedListing raw ->
            ( { model | selectedListing = nonEmpty raw |> Maybe.map Listing.idFromString }, Cmd.none )

        ChangedCheckIn raw ->
            ( { model | checkIn = raw }, Cmd.none )

        ChangedCheckOut raw ->
            ( { model | checkOut = raw }, Cmd.none )

        ChangedAdults raw ->
            ( { model | adults = parseIntOr model.adults raw }, Cmd.none )

        ChangedChildren raw ->
            ( { model | children = parseIntOr model.children raw }, Cmd.none )

        ChangedInfants raw ->
            ( { model | infants = parseIntOr model.infants raw }, Cmd.none )

        ChangedPets raw ->
            ( { model | pets = parseIntOr model.pets raw }, Cmd.none )


nonEmpty : String -> Maybe String
nonEmpty s =
    if String.isEmpty s then
        Nothing

    else
        Just s


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


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ h1 [] [ text "Airbnb Prompt Booker (Phase 0)" ]
        , p []
            [ text "Pick a listing, set dates and guests, then click "
            , code [] [ text "Book on Airbnb" ]
            , text " to land on the reservation page."
            ]
        , viewBody model
        ]


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
        listings =
            Catalog.all catalog

        selectedListing =
            model.selectedListing
                |> Maybe.andThen (\id -> Catalog.findById id catalog)

        intentResult =
            buildIntent today model selectedListing
    in
    div [ class "form" ]
        [ label []
            [ text "Listing"
            , select [ onInput SelectedListing ]
                (option [ value "" ] [ text "Choose a property..." ]
                    :: List.map (viewListingOption model.selectedListing) listings
                )
            ]
        , label []
            [ text "Check in"
            , input
                [ type_ "date"
                , value model.checkIn
                , onInput ChangedCheckIn
                ]
                []
            ]
        , label []
            [ text "Check out"
            , input
                [ type_ "date"
                , value model.checkOut
                , onInput ChangedCheckOut
                ]
                []
            ]
        , numberInput "Adults" model.adults ChangedAdults
        , numberInput "Children" model.children ChangedChildren
        , numberInput "Infants" model.infants ChangedInfants
        , numberInput "Pets" model.pets ChangedPets
        , viewBookButton intentResult
        ]


viewListingOption : Maybe ListingId -> Listing -> Html Msg
viewListingOption maybeSelected listing =
    let
        idStr =
            Listing.idToString listing.id

        isSelected =
            maybeSelected
                |> Maybe.map (\sid -> Listing.idToString sid == idStr)
                |> Maybe.withDefault False
    in
    option [ value idStr, selected isSelected ]
        [ text (listing.title ++ " - " ++ listing.city ++ ", " ++ listing.country) ]


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


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
