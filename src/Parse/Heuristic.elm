module Parse.Heuristic exposing (PartialQuery, parse)

import Date exposing (Date)
import Domain.Catalog as Catalog exposing (Catalog)
import Domain.Listing exposing (Tag(..))
import Regex exposing (Regex)


type alias PartialQuery =
    { destination : Maybe String
    , checkIn : Maybe Date
    , checkOut : Maybe Date
    , adults : Maybe Int
    , children : Maybe Int
    , infants : Maybe Int
    , pets : Maybe Int
    , tags : List Tag
    }


parse : Date -> Catalog -> String -> PartialQuery
parse today catalog input =
    let
        text =
            String.toLower (String.trim input)

        ( ci, co ) =
            parseDateRange today text
    in
    { destination = parseDestination catalog text
    , checkIn = ci
    , checkOut = co
    , adults = parseAdults text
    , children = parseExplicitCount text "children|kids"
    , infants = parseExplicitCount text "infants?|babies|baby"
    , pets = parsePets text
    , tags = parseTags text
    }



-- HELPERS


rgx : String -> Regex
rgx s =
    Regex.fromString s |> Maybe.withDefault Regex.never


firstJust : List (Maybe a) -> Maybe a
firstJust =
    List.filterMap identity >> List.head


orElse : Maybe a -> Maybe a -> Maybe a
orElse fallback m =
    case m of
        Just _ ->
            m

        Nothing ->
            fallback


submatch : Int -> Regex.Match -> Maybe String
submatch i m =
    m.submatches
        |> List.drop i
        |> List.head
        |> Maybe.andThen identity


numberWord : String -> Maybe Int
numberWord w =
    case w of
        "one" ->
            Just 1

        "two" ->
            Just 2

        "three" ->
            Just 3

        "four" ->
            Just 4

        "five" ->
            Just 5

        "six" ->
            Just 6

        "seven" ->
            Just 7

        "eight" ->
            Just 8

        "nine" ->
            Just 9

        "ten" ->
            Just 10

        _ ->
            Nothing


parseIntFlex : String -> Maybe Int
parseIntFlex s =
    String.toInt s |> orElse (numberWord s)



-- COUNTS


countWordPart : String
countWordPart =
    "(?:one|two|three|four|five|six|seven|eight|nine|ten|\\d+)"


parseExplicitCount : String -> String -> Maybe Int
parseExplicitCount text labelAlternation =
    let
        pattern =
            "\\b(" ++ countWordPart ++ ")\\s+(?:" ++ labelAlternation ++ ")\\b"
    in
    Regex.find (rgx pattern) text
        |> List.head
        |> Maybe.andThen (submatch 0)
        |> Maybe.andThen parseIntFlex


parseAdults : String -> Maybe Int
parseAdults text =
    firstJust
        [ parseExplicitCount text "adults?|grown-?ups?|guests?|people"
        , parseCouple text
        , parseSolo text
        ]


parseCouple : String -> Maybe Int
parseCouple text =
    let
        couplePatterns =
            [ "\\bcouple\\b"
            , "\\bme and my (?:partner|wife|husband|spouse|girlfriend|boyfriend|so)\\b"
            , "\\bus two\\b"
            , "\\btwo of us\\b"
            , "\\bfor two\\b"
            ]

        any =
            List.any (\p -> Regex.contains (rgx p) text) couplePatterns
    in
    if any then
        Just 2

    else
        Nothing


parseSolo : String -> Maybe Int
parseSolo text =
    let
        soloPatterns =
            [ "\\bsolo\\b"
            , "\\bjust me\\b"
            , "\\balone\\b"
            , "\\bby myself\\b"
            ]

        any =
            List.any (\p -> Regex.contains (rgx p) text) soloPatterns
    in
    if any then
        Just 1

    else
        Nothing


parsePets : String -> Maybe Int
parsePets text =
    let
        explicit =
            parseExplicitCount text "pets?|dogs?|cats?"

        implicit =
            if Regex.contains (rgx "\\b(?:my|our|with my|with our|bring my|bring our)\\s+(?:dog|cat|pet)s?\\b") text then
                Just 1

            else
                Nothing
    in
    firstJust [ explicit, implicit ]



-- TAGS


parseTags : String -> List Tag
parseTags text =
    let
        synonymTable =
            [ ( Beachfront, [ "beachfront", "beach", "ocean", "oceanfront", "seaside", "seafront", "by the sea" ] )
            , ( PetFriendly, [ "pet-friendly", "pet friendly", "dog-friendly", "dog friendly", "pets ok", "pets allowed", "dog ok", "cat ok" ] )
            , ( Wifi, [ "wifi", "wi-fi", "internet" ] )
            , ( Workspace, [ "workspace", "remote work", "work-friendly", "work friendly", "wfh", "work from home", "office", "desk" ] )
            , ( Pool, [ "pool", "swimming", "swim" ] )
            , ( Mountain, [ "mountain", "ski", "alpine", "lodge", "chalet" ] )
            , ( CityCenter, [ "downtown", "city center", "city centre", "central", "in town" ] )
            , ( Quiet, [ "quiet", "peaceful", "secluded", "tranquil", "off the grid", "off-grid" ] )
            ]

        explicit =
            synonymTable
                |> List.filterMap
                    (\( tag, syns ) ->
                        if List.any (\s -> String.contains s text) syns then
                            Just tag

                        else
                            Nothing
                    )

        implicitPet =
            if Regex.contains (rgx "\\b(?:dog|cat|pet)s?\\b") text then
                [ PetFriendly ]

            else
                []
    in
    dedupTags (explicit ++ implicitPet)


dedupTags : List Tag -> List Tag
dedupTags =
    List.foldr
        (\t acc ->
            if List.member t acc then
                acc

            else
                t :: acc
        )
        []



-- DESTINATION


parseDestination : Catalog -> String -> Maybe String
parseDestination catalog text =
    Catalog.all catalog
        |> List.map .city
        |> distinct
        |> List.sortBy (String.length >> negate)
        |> List.filter (\city -> String.contains (String.toLower city) text)
        |> List.head


distinct : List String -> List String
distinct =
    List.foldr
        (\x acc ->
            if List.member x acc then
                acc

            else
                x :: acc
        )
        []



-- DATES


parseDateRange : Date -> String -> ( Maybe Date, Maybe Date )
parseDateRange today text =
    [ parseIsoRange text
    , parseSameMonthRange today text
    , parseCrossMonthRange today text
    , parseDayFirstRange today text
    ]
        |> List.filterMap identity
        |> List.head
        |> Maybe.map (\( a, b ) -> ( Just a, Just b ))
        |> Maybe.withDefault ( Nothing, Nothing )


monthAlt : String
monthAlt =
    "jan(?:uary)?|feb(?:ruary)?|mar(?:ch)?|apr(?:il)?|may|jun(?:e)?|jul(?:y)?|aug(?:ust)?|sep(?:t(?:ember)?)?|oct(?:ober)?|nov(?:ember)?|dec(?:ember)?"


separatorAlt : String
separatorAlt =
    "(?:\\s*(?:to|through|until|-|–)\\s*)"


parseIsoRange : String -> Maybe ( Date, Date )
parseIsoRange text =
    let
        pattern =
            "(\\d{4}-\\d{2}-\\d{2})" ++ separatorAlt ++ "(\\d{4}-\\d{2}-\\d{2})"
    in
    Regex.find (rgx pattern) text
        |> List.head
        |> Maybe.andThen
            (\m ->
                Maybe.map2 Tuple.pair
                    (submatch 0 m |> Maybe.andThen (Date.fromIsoString >> Result.toMaybe))
                    (submatch 1 m |> Maybe.andThen (Date.fromIsoString >> Result.toMaybe))
            )


parseSameMonthRange : Date -> String -> Maybe ( Date, Date )
parseSameMonthRange today text =
    let
        pattern =
            "\\b(" ++ monthAlt ++ ")\\s+(\\d{1,2})" ++ separatorAlt ++ "(\\d{1,2})\\b"
    in
    Regex.find (rgx pattern) text
        |> List.head
        |> Maybe.andThen
            (\m ->
                let
                    monthM =
                        submatch 0 m |> Maybe.andThen monthNumber

                    d1M =
                        submatch 1 m |> Maybe.andThen String.toInt

                    d2M =
                        submatch 2 m |> Maybe.andThen String.toInt
                in
                Maybe.map3
                    (\month d1 d2 ->
                        let
                            y =
                                inferYear today month d1
                        in
                        Maybe.map2 Tuple.pair (makeDate y month d1) (makeDate y month d2)
                    )
                    monthM
                    d1M
                    d2M
                    |> Maybe.andThen identity
            )


parseCrossMonthRange : Date -> String -> Maybe ( Date, Date )
parseCrossMonthRange today text =
    let
        pattern =
            "\\b(" ++ monthAlt ++ ")\\s+(\\d{1,2})" ++ separatorAlt ++ "(" ++ monthAlt ++ ")\\s+(\\d{1,2})\\b"
    in
    Regex.find (rgx pattern) text
        |> List.head
        |> Maybe.andThen
            (\m ->
                let
                    m1 =
                        submatch 0 m |> Maybe.andThen monthNumber

                    d1 =
                        submatch 1 m |> Maybe.andThen String.toInt

                    m2 =
                        submatch 2 m |> Maybe.andThen monthNumber

                    d2 =
                        submatch 3 m |> Maybe.andThen String.toInt
                in
                Maybe.map4
                    (\mo1 da1 mo2 da2 ->
                        let
                            y1 =
                                inferYear today mo1 da1

                            y2 =
                                if mo2 < mo1 then
                                    y1 + 1

                                else
                                    y1
                        in
                        Maybe.map2 Tuple.pair (makeDate y1 mo1 da1) (makeDate y2 mo2 da2)
                    )
                    m1
                    d1
                    m2
                    d2
                    |> Maybe.andThen identity
            )


parseDayFirstRange : Date -> String -> Maybe ( Date, Date )
parseDayFirstRange today text =
    let
        pattern =
            "\\b(\\d{1,2})" ++ separatorAlt ++ "(\\d{1,2})\\s+(" ++ monthAlt ++ ")\\b"
    in
    Regex.find (rgx pattern) text
        |> List.head
        |> Maybe.andThen
            (\m ->
                let
                    d1 =
                        submatch 0 m |> Maybe.andThen String.toInt

                    d2 =
                        submatch 1 m |> Maybe.andThen String.toInt

                    mo =
                        submatch 2 m |> Maybe.andThen monthNumber
                in
                Maybe.map3
                    (\da1 da2 month ->
                        let
                            y =
                                inferYear today month da1
                        in
                        Maybe.map2 Tuple.pair (makeDate y month da1) (makeDate y month da2)
                    )
                    d1
                    d2
                    mo
                    |> Maybe.andThen identity
            )


inferYear : Date -> Int -> Int -> Int
inferYear today month day =
    let
        thisYear =
            Date.year today
    in
    case makeDate thisYear month day of
        Just c ->
            if Date.compare c today == LT then
                thisYear + 1

            else
                thisYear

        Nothing ->
            thisYear


makeDate : Int -> Int -> Int -> Maybe Date
makeDate year month day =
    if year < 1 || month < 1 || month > 12 || day < 1 || day > 31 then
        Nothing

    else
        let
            d =
                Date.fromCalendarDate year (Date.numberToMonth month) day
        in
        if Date.day d == day && Date.monthNumber d == month then
            Just d

        else
            Nothing


monthNumber : String -> Maybe Int
monthNumber raw =
    case raw of
        "jan" ->
            Just 1

        "january" ->
            Just 1

        "feb" ->
            Just 2

        "february" ->
            Just 2

        "mar" ->
            Just 3

        "march" ->
            Just 3

        "apr" ->
            Just 4

        "april" ->
            Just 4

        "may" ->
            Just 5

        "jun" ->
            Just 6

        "june" ->
            Just 6

        "jul" ->
            Just 7

        "july" ->
            Just 7

        "aug" ->
            Just 8

        "august" ->
            Just 8

        "sep" ->
            Just 9

        "sept" ->
            Just 9

        "september" ->
            Just 9

        "oct" ->
            Just 10

        "october" ->
            Just 10

        "nov" ->
            Just 11

        "november" ->
            Just 11

        "dec" ->
            Just 12

        "december" ->
            Just 12

        _ ->
            Nothing
