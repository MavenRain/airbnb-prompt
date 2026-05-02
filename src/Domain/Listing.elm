module Domain.Listing exposing
    ( Listing
    , ListingId
    , Tag(..)
    , idFromString
    , idToString
    , listingDecoder
    , tagFromString
    , tagToString
    )

import Json.Decode as D exposing (Decoder)


type ListingId
    = ListingId String


idFromString : String -> ListingId
idFromString =
    ListingId


idToString : ListingId -> String
idToString (ListingId raw) =
    raw


type Tag
    = Beachfront
    | PetFriendly
    | Wifi
    | Workspace
    | Pool
    | Mountain
    | CityCenter
    | Quiet


tagToString : Tag -> String
tagToString tag =
    case tag of
        Beachfront ->
            "beachfront"

        PetFriendly ->
            "pet-friendly"

        Wifi ->
            "wifi"

        Workspace ->
            "workspace"

        Pool ->
            "pool"

        Mountain ->
            "mountain"

        CityCenter ->
            "city-center"

        Quiet ->
            "quiet"


tagFromString : String -> Maybe Tag
tagFromString raw =
    case raw of
        "beachfront" ->
            Just Beachfront

        "pet-friendly" ->
            Just PetFriendly

        "wifi" ->
            Just Wifi

        "workspace" ->
            Just Workspace

        "pool" ->
            Just Pool

        "mountain" ->
            Just Mountain

        "city-center" ->
            Just CityCenter

        "quiet" ->
            Just Quiet

        _ ->
            Nothing


type alias Listing =
    { id : ListingId
    , title : String
    , city : String
    , country : String
    , maxGuests : Int
    , petsAllowed : Bool
    , tags : List Tag
    , thumbnailUrl : String
    }


listingDecoder : Decoder Listing
listingDecoder =
    D.map8 Listing
        (D.field "id" (D.map ListingId D.string))
        (D.field "title" D.string)
        (D.field "city" D.string)
        (D.field "country" D.string)
        (D.field "maxGuests" D.int)
        (D.field "petsAllowed" D.bool)
        (D.field "tags" (D.list tagDecoder))
        (D.field "thumbnailUrl" D.string)


tagDecoder : Decoder Tag
tagDecoder =
    D.string
        |> D.andThen
            (\raw ->
                tagFromString raw
                    |> Maybe.map D.succeed
                    |> Maybe.withDefault (D.fail ("Unknown tag: " ++ raw))
            )
