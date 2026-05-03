module Domain.Catalog exposing (Catalog, Filters, all, decoder, findById, matchAgainst, matchAgainstSemantic)

import Domain.Listing as Listing exposing (Listing, ListingId, Tag, listingDecoder)
import Json.Decode as D exposing (Decoder)
import Rag.Index as Rag


type Catalog
    = Catalog (List Listing)


type alias Filters =
    { destination : Maybe String
    , totalGuests : Int
    , petsRequested : Bool
    , tags : List Tag
    }


decoder : Decoder Catalog
decoder =
    D.field "listings" (D.list listingDecoder)
        |> D.map Catalog


all : Catalog -> List Listing
all (Catalog xs) =
    xs


findById : ListingId -> Catalog -> Maybe Listing
findById target (Catalog xs) =
    let
        targetStr =
            Listing.idToString target
    in
    xs
        |> List.filter (\l -> Listing.idToString l.id == targetStr)
        |> List.head


matchAgainst : Filters -> Catalog -> List Listing
matchAgainst filters (Catalog xs) =
    xs
        |> List.filter (passesHard filters)
        |> List.sortBy (\l -> -(softScore filters l))


passesHard : Filters -> Listing -> Bool
passesHard f l =
    let
        cityOk =
            case f.destination of
                Just c ->
                    String.toLower l.city == String.toLower c

                Nothing ->
                    True

        capacityOk =
            f.totalGuests <= l.maxGuests

        petsOk =
            not f.petsRequested || l.petsAllowed
    in
    cityOk && capacityOk && petsOk


softScore : Filters -> Listing -> Int
softScore f l =
    List.length (List.filter (\t -> List.member t f.tags) l.tags)


matchAgainstSemantic : Filters -> Rag.Vector -> Rag.Index Listing -> Catalog -> List Listing
matchAgainstSemantic filters promptVec idx (Catalog xs) =
    let
        hardIds =
            xs
                |> List.filter (passesHard filters)
                |> List.map (.id >> Listing.idToString)

        rankedAll =
            Rag.query promptVec (List.length xs) idx
    in
    rankedAll
        |> List.filter (\l -> List.member (Listing.idToString l.id) hardIds)
