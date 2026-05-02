module Domain.Catalog exposing (Catalog, all, decoder, findById)

import Domain.Listing as Listing exposing (Listing, ListingId, listingDecoder)
import Json.Decode as D exposing (Decoder)


type Catalog
    = Catalog (List Listing)


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
