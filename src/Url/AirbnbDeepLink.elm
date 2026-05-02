module Url.AirbnbDeepLink exposing (build)

import Date
import Domain.Booking as Booking exposing (BookingIntent)
import Domain.Listing as Listing
import Url.Builder


build : BookingIntent -> String
build intent =
    let
        listingIdStr =
            Listing.idToString (Booking.intentListing intent)

        dates =
            Booking.intentDates intent

        guests =
            Booking.intentGuests intent
    in
    Url.Builder.crossOrigin
        "https://www.airbnb.com"
        [ "rooms", listingIdStr ]
        [ Url.Builder.string "check_in" (Date.toIsoString (Booking.dateRangeCheckIn dates))
        , Url.Builder.string "check_out" (Date.toIsoString (Booking.dateRangeCheckOut dates))
        , Url.Builder.int "adults" (Booking.guestAdults guests)
        , Url.Builder.int "children" (Booking.guestChildren guests)
        , Url.Builder.int "infants" (Booking.guestInfants guests)
        , Url.Builder.int "pets" (Booking.guestPets guests)
        ]
