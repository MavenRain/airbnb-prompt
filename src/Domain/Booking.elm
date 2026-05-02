module Domain.Booking exposing
    ( BookingIntent
    , DateRange
    , Error(..)
    , GuestCount
    , dateRangeCheckIn
    , dateRangeCheckOut
    , errorToString
    , guestAdults
    , guestChildren
    , guestInfants
    , guestPets
    , intentDates
    , intentGuests
    , intentListing
    , makeBookingIntent
    , makeDateRange
    , makeGuestCount
    )

import Date exposing (Date)
import Domain.Listing as Listing exposing (Listing, ListingId)


type DateRange
    = DateRange { checkIn : Date, checkOut : Date }


type GuestCount
    = GuestCount
        { adults : Int
        , children : Int
        , infants : Int
        , pets : Int
        }


type BookingIntent
    = BookingIntent
        { listing : ListingId
        , dates : DateRange
        , guests : GuestCount
        }


type Error
    = CheckOutBeforeCheckIn
    | CheckInInPast
    | NoAdults
    | InfantsExceedAdults
    | GuestsExceedListingCapacity


makeDateRange :
    { today : Date, checkIn : Date, checkOut : Date }
    -> Result Error DateRange
makeDateRange { today, checkIn, checkOut } =
    if Date.compare checkIn today == LT then
        Err CheckInInPast

    else if Date.compare checkOut checkIn /= GT then
        Err CheckOutBeforeCheckIn

    else
        Ok (DateRange { checkIn = checkIn, checkOut = checkOut })


makeGuestCount :
    { adults : Int, children : Int, infants : Int, pets : Int }
    -> Result Error GuestCount
makeGuestCount r =
    if r.adults < 1 then
        Err NoAdults

    else if r.infants > r.adults then
        Err InfantsExceedAdults

    else
        Ok (GuestCount r)


makeBookingIntent : Listing -> DateRange -> GuestCount -> Result Error BookingIntent
makeBookingIntent listing dates guests =
    if guestAdults guests + guestChildren guests > listing.maxGuests then
        Err GuestsExceedListingCapacity

    else
        Ok
            (BookingIntent
                { listing = listing.id
                , dates = dates
                , guests = guests
                }
            )


intentListing : BookingIntent -> ListingId
intentListing (BookingIntent r) =
    r.listing


intentDates : BookingIntent -> DateRange
intentDates (BookingIntent r) =
    r.dates


intentGuests : BookingIntent -> GuestCount
intentGuests (BookingIntent r) =
    r.guests


dateRangeCheckIn : DateRange -> Date
dateRangeCheckIn (DateRange r) =
    r.checkIn


dateRangeCheckOut : DateRange -> Date
dateRangeCheckOut (DateRange r) =
    r.checkOut


guestAdults : GuestCount -> Int
guestAdults (GuestCount g) =
    g.adults


guestChildren : GuestCount -> Int
guestChildren (GuestCount g) =
    g.children


guestInfants : GuestCount -> Int
guestInfants (GuestCount g) =
    g.infants


guestPets : GuestCount -> Int
guestPets (GuestCount g) =
    g.pets


errorToString : Error -> String
errorToString err =
    case err of
        CheckOutBeforeCheckIn ->
            "check-out must be after check-in"

        CheckInInPast ->
            "check-in cannot be in the past"

        NoAdults ->
            "at least one adult is required"

        InfantsExceedAdults ->
            "infants cannot exceed adults (Airbnb rule)"

        GuestsExceedListingCapacity ->
            "this listing cannot accommodate that many guests"
