module App exposing (..)

import Html exposing (..)
import CSS
import Time exposing (Time)
import Date
import Task
import Array exposing (Array)


msPerDay : Time
msPerDay =
    86400000


dayRange : Int
dayRange =
    15


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Dev =
    { name : String, color : String, fontColor : String }


type alias Model =
    { today : Time }


init : ( Model, Cmd Msg )
init =
    ( { today = 0 }, Task.perform Today Time.now )


devs : Array.Array Dev
devs =
    [ { name = "Tomke", color = "Green", fontColor = "Black" }
    , { name = "Gregor", color = "Purple", fontColor = "White" }
    , { name = "Jonas", color = "Red", fontColor = "Black" }
    , { name = "Jens", color = "Black", fontColor = "White" }
    , { name = "Tim", color = "Yellow", fontColor = "Black" }
    , { name = "Daniel", color = "Orange", fontColor = "Black" }
    ]
        |> Array.fromList



-- UPDATE


type Msg
    = Today Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Today today ->
            ( { model | today = today }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ CSS.body ]
        [ h1 [ CSS.header ] [ text "Daily Code Review" ]
        , div [ CSS.column ] [ model.today |> todayToPrintedDevs ]
        , div [ CSS.column ] [ model.today |> todayToPrintedDays ]
        ]


todayToPrintedDays : Time -> Html Msg
todayToPrintedDays today =
    today |> nextTwoWeeks |> withoutWeekends |> printDays


todayToPrintedDevs : Time -> Html Msg
todayToPrintedDevs today =
    today
        |> nextTwoWeeks
        |> accountForWeekends
        |> timeToDays
        |> daysToDevs
        |> printDevs


accountForWeekends : List Time -> List Time
accountForWeekends days =
    List.foldr skipWeekends days days


skipWeekends : Time -> List Time -> List Time
skipWeekends day days =
    if isWorkday day then
        days
    else
        days |> List.reverse |> List.drop 1 |> List.reverse


withoutWeekends : List Time -> List Time
withoutWeekends days =
    List.filter (\x -> x |> isWorkday) days


isWorkday : Time -> Bool
isWorkday time =
    let
        day =
            time |> Date.fromTime |> Date.dayOfWeek
    in
        day /= Date.Sun && day /= Date.Sat


printDays : List Time -> Html Msg
printDays days =
    div [] (List.map printDay days)


printDay : Time -> Html Msg
printDay time =
    div [] [ time |> Date.fromTime |> prettyDate |> text ]


nextTwoWeeks : Time -> List Time
nextTwoWeeks today =
    let
        fourteenDays =
            List.range 0 dayRange

        indexToDayFromToday =
            indexToDay today
    in
        List.map indexToDayFromToday fourteenDays


indexToDay : Time -> Int -> Time
indexToDay today i =
    today + ((toFloat i) * msPerDay)


timeToDays : List Time -> List Int
timeToDays =
    List.map (\x -> Time.inHours x |> flippedDevide 24 |> floor)


daysToDevs : List Int -> List (Maybe Dev)
daysToDevs =
    let
        devCount =
            Array.length devs

        indexToDev day =
            day |> flippedModulo devCount |> flippedGet devs
    in
        List.map indexToDev


printDevs : List (Maybe Dev) -> Html Msg
printDevs devs =
    div [] (List.map printDev devs)


printDev : Maybe Dev -> Html Msg
printDev dev =
    case dev of
        Just dev ->
            div
                [ CSS.backgroundColor dev.color dev.fontColor ]
                [ text dev.name, br [] [] ]

        Nothing ->
            div [] [ text "I don't know that guy" ]



--Helper


flippedModulo : Int -> Int -> Int
flippedModulo =
    flip (%)


flippedDevide : Float -> Float -> Float
flippedDevide =
    flip (/)


flippedGet : Array a -> Int -> Maybe a
flippedGet =
    flip Array.get


prettyDate : Date.Date -> String
prettyDate date =
    if (date |> Date.day |> toString) == "NaN" then
        "That's no date!"
    else
        (toString (Date.day date)) ++ ". " ++ (toString (Date.month date)) ++ " " ++ (toString (Date.year date))
