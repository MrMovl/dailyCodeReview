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
        , div [ CSS.column ] [ model.today |> nextTwoWeeks |> timeToDays |> daysToDevs |> printDevs ]
        , div [ CSS.column ] [ model.today |> nextTwoWeeks |> printDays ]
        ]


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
            List.range 0 13
    in
        List.map (\number -> today + ((toFloat number) * msPerDay)) fourteenDays


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
