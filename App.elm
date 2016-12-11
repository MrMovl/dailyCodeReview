module App exposing (..)

import Html exposing (..)
import CSS
import Date exposing (Date)
import Time exposing (Time)
import Task
import Array


{-
   umwandeln in DaysSinceEpoch, modulo mit devs.length, dadurch Liste von Indizes zum rendern der Dev Felder

-}


type alias Dev =
    { name : String, color : String }


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


type alias Model =
    { today : Time }


init : ( Model, Cmd Msg )
init =
    ( { today = 0 }, Task.perform Today Time.now )


devs : Array.Array Dev
devs =
    [ { name = "Tomke", color = "Green" }
    , { name = "Gregor", color = "Purple" }
    , { name = "Jonas", color = "Red" }
    , { name = "Jens", color = "Black" }
    , { name = "Tim", color = "Yellow" }
    , { name = "Daniel", color = "Orange" }
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
        , model.today |> nextTwoWeeks |> toDays |> toDevs |> printDevs
        ]


printDevs : List Dev -> Html Msg
printDevs devs =
    div [] (List.map (\dev -> dev.name ++ "\n" |> text) devs)


toDevs : List Int -> List Dev
toDevs =
    let
        devCount =
            Array.length devs
    in
        List.map (\day -> day |> flipedModulo devCount |> getDev devs)


getDev devs i =
    Array.get i devs |> Maybe.withDefault { name = "Tomke", color = "Green" }


toDays : List Time -> List Int
toDays =
    List.map (\x -> Time.inHours x |> flipedDevide 24 |> floor)


flipedModulo =
    flip (%)


flipedDevide : Float -> Float -> Float
flipedDevide =
    flip (/)


printTimeList : List Int -> Html Msg
printTimeList list =
    div [] (List.map (\x -> x |> toString |> (++) "\n" |> text) list)


nextTwoWeeks : Time -> List Time
nextTwoWeeks today =
    let
        fourteenDays =
            List.range 0 13
    in
        List.map (\number -> today + ((toFloat number) * msPerDay)) fourteenDays


prettyDate : Date -> String
prettyDate date =
    (toString (Date.day date)) ++ ". " ++ (toString (Date.month date)) ++ " " ++ (toString (Date.year date))
