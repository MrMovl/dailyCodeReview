module App exposing (..)

import Html exposing (..)
import CSS
import Time exposing (Time)
import Task
import Array exposing (Array)


type alias Dev =
    { name : String, color : String, fontColor : String }


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
        , model.today |> nextTwoWeeks |> timeToDays |> dayToDevs |> printDevs
        ]


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


dayToDevs : List Int -> List (Maybe Dev)
dayToDevs =
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
