module App exposing (..)

import Html exposing (..)
import CSS
import Date exposing (Date)
import Time exposing (Time)
import Task


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
    ( { today = 0 |> Date.fromTime }, Task.perform Today Date.now )


devs : List String
devs =
    [ "Tomke", "Gregor", "Jonas", "Jens", "Tim", "Daniel" ]



-- UPDATE


type Msg
    = Today Date


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
        , model.today |> prettyDate |> text
        ]


nextTwoWeeks : Date -> List Date
nextTwoWeeks today =
    let
        fourteenDays =
            List.range 0 13
        fromToday = addDay today
    in
        List.map addDay fourteenDays


addDay : Date -> Int -> Date
addDay today i =
    

dayList : Date -> Html Msg
dayList date =
    div [] [ date |> prettyDate |> text ]


prettyDate : Date -> String
prettyDate date =
    (toString (Date.day date)) ++ ". " ++ (toString (Date.month date)) ++ " " ++ (toString (Date.year date))
