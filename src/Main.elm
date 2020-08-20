module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser
import Html exposing (Attribute, Html, br, div, input, node, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { content : String
    , field : Array (Array Int)
    }


emptyRow : Array Int
emptyRow =
    Array.initialize 7 (always 0)


init : Model
init =
    { content = "", field = Array.initialize 6 (always emptyRow) }



-- UPDATE


type Msg
    = Change String
    | ColumnClick Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | content = newContent }

        ColumnClick columnIndex ->
            { model | content = String.fromInt columnIndex }



-- VIEW


css path =
    node "link" [ rel "stylesheet", href path ] []


gridView : Array (Array Int) -> Html Msg
gridView grid =
    div [ class "connect_4__grid" ] (Array.toList (Array.map (\row -> rowView row) grid))


rowView : Array Int -> Html Msg
rowView row =
    div [ class "connect_4__row" ] (Array.toList (Array.indexedMap (\i cellValue -> cellView ( i, cellValue )) row))


cellView : ( Int, Int ) -> Html Msg
cellView ( index, cellValue ) =
    div (List.concat [ cellClass cellValue, [ onClick (ColumnClick index) ] ]) []


cellClass : Int -> List (Attribute Msg)
cellClass cellValue =
    [ class "connect_4__cell", class (String.concat [ "connect_4__cell--value_", String.fromInt cellValue ]) ]


view : Model -> Html Msg
view model =
    div []
        [ css "css/styles.css"
        , div
            [ class "connect_4__grid_container" ]
            [ input [ placeholder "Text to reverse", value model.content, onInput Change ] []
            , div [] [ text (String.reverse model.content) ]
            , gridView model.field
            ]
        ]
