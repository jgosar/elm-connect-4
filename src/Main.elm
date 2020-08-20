module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser
import Html exposing (Attribute, Html, div, node)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { field : Array (Array Int)
    , nextToken : Int
    }


init : Model
init =
    { field = Array.initialize 6 (always emptyRow), nextToken = 1 }


emptyRow : Array Int
emptyRow =
    Array.initialize 7 (always 0)



-- UPDATE


type Msg
    = ColumnClick Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        ColumnClick columnIndex ->
            { model
                | nextToken = 3 - model.nextToken
                , field = throwToken model.nextToken columnIndex model.field
            }


throwToken : Int -> Int -> Array (Array Int) -> Array (Array Int)
throwToken tokenType columnIndex field =
    if getLowestFreeCell columnIndex field /= Nothing then
        updateCell columnIndex (Maybe.withDefault 0 (getLowestFreeCell columnIndex field)) tokenType field

    else
        field


getLowestFreeCell : Int -> Array (Array Int) -> Maybe Int
getLowestFreeCell columnIndex field =
    getLowestFreeCellAboveX columnIndex field 5


getLowestFreeCellAboveX : Int -> Array (Array Int) -> Int -> Maybe Int
getLowestFreeCellAboveX columnIndex field maxIndex =
    if maxIndex < 0 then
        Nothing

    else if getCellValue columnIndex maxIndex field == 0 then
        Just maxIndex

    else
        getLowestFreeCellAboveX columnIndex field (maxIndex - 1)


getCellValue : Int -> Int -> Array (Array Int) -> Int
getCellValue columnIndex rowIndex field =
    Maybe.withDefault 0 (Array.get columnIndex (getRow rowIndex field))


updateCell : Int -> Int -> Int -> Array (Array Int) -> Array (Array Int)
updateCell columnIndex rowIndex newValue field =
    updateRow rowIndex (Array.set columnIndex newValue) field


updateRow : Int -> (Array Int -> Array Int) -> Array (Array Int) -> Array (Array Int)
updateRow rowIndex updateFunction field =
    Array.set rowIndex (updateFunction (getRow rowIndex field)) field


getRow : Int -> Array (Array Int) -> Array Int
getRow rowIndex field =
    Maybe.withDefault emptyRow (Array.get rowIndex field)



-- VIEW


css path =
    node "link" [ rel "stylesheet", href path ] []


gridView : Array (Array Int) -> Html Msg
gridView grid =
    div [ class "connect_4__grid" ] (Array.toList (Array.map (\row -> rowView row) grid))


rowView : Array Int -> Html Msg
rowView row =
    div [ class "connect_4__row" ] (Array.toList (Array.indexedMap (\i cellValue -> cellView i cellValue) row))


cellView : Int -> Int -> Html Msg
cellView index cellValue =
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
            [ gridView model.field
            ]
        ]
