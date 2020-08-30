module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array, fromList)
import Browser
import Html exposing (Attribute, Html, div, node, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


fieldHeight =
    6


fieldWidth =
    7


type alias Connect4Field =
    Array (Array Int)


type alias Model =
    { field : Connect4Field
    , nextToken : Int
    , redScore : Int
    , yellowScore : Int
    }


init : Model
init =
    { field = Array.initialize fieldHeight (always emptyRow), nextToken = 1, redScore = 0, yellowScore = 0 }


emptyRow : Array Int
emptyRow =
    Array.initialize fieldWidth (always 0)



-- UPDATE


type Msg
    = ColumnClick Int


type alias Coords =
    { column : Int
    , row : Int
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ColumnClick columnIndex ->
            let
                newField1 =
                    throwToken model.nextToken columnIndex model.field

                nextToken1 =
                    3 - model.nextToken

                computersMove =
                    getBestMove nextToken1 newField1 3

                newField2 =
                    throwToken nextToken1 (Tuple.first computersMove) newField1

                nextToken2 =
                    3 - nextToken1
            in
            { model
                | nextToken = nextToken2
                , field = newField2
                , redScore = totalScore 1 newField2
                , yellowScore = totalScore 2 newField2
            }


throwToken : Int -> Int -> Connect4Field -> Connect4Field
throwToken tokenType columnIndex field =
    let
        lowestFreeCell =
            getLowestFreeCell columnIndex field
    in
    if lowestFreeCell /= Nothing then
        updateCell { column = columnIndex, row = Maybe.withDefault 0 lowestFreeCell } tokenType field

    else
        field


getLowestFreeCell : Int -> Connect4Field -> Maybe Int
getLowestFreeCell columnIndex field =
    getLowestFreeCellAboveRow columnIndex field (fieldHeight - 1)


getLowestFreeCellAboveRow : Int -> Connect4Field -> Int -> Maybe Int
getLowestFreeCellAboveRow columnIndex field maxRow =
    if maxRow < 0 then
        Nothing

    else if getCellValue field { column = columnIndex, row = maxRow } == 0 then
        Just maxRow

    else
        getLowestFreeCellAboveRow columnIndex field (maxRow - 1)


getCellValue : Connect4Field -> Coords -> Int
getCellValue field coords =
    Maybe.withDefault 0 (Array.get coords.column (getRow coords.row field))


updateCell : Coords -> Int -> Connect4Field -> Connect4Field
updateCell coords newValue field =
    updateRow coords.row (Array.set coords.column newValue) field


updateRow : Int -> (Array Int -> Array Int) -> Connect4Field -> Connect4Field
updateRow rowIndex updateFunction field =
    Array.set rowIndex (updateFunction (getRow rowIndex field)) field


getRow : Int -> Connect4Field -> Array Int
getRow rowIndex field =
    Maybe.withDefault emptyRow (Array.get rowIndex field)



--SCORES


type ComboDirection
    = Right
    | RightDown
    | Down
    | LeftDown


totalScore : Int -> Connect4Field -> Int
totalScore tokenType field =
    List.sum (Array.toList (Array.map (cellScore field tokenType) getAllCoords))


getAllCoords : Array Coords
getAllCoords =
    flatten (Array.map getRowCoords (Array.fromList (List.range 0 (fieldHeight - 1))))


getRowCoords : Int -> Array Coords
getRowCoords row =
    Array.map (\column -> { column = column, row = row }) (Array.fromList (List.range 0 (fieldWidth - 1)))


cellScore : Connect4Field -> Int -> Coords -> Int
cellScore field tokenType coords =
    let
        directions =
            Array.fromList [ Right, RightDown, Down, LeftDown ]

        cellGroups =
            Array.map (\direction -> getCoordsList 4 direction field coords) directions
    in
    List.sum (Array.toList (Array.map (\cellGroup -> convertToScore (countGoodTokens tokenType field cellGroup)) cellGroups))


convertToScore : Int -> Int
convertToScore goodTokens =
    if goodTokens == 4 then
        1000000

    else if goodTokens == 3 then
        100

    else if goodTokens == 2 then
        10

    else if goodTokens == 1 then
        1

    else
        0


countGoodTokens : Int -> Connect4Field -> Maybe (Array Coords) -> Int
countGoodTokens tokenType field coordsList =
    case coordsList of
        Nothing ->
            0

        Just coordsListValue ->
            let
                tokens =
                    Array.map (getCellValue field) coordsListValue

                goodTokensCount =
                    Array.length (Array.filter (\token -> token == tokenType) tokens)

                badTokensCount =
                    Array.length (Array.filter (\token -> token /= tokenType && token /= 0) tokens)
            in
            if badTokensCount > 0 then
                0

            else
                goodTokensCount


getCoordsList : Int -> ComboDirection -> Connect4Field -> Coords -> Maybe (Array Coords)
getCoordsList length direction field coords =
    if length == 1 then
        Just (Array.fromList [ coords ])

    else
        let
            nextCoords =
                getNextCoords coords direction

            nextCoordsList =
                unMaybeMap (getCoordsList (length - 1) direction field) nextCoords
        in
        Maybe.map (Array.push coords) nextCoordsList


getNextCoords : Coords -> ComboDirection -> Maybe Coords
getNextCoords coords direction =
    if direction == Right then
        validateCoords { column = coords.column + 1, row = coords.row }

    else if direction == RightDown then
        validateCoords { column = coords.column + 1, row = coords.row + 1 }

    else if direction == Down then
        validateCoords { column = coords.column, row = coords.row + 1 }

    else if direction == LeftDown then
        validateCoords { column = coords.column - 1, row = coords.row + 1 }

    else
        Nothing


validateCoords : Coords -> Maybe Coords
validateCoords coords =
    if coords.column >= 0 && coords.column < fieldWidth && coords.row >= 0 && coords.row < fieldHeight then
        Just coords

    else
        Nothing



-- MOVE PLANNING


getNextMoveScores : Int -> Connect4Field -> Int -> Array ( Int, Int )
getNextMoveScores tokenType field depth =
    Array.map (\column -> ( column, getNextMoveScore tokenType field depth column )) (Array.fromList (List.range 0 (fieldWidth - 1)))


getNextMoveScore : Int -> Connect4Field -> Int -> Int -> Int
getNextMoveScore tokenType field depth column =
    let
        newField =
            throwToken tokenType column field
    in
    if getWinner newField == tokenType || depth == 0 then
        totalScore tokenType newField - totalScore (3 - tokenType) newField

    else
        let
            opponentsMove =
                getBestMove (3 - tokenType) newField (depth - 1)
        in
        0 - Tuple.second opponentsMove


getBestMove : Int -> Connect4Field -> Int -> ( Int, Int )
getBestMove tokenType field depth =
    let
        nextMoveScores =
            getNextMoveScores tokenType field depth

        maxScore =
            Maybe.withDefault 0 (List.maximum (Array.toList (Array.map Tuple.second nextMoveScores)))
    in
    Maybe.withDefault ( 0, 0 ) (Array.get 0 (Array.filter (\moveScore -> Tuple.second moveScore == maxScore) nextMoveScores))


getWinner : Connect4Field -> Int
getWinner field =
    if totalScore 1 field > 1000000 then
        1

    else if totalScore 2 field > 1000000 then
        2

    else
        0



-- UTILS


unMaybeMap : (a -> Maybe b) -> Maybe a -> Maybe b
unMaybeMap x y =
    unMaybe (Maybe.map x y)


unMaybe : Maybe (Maybe a) -> Maybe a
unMaybe x =
    case x of
        Nothing ->
            Nothing

        Just innerX ->
            innerX


flatten : Array (Array a) -> Array a
flatten array =
    Array.foldr Array.append (Array.fromList []) array



-- VIEW


css path =
    node "link" [ rel "stylesheet", href path ] []


gridView : Connect4Field -> Html Msg
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
            , div [ class "connect_4__score--red" ] [ text (String.fromInt model.redScore) ]
            , div [ class "connect_4__score--yellow" ] [ text (String.fromInt model.yellowScore) ]
            ]
        ]
