module Main exposing (..)

import Html exposing (Html, button, div, span, table, td, text, tr)
import Html.Attributes exposing (id, style)
import Html.Events exposing (onClick)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { nextType : Type
    , currentList : List XnO
    , overStatus : OverStatus
    , winner : String
    , winItems : List Int
    }


init : ( Model, Cmd Msg )
init =
    ( getInititalModel
    , Cmd.none
    )


getInititalModel : Model
getInititalModel =
    { nextType = Cross
    , currentList = getInititalList
    , overStatus = Not
    , winner = ""
    , winItems = []
    }


getInititalList : List XnO
getInititalList =
    [ { id = 1, value = None }
    , { id = 2, value = None }
    , { id = 3, value = None }
    , { id = 4, value = None }
    , { id = 5, value = None }
    , { id = 6, value = None }
    , { id = 7, value = None }
    , { id = 8, value = None }
    , { id = 9, value = None }
    ]


type alias XnO =
    { id : Int
    , value : Type
    }


type Type
    = Cross
    | Zero
    | None


type Msg
    = NoOp
    | UpdateValue Int
    | Restart


type OverStatus
    = Draw
    | Win
    | Not


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateValue id ->
            let
                newList =
                    List.map
                        (\x ->
                            if x.id == id && x.value == None then
                                { x | value = model.nextType }
                            else
                                x
                        )
                        model.currentList

                newType =
                    if newList /= model.currentList then
                        if model.nextType == Cross then
                            Zero
                        else
                            Cross
                    else
                        model.nextType

                ( isWin, winItems ) =
                    checkGameOver model.nextType newList

                isGameOver =
                    newList
                        |> List.filter (\x -> x.value == None)
                        |> List.length
                        |> (==) 0

                ( newWinner, newoverStatus ) =
                    if isWin then
                        ( typeToString model.nextType, Win )
                    else if isGameOver then
                        ( "", Draw )
                    else
                        ( "", Not )
            in
            ( { model
                | nextType = newType
                , currentList = newList
                , overStatus = newoverStatus
                , winner = newWinner
                , winItems = winItems
              }
            , Cmd.none
            )

        Restart ->
            getInititalModel ! []

        NoOp ->
            model ! []


view : Model -> Html Msg
view model =
    let
        firstRow =
            List.take 3 model.currentList

        secondRow =
            List.filter (\x -> x.id > 3 && x.id < 7) model.currentList

        thirdRow =
            List.reverse model.currentList |> List.take 3 |> List.reverse

        overStatusText =
            if model.overStatus == Win then
                "Winner is " ++ model.winner
            else if model.overStatus == Draw then
                "Draw"
            else
                ""
    in
    div []
        [ div []
            [ table []
                [ createRow firstRow model.overStatus model.winItems
                , createRow secondRow model.overStatus model.winItems
                , createRow thirdRow model.overStatus model.winItems
                ]
            , text overStatusText
            ]
        , div [] [ button [ onClick Restart ] [ text "Restart Game" ] ]
        ]


createRow : List XnO -> OverStatus -> List Int -> Html Msg
createRow list overStatus winItems =
    tr [] (List.map (\x -> renderBox overStatus x.id x.value winItems) list)


renderBox : OverStatus -> Int -> Type -> List Int -> Html Msg
renderBox overStatus itemId value winItems =
    let
        newValue =
            typeToString value

        onClickMsg =
            case overStatus of
                Not ->
                    UpdateValue itemId

                _ ->
                    NoOp

        isWinItem =
            List.member itemId winItems
    in
    td [ id (toString itemId), style (tdClass isWinItem), onClick onClickMsg ] [ text newValue ]


tdClass : Bool -> List ( String, String )
tdClass isWinItem =
    [ ( "border-right", "1px solid black" )
    , ( "border-bottom", "1px solid black" )
    , ( "border-top", "1px solid black" )
    , ( "border-left", "1px solid black" )
    , ( "padding-top", "30px" )
    , ( "padding-bottom", "30px" )
    , ( "padding-right", "40px" )
    , ( "padding-left", "30px" )
    , ( "min-width", "30px" )
    , ( "max-width", "30px" )
    , ( "min-height", "30px" )
    , ( "max-height", "30px" )
    , if isWinItem then
        ( "background", "#90EE90" )
      else
        ( "background", "#FFEFD5" )
    ]


typeToString : Type -> String
typeToString value =
    case value of
        Cross ->
            "X"

        Zero ->
            "O"

        None ->
            ""


checkGameOver : Type -> List XnO -> ( Bool, List Int )
checkGameOver nextType xnOList =
    let
        filteredList =
            List.filter (\x -> x.value == nextType) xnOList
                |> List.map (\x -> x.id)
                |> List.sort

        ( isSubList, checkedList ) =
            if List.length filteredList >= 3 then
                List.foldl
                    (\x acc ->
                        if Tuple.first acc == True then
                            acc
                        else
                            let
                                list =
                                    checkSubList filteredList x

                                tempCheck =
                                    list
                                        |> List.length
                                        |> (==) 3

                                finalTuple =
                                    if tempCheck then
                                        ( True, list )
                                    else
                                        ( False, [] )
                            in
                            finalTuple
                    )
                    ( False, [] )
                    possibleMatches
            else
                ( False, [] )

        isMember =
            if isSubList then
                True
            else
                False
    in
    ( isMember, checkedList )


possibleMatches : List (List Int)
possibleMatches =
    [ [ 1, 2, 3 ]
    , [ 1, 4, 7 ]
    , [ 1, 5, 9 ]
    , [ 2, 5, 8 ]
    , [ 3, 5, 7 ]
    , [ 3, 6, 9 ]
    , [ 4, 5, 6 ]
    , [ 7, 8, 9 ]
    ]


checkSubList : List Int -> List Int -> List Int
checkSubList subList sourceList =
    subList
        |> List.filter (\x -> List.member x sourceList)
        |> List.sort


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
