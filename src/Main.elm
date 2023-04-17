module Main exposing (..)

--elm make src/Main.elm  --output src/elm.js -> para converter elm para js

import Browser

-- importando funções do HTML
import Html exposing (text, div, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class) 

main = 
    Browser.sandbox { init = init, update = update, view = view }

type alias Model =
    { currentValueString : String
    , currentValue : Float
    , currentOp : (Float -> Float)
    }

type Op 
    = Add
    | Sub
    | Mult
    | Div

toFn op =
    case op of 
        Add -> (+)
        Sub -> (-)
        Mult -> (*)
        Div -> (/)


init : Model
init = Model "" 0.0 identity 

type Msg 
        = Number Int
        | Perform Op
        | Reset
        | Eval

update msg model =
            case msg of
                Number n ->
                        let 
                         novoValor = model.currentValueString ++ String.fromInt n
                        in
                        { model 
                            | currentValueString = novoValor
                            , currentValue = novoValor |> String.toFloat |> Maybe.withDefault 0 
                        } 

                Perform op -> 
                        let
                            updatedValue = model.currentOp model.currentValue  
                        in
                        { model 
                            | currentValue = updatedValue
                            , currentOp = updatedValue |> toFn op
                            , currentValueString = "" 
                        }      

                Eval ->
                    let
                        result = model.currentOp model.currentValue
                    in
                    { model
                        | currentValue = result
                        , currentValueString = ""
                        , currentOp = identity
                    }

                Reset ->
                    init 

view model = 
        div [ class "calculadora"] 
        [div [class "display"] [ text (String.fromFloat model.currentValue)]
        , div [class "buttons"] 
            [
                button [class "operador", onClick (Perform Add)] [text "+"]
                , button [class "operador", onClick(Perform Sub)] [text "-"]
                , button [class "operador", onClick(Perform Mult)] [text "x"]
                , button [class "operador", onClick(Perform Div)] [text "÷"]
                , button [class "igual", onClick Eval] [text "="]
                , button [ onClick (Number 7) ] [ text "7"]
                , button [ onClick (Number 8) ] [ text "8"]
                , button [ onClick (Number 9) ] [ text "9"]
                , button [ onClick (Number 4) ] [ text "4"]
                , button [ onClick (Number 5) ] [ text "5"]
                , button [ onClick (Number 6) ] [ text "6"]
                , button [ onClick (Number 1)  ] [ text "1"]
                , button [ onClick (Number 2) ] [ text "2"]
                , button [ onClick (Number 3) ] [ text "3"]
                , button [ onClick (Number 0) ] [ text "0"]
                , button [] [ text "."]
                , button [onClick Reset] [ text "C"]
            ]
        
        ]