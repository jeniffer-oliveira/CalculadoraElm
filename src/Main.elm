module Main exposing (..)

--elm make src/Main.elm  --output src/elm.js -> para converter elm para js

-- biblioteca usada para criar aplicativos 
import Browser

-- importando funções do HTML
import Html exposing (text, div, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class) 

-- é a função principal que é chamada para executar o app, cria um sandbox (um ambiente limitado que o aplicativo possa ser executado) e define as funções init, update e view para gerenciar o estado do app e sua exibição
main = 
    Browser.sandbox { init = init, update = update, view = view }

-- Model é usado para armazenar o estado atual da calculadora, tendo três campos:
-- currentValueString -> é a representação em string do valor atual
-- currentValue -> é o valor atual como um número float
-- currentOp -> é a operação atual (adição, soma, subtração, multiplicação ou divisão)
type alias Model =
    { currentValueString : String
    , currentValue : Float
    , currentOp : (Float -> Float)
    }

-- Define uma nova operação
type Op 
    = Add 
    | Sub
    | Mult
    | Div

-- Converte a operação acima em uma função de float para float
toFn op =
    case op of 
        Add -> (+)
        Sub -> (-)
        Mult -> (*)
        Div -> (/)


-- inicia o estado da calculadora.
-- Aqui o currentValueString é definido como uma string vazia
-- currentValue é 0.0, ou seja, um float
-- currentOp é definido como a função identity 
init : Model
init = Model "" 0.0 identity 

-- difine as mensagens que podem ser enviadas ao aplicativo, possuindo 4 valores:
-- Number -> um número que foi pressionado
-- Perform -> uma operação (Op) que foi selecionada
-- Reset -> o botão 'C' foi pressionado 
-- Eval -> o botão '=' foi pressionado
type Msg 
        = Number Int
        | Perform Op
        | Reset
        | Eval

-- bloco responsávek por receber as mensagens enviadas pelos usuários da aplicação e atualizar o Model da aplicação de acordo com a mensagem recebida, cada Msg possui uma estrutura especifica e as funções são definidas para tratar cada tipo de mensagem 
update msg model =
            case msg of
                Number n -> -- é enviada sempre que o usuário clica em um número. O codigo atualiza o valor atual da calculadora para incluir o novo numero. O valor é atualizado como uma String e como um número Float, para que possa ser usado em cálculos posteriores.
                        let 
                         novoValor = model.currentValueString ++ String.fromInt n
                        in
                        { model 
                            | currentValueString = novoValor
                            , currentValue = novoValor |> String.toFloat |> Maybe.withDefault 0 
                        } 

                Perform op -> -- é enviada sempre que o usuário clica em um dos quatros botões de Op. O código atualiza o valor atual da calculadora para ser o resultado da operação anterior, usando o valor atual como um dos operandos. A operação atual é atualizada de acordo com a operação que o usuário acabou de clicar. A String do valor atual é limpa. 
                        let
                            updatedValue = model.currentOp model.currentValue  
                        in
                        { model 
                            | currentValue = updatedValue
                            , currentOp = updatedValue |> toFn op
                            , currentValueString = "" 
                        }      

                Eval -> -- é enviada quando o usuário clica no botão '=' para calcular o resultado da operação atual. O código atualiza o valor atual da calc para ser o resultado da operação atual, usando o valor atual como um dos operandos. A string do valor atual é limpa e a função atual é definida como uma função identidade
                    let
                        result = model.currentOp model.currentValue
                    in
                    { model
                        | currentValue = result
                        , currentValueString = ""
                        , currentOp = identity
                    }

                Reset -> -- é enviada quando o usuário clica no botão 'C' para limpar o valor atual da calculadora e redefinir para o estado inicial (init)
                    init 

-- é reponsável por renderizar o modelo da aplicação na tela do usuário. O código cria uma div que contém o valor atual e um conjunto de botões que permitem o usuário inserir números e realizar operações. Para realizar a ação de click, cada botão possui o evento onClick, que envia a mensagem apropriada para a função update quando o botão é clicado.
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