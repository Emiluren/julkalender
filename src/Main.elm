{- Copyright 2018 Emil Segerbäck

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

import Browser
import Html exposing (Html, a, button, div, h1, option, select, text, textarea)
import Html.Attributes exposing (style, href)
import Html.Events exposing (onClick, onInput)
import Set
import Svg exposing (Svg, svg)
import Svg.Attributes as Svg
import Parser exposing (Parser, (|.), (|=), spaces)

-- symbol : Parser Char
-- symbol = Parser.oneOf (String.toList "!#$%&|*+-/:<=>?@^_~")

legalSymbolsInIdentifiers : List Char
legalSymbolsInIdentifiers = String.toList "!#$%&|*+-/:<=>?@^_~"

type LispVal
    = Atom String
    | List (List LispVal)
    | DottedList (List LispVal) LispVal
    | Number Int
    | String String
    | Bool Bool

parseAtom : Parser LispVal
parseAtom =
    let atomLispVal : String -> LispVal
        atomLispVal atom =
            case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _ -> Atom atom
    in Parser.map atomLispVal <| Parser.variable
        { start = \c -> Char.isAlpha c ||
              List.member c legalSymbolsInIdentifiers
        , inner = \c -> Char.isAlphaNum c ||
              List.member c legalSymbolsInIdentifiers
        , reserved = Set.empty
        }

parseList : Parser LispVal
parseList =
    Parser.loop [] listHelp

listHelp : List LispVal -> Parser (Parser.Step (List LispVal) LispVal)
listHelp revValues =
    Parser.oneOf
        [ Parser.succeed (\expr -> Parser.Loop (expr :: revValues))
            |. spaces
            |= parseExpr
            |. spaces
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List (List.reverse revValues)))
        ]

parseExpr : Parser LispVal
parseExpr =
    Parser.oneOf
        [ parseAtom
        , parseList
        ]

readExpr : String -> String
readExpr input =
    case Parser.run parseExpr input of
        Err e -> "Error"
        Ok _ -> "Found value"

type alias Model =
    { program : Expression
    , code : String
    , evalResult : String
    }

main =
    Browser.sandbox
        { init =
              { program = christmasTree
              , code = "(triangle (10 . 150) (50 . 110) (90 . 150))\n(triangle (20 . 120) (50 . 90) (80 . 120))"
              , evalResult = ""
              }
        , update = update
        , view = view
        }

type Msg = ChangeProgram Expression | RunCode | NewCode String

type alias Vec2 = { x : Float, y : Float }

-- TODO: add more shapes
type Shape = Triangle Vec2 Vec2 Vec2

type Color = Green | Red

allColors = [ Green, Red ]

colorString : Color -> String
colorString color =
    case color of
        Green ->
            "green"
        Red ->
            "red"

-- stringColor : String -> Color
-- stringColor 

-- TODO: maybe add interactivity
type Expression = Combine (List Expression) | Draw Shape Color

-- Default program to show at start. Should draw a christmas tree.
christmasTree : Expression
christmasTree = Combine
    [ Draw
          (Triangle { x = 10, y = 150 } { x = 50, y = 110 } { x = 90, y = 150 })
          Green
    , Draw
          (Triangle { x = 20, y = 120 } { x = 50, y = 90 } { x = 80, y = 120 })
          Green
    ]

shapeToSvg : (Shape, Color) -> Svg msg
shapeToSvg (shape, color) =
    let pointString {x, y} = String.fromFloat x ++ "," ++ String.fromFloat y
    in case shape of
           Triangle v1 v2 v3 ->
               Svg.polygon
                   [ Svg.fill (colorString color)
                   , [v1, v2, v3]
                       |> List.map pointString
                       |> String.join " "
                       |> Svg.points
                   ]
                   []

-- Render a list of shapes into svg
drawShapes : List (Shape, Color) -> Html msg
drawShapes shapes =
    svg
        [ Svg.width "100%", Svg.height "100%", Svg.viewBox "0 0 100 200" ]
        (List.map shapeToSvg shapes)

update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeProgram newProgram ->
            { model | program = newProgram }
        NewCode code ->
            { model | code = code }
        RunCode ->
            { model | evalResult = readExpr model.code }

-- Evaluate a program to
evalProgram : Expression -> List (Shape, Color)
evalProgram expression =
    case expression of
        Draw shape color ->
            [(shape, color)]
        Combine expressions ->
            List.concatMap evalProgram expressions

makeOption : Color -> Html Msg
makeOption c = option [] [ text (colorString c) ]

viewProgramEditor : Expression -> Html Msg
viewProgramEditor expr =
    case expr of
        Draw shape color ->
            div
                []
                [ text "Draw"
                , select [] [ option [] [ text "Triangle" ] ]
                , select [] (List.map makeOption allColors)
                ]
        Combine expressions ->
            div [] <| List.concat
                [ [ text "Combine" ]
                , List.map viewProgramEditor expressions
                , [ button [] [ text "+" ] ]
                ]

codeView : Model -> Html Msg
codeView model =
    div
        [ style "width" "50%"
        --, style "height" "200px"
        --, style "float" "left"
        , style "flex" "1"
        ]
        [ button [ onClick RunCode ] [ text "Kör" ]
        , textarea
              [ style "width" "100%"
              , style "height" "100%"
              , style "resize" "none"
              , onInput NewCode
              ]
              [ text model.code ]
        ]

drawView : Model -> Html Msg
drawView model =
    div 
        [ style "width" "50%"
        --, style "float" "left"
        , style "flex" "1"
        ]
        [ drawShapes (evalProgram christmasTree)
        ]

view : Model -> Html Msg
view model =
    div
        []
        [ h1 [] [ text "God jul önskar LiTHe kod!" ]
        , text "Delta gärna i vår "
        , a [ href "http://lithekod.se/advent-of-code/" ] [ text "Advent of code-tävling" ]
        , text model.evalResult
        , div
              [ style "width" "100%"
              , style "height" "100%"
              , style "display" "flex"
              ]
              [ codeView model
              , drawView model
              ]
        ]
