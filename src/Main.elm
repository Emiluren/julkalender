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
import Html exposing (Html, button, div, h1, option, select, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
--import Monocle.Lens as Lens exposing (Lens)
import Svg exposing (Svg, svg)
import Svg.Attributes as Svg

type alias Model =
    { program : Expression
    }

main =
    Browser.sandbox
        { init = Model christmasTree
        , update = update
        , view = view
        }

type Msg = ChangeProgram Expression

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
        , style "float" "left"
        ]
        [ viewProgramEditor model.program
        ]

drawView model =
    div 
        [ style "width" "50%"
        , style "float" "left"
        ]
        [ drawShapes (evalProgram christmasTree)
        ]

view model =
    div
        []
        [ h1 [] [ text "God jul" ]
        , codeView model
        , drawView model
        ]
