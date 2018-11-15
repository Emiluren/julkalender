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
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Svg exposing (Svg, svg)
import Svg.Attributes as Svg

type alias Model = Int

main =
  Browser.sandbox { init = 0, update = update, view = view }

type Msg = Increment | Decrement

type alias Vec2 = { x : Float, y : Float }

-- TODO: add more shapes
type Shape = Triangle Vec2 Vec2 Vec2

type Color = Green

colorString : Color -> String
colorString color =
    case color of
        Green ->
            "green"

-- TODO: maybe add interactivity
type alias ChristmasProgram = List (Shape, Color)

christmasTree : ChristmasProgram
christmasTree =
    [ (Triangle { x = 10, y = 150 } { x = 50, y = 110 } { x = 90, y = 150 }, Green)
    ]

shapeToSvg : (Shape, Color) -> Svg msg
shapeToSvg (shape, color) =
    case shape of
        Triangle v1 v2 v3 ->
            Svg.polygon
                [ Svg.fill (colorString color)
                , Svg.points <|
                    String.join " " <|
                        List.map (\{x, y} ->
                                      String.fromFloat x ++ "," ++ String.fromFloat y)
                            [v1, v2, v3]
                ]
                []

-- Render a list of shapes into svg
drawShapes : List (Shape, Color) -> Html msg
drawShapes shapes =
    svg
        [ Svg.width "100%", Svg.height "100%", Svg.viewBox "0 0 100 200" ]
        (List.map shapeToSvg shapes)

update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1

codeView : Model -> Html Msg
codeView model =
    div
        [ style "width" "50%"
        , style "float" "left"
        ]
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]

drawView model =
    div 
        [ style "width" "50%"
        , style "float" "left"
        ]
        [ drawShapes christmasTree
        ]

view model = div [] [ h1 [] [ text "God jul" ], codeView model, drawView model ]
