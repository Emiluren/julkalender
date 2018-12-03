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
import Html exposing (Html, a, br, button, div, h1, option, select, text, textarea)
import Html.Attributes exposing (style, href)
import Html.Events exposing (onClick, onInput)
import Set
import Svg exposing (Svg, svg)
import Svg.Attributes as Svg
import Parser exposing (Parser, (|.), (|=), spaces, Problem(..))

legalSymbolsInIdentifiers : List Char
legalSymbolsInIdentifiers = String.toList "!#$%&|*+-/:<=>?@^_~"

type LispVal
    = Atom String
    | Cons LispVal LispVal
    | Nil
    | Int Int
    | Float Float
    | String String
    | Bool Bool
    | V2 Vec2

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

parseNumber : Parser LispVal
parseNumber =
    Parser.oneOf
        [ Parser.symbol "."
            |> Parser.andThen (\_ -> Parser.problem "floating point numbers must start with a digit, like 0.25")
        , Parser.number
              { int = Just Int
              , hex = Just Int
              , octal = Just Int
              , binary = Just Int
              , float = Just Float
              }
        ]

parseList : Parser (List LispVal)
parseList =
    Parser.loop [] listHelp

listHelp : List LispVal -> Parser (Parser.Step (List LispVal) (List LispVal))
listHelp revValues =
    Parser.oneOf
        [ Parser.succeed (\expr -> Parser.Loop (expr :: revValues))
            |= parseExpr
            |. spaces
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse revValues))
        ]

makeList : List LispVal -> LispVal
makeList = List.foldr Cons Nil

makeDottedList : List LispVal -> LispVal -> LispVal
makeDottedList list lastElem = List.foldr Cons lastElem list

parseListOrDotted : List LispVal -> Parser LispVal
parseListOrDotted list =
    Parser.oneOf
        [ Parser.succeed (makeDottedList list)
              |. Parser.symbol "."
              |. spaces
              |= parseExpr
              |. spaces
              |. Parser.symbol ")"
        , Parser.succeed (makeList list)
              |. Parser.symbol ")"
        ]

parseExpr : Parser LispVal
parseExpr =
    Parser.oneOf
        -- Numbers need to be backtrackable because the builtin number parser
        -- accepts floats like .25 and we want . for dotted lists
        [ Parser.backtrackable parseNumber
        , parseAtom
        , Parser.succeed identity
            |. Parser.symbol "("
            |. spaces
            |= parseList
            |> Parser.andThen parseListOrDotted
        ]

-- Helper function to display cons cells
showList : LispVal -> String
showList val =
    let internal v =
            case v of
                Cons x (Cons y ys) ->
                    showVal x :: " " :: internal (Cons y ys)

                Cons x Nil ->
                    [ showVal x, ")"]

                Cons x y ->
                    [ showVal x, " . ", showVal y, ")" ]

                -- Should not happen unless we call this function with something
                -- that is not a cons cell
                _ ->
                    [ ")" ]

    in
        String.concat <| "(" :: internal val

showVal : LispVal -> String
showVal val =
    case val of
        Atom name -> name
        Int i -> String.fromInt i
        Float f -> String.fromFloat f
        String str -> "\"" ++ str ++ "\""
        Bool True -> "#t"
        Bool False -> "#f"
        Cons (Atom "quote") (Cons x Nil) -> "'" ++ showVal x
        Cons _ _ -> showList val
        Nil -> "()"
        V2 { x, y } ->
            "(v2 x y)"

showErrors : List Parser.DeadEnd -> String
showErrors errors =
    let showError e =
            case e.problem of
                Expecting str -> "Expected " ++ str
                ExpectingInt -> "Expected int"
                ExpectingHex -> "Expected hex"
                ExpectingOctal -> "Expected octal"
                ExpectingBinary -> "Expected binary"
                ExpectingFloat -> "Expected float"
                ExpectingNumber -> "Expected number"
                ExpectingVariable -> "Expected variable"
                ExpectingSymbol sym -> "Expected symbol " ++ sym
                ExpectingKeyword keyw -> "Expected keyword " ++ keyw
                ExpectingEnd -> "Expected end"
                UnexpectedChar -> "Unexpected char"
                Problem str -> "Problem: " ++ str
                BadRepeat -> "Bad repeat"
    in String.join "\n" <| List.map showError errors

readExpr : String -> Result String String
readExpr input =
    case Parser.run parseExpr input of
        Err e -> Err ("Fel: " ++ showErrors e)
        Ok val -> Ok (showVal val)

-- Default program to show at start. Should draw a christmas tree.
christmasProgram : String
christmasProgram =
    "(triangle (v2 10 150) (v2 50 110) (v2 90 150))\n(triangle (v2 20 120) (v2 50 90) (v2 80 120))"

eval : LispVal -> Result String (LispVal, List Shape)
eval v =
    case v of
        Cons (Atom "quote") (Cons val Nil) ->
            Ok (val, [])
        _ -> Err ("Okänt uttryck " ++ showVal v)

type alias Model =
    { code : String
    , evalResult : String
    , error : String
    }

main : Program () Model Msg
main =
    Browser.sandbox
        { init =
              { code = christmasProgram
              , error = ""
              , evalResult = ""
              }
        , update = update
        , view = view
        }

type Msg = RunCode | NewCode String

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
        NewCode code ->
            { model | code = code }
        RunCode ->
            case readExpr model.code of
                Ok res -> 
                    { model
                        | evalResult = res
                        , error = ""
                    }
                Err e ->
                    { model | error = e }

codeView : Model -> Html Msg
codeView model =
    div
        [ style "width" "50%"
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
        , style "flex" "1"
        ]
        [ --drawShapes (evalProgram christmasTree)
        ]

view : Model -> Html Msg
view model =
    div
        []
        [ h1 [] [ text "God jul önskar LiTHe kod!" ]
        , text "Delta gärna i vår "
        , a
            [ href "http://lithekod.se/advent-of-code/" ]
            [ text "Advent of code-tävling" ]
        , br [] []
        , text model.error
        , div
              [ style "width" "100%"
              , style "height" "500px"
              , style "display" "flex"
              ]
              [ codeView model
              , drawView model
              ]
        , div
              [ style "flex" "1"
              , style "width" "100%"
              , style "margin-top" "50px"
              ]
              [ text "Log:"
              , br [] []
              , text model.evalResult
              ]
        ]
