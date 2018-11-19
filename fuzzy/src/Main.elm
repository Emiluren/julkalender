module Main exposing (..)

import Html exposing (..)
import Html
import Html.Events exposing (onInput)
import Html.Attributes exposing (style)
import Dict

import CommandLine
import CommandLine exposing (Command (..), ParamGreed (..), FuzzyError(..), ParseError(..))

searchOptions : List String
searchOptions =
    [ "goto"
    , "move"
    , "drawTo"
    , "sub"
    , "color"
    , "gotoIf0"
    , "set"
    ]


type Instruction
    = Goto Int
    | Move Int Int
    | DrawTo Int Int
    | Sub String
    | Set String Int
    | GotoIf0
    | Label
    | Print String


intParser : String -> Maybe Int
intParser word =
    Result.toMaybe <| String.toInt word

intReference : (Int -> Instruction) -> Command Instruction
intReference msg =
    NonTerminal Word []
        (\query ->
            case intParser query of
                Just val ->
                    Just <| Terminal (msg val)
                Nothing ->
                    Nothing
        )

twoInts : (Int -> Int -> Instruction) -> Command Instruction
twoInts msg =
    NonTerminal Word []
        (\query ->
            case intParser query of
                Just val ->
                    Just <| intReference (msg val)
                Nothing ->
                    Nothing
        )

topLevelCommand : Command Instruction
topLevelCommand =
    NonTerminal Word []
        (\query ->
            let
                -- tagCommand msg = NonTerminal Rest tags (\query -> Just (Terminal (msg query)))
                -- move NonTerminal Rest [] (\query -> Just )

                singleString msg = NonTerminal Rest [] (\query -> Just (Terminal (msg query)))

            in
                case query of
                    "goto" -> Just <| intReference Goto
                    "move" -> Just <| twoInts Move
                    "drawTo" -> Just <| twoInts DrawTo
                    "sub" -> Just <| singleString Sub
                    "print" -> Just <| singleString Print
                    _ -> Nothing
        )


type alias Error a = Result  (ParseError Instruction, Int) a

type Msg
    = Input String


type alias Model =
    { code: Error (List Instruction)
    }


init : (Model, Cmd Msg)
init =
    (Model (Ok []), Cmd.none)


type alias ExecutionState =
    { line: Int
    , vars: Dict.Dict String Int
    , color: (Int, Int, Int)
    }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Input query ->
            ({model | code = parseCode (String.lines query)}, Cmd.none)


outputParsed instruction =
    case instruction of
        Goto n -> 
            "goto " ++ toString n
        Move x y ->
            "move " ++ (toString x) ++ " " ++ toString y
        DrawTo x y ->
            "drawTo " ++ (toString x) ++ " " ++ toString y
        Print str ->
            str
        _ -> "Not displayed"



parseCode : List String -> Error (List Instruction)
parseCode input =
    let
        inner : List String -> List Instruction -> Int -> Error (List Instruction)

        inner input prev line =
            case input of
                head::rest ->
                    case CommandLine.parseCommand head topLevelCommand of
                        Ok command ->
                            inner rest (prev ++ [command]) (line + 1)
                        Err e ->
                            Err (e, line)
                [] ->
                    Ok prev
    in
        inner input [] 1


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    let
        matchRenderer : (String, List Bool) -> Html Msg
        matchRenderer (string, matches) =
            let
                charRenderer (char, match) =
                    ( if match then
                        b []
                      else
                        span []
                    ) [text <| String.fromChar char]
            in
            p
                []
                <| List.map charRenderer
                    <| List.map2 (,) (String.toList string) matches


        -- Thanks stack overflow
        -- https://stackoverflow.com/questions/1995370/html-adding-line-numbers-to-textarea
        textAreaStyle = style
            [ ("background", "url(http://i.imgur.com/2cOaJ.png)")
            , ("background-attachment", "local")
            , ("background-repeat", "no-repeat")
            , ("padding-left", "35px")
            , ("padding-top", "10px")
            , ("border-color", "ccc")
            ]


        resultingCode =
            case model.code of 
                Ok code ->
                    String.join "\n" <| List.map outputParsed code
                Err (e, line) ->
                    ("Error on line : " ++ (toString line) ++
                    case e of 
                        InvalidParameter -> "Invalid parameter"
                        (MissingParameters _) -> "Missing parameters"
                        ExtraParameters -> "Extra parameters"
                    )

    in
    div
        []
        [ textarea [onInput Input, textAreaStyle] []
        , p [] [text resultingCode]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
