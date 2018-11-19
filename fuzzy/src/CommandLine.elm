module CommandLine exposing (fuzzyScore, fuzzyMatch, Command(..), parseCommand, ParseError(..), ParamGreed (..), expandCommand, FuzzyError(..))

import Char

type Command a
    = Terminal a
    | NonTerminal ParamGreed (List String) (String -> Maybe (Command a))


type ParseError a
    = InvalidParameter
    | MissingParameters (Command a)
    | ExtraParameters


type ParamGreed
    = Word
    | Rest


{-|
  Separates a String into Just (firstWord, rest) if the string
  is non-empty or Nothing if it is empty
-}
separateFirstWord : String -> (String, String, String)
separateFirstWord query =
    let 
        (before, word, after) = 
            String.foldl
                (\char (before, word, after) ->
                    case char of
                        ' ' ->
                            if word /= "" then
                                (before, word, String.cons char after)
                            else
                                (String.cons char before, word, after)
                        char ->
                            if after == "" then
                                (before, String.cons char word, after)
                            else
                                (before, word, String.cons char after)
                )
                ("", "", "")
                query
    in
        (String.reverse before, String.reverse word, String.reverse after)



parseCommand : String -> Command a -> Result (ParseError a) a
parseCommand query command =
    case command of
        Terminal val ->
            case query of
                "" ->
                    Ok val
                _ ->
                    Err ExtraParameters
        NonTerminal greed suggestions parsingFunction ->
            let
                (beforeWord, firstWord, restWords) =
                    case greed of
                        Word ->
                            separateFirstWord query
                        Rest ->
                            ("", String.trimLeft query, "")
            in
                case firstWord of
                    "" ->
                        Err (MissingParameters command)
                    param ->
                        case (parsingFunction param) of
                            Just command ->
                                parseCommand restWords command
                            Nothing ->
                                Err InvalidParameter



type FuzzyError
    -- Indicates that something went wrong when doing fuzzy expansion which
    -- lead to trying to do fuzzy matching on a terminal
    = ReachedTerminal
    -- The query was expanded based on the suggestions, but the expansion
    -- was not a vailid command
    | MalformedCommand (List String) String
    -- The fuzzy expander ran out of input and can not make any more suggestions.
    -- This should never be returned publically
    | NoMoreInput


{-|
  Recursively expand parts of query based on the suggested strings for
  each command.
  Returns the best matching suggestion thus far, along with a `FuzzyState` which
  contains match data for the last parameter or errors
-}

handleNonTerminalFuzz : String -> String -> ParamGreed -> List String -> (String -> Maybe (Command a)) -> (String, Result FuzzyError (List (String, List Bool)))
handleNonTerminalFuzz previousQuery query greed suggestions parser =
    let
        (leadingWhitespace, currentSection, restQuery) =
            case greed of
                Word -> separateFirstWord query
                Rest -> separateFirstWord query |> (\(init,word,rest) -> (init, word ++ rest, ""))
    in
        if (leadingWhitespace, currentSection) == ("", "") then
            (previousQuery, Err NoMoreInput)
        else
            let
                expandedCommands = fuzzyMatch fuzzyScore suggestions currentSection 

                bestExpansion =
                    List.head expandedCommands
                    |> Maybe.map Tuple.first
                    |> Maybe.withDefault currentSection
                    |> (\expansion ->
                            (Maybe.map (\command -> (expansion, command)) (parser expansion))
                        )
            in
                -- Find all possible expansions of currentSection. Parse based on on the most
                -- Likely expansion.
                -- If that gives a terminal, return reached terminal
                case bestExpansion of
                    Just (expansion, NonTerminal nextGreed nextSuggestions nextParser) ->
                        let
                            nextResult =
                                handleNonTerminalFuzz
                                    (previousQuery ++ " " ++ expansion)
                                    restQuery
                                    nextGreed
                                    nextSuggestions
                                    nextParser
                        in
                            case nextResult of
                                (_, Err NoMoreInput) ->
                                    -- Return all suggestions for the current non-terminal
                                    (previousQuery ++ " " ++ expansion, Ok expandedCommands)
                                futureResult ->
                                    futureResult
                    Just (expansion, Terminal _) ->
                        (previousQuery ++ " " ++ expansion, Ok expandedCommands)
                    Nothing ->
                        (previousQuery, Err <| MalformedCommand suggestions query)

expandCommand : String -> Command a -> (String, Result FuzzyError (List (String, List Bool)))
expandCommand query command =
    case command of
        NonTerminal greed suggestions parser ->
            case handleNonTerminalFuzz "" query greed suggestions parser of
                (expanded, Ok result) ->
                    (expanded, Ok result)
                (expanded, Err NoMoreInput) ->
                    ( expanded
                    , Ok <| List.map (\suggestion -> 
                                (suggestion, List.repeat (String.length suggestion) False))
                                suggestions
                    )
                (expanded, Err e) ->
                    (expanded, Err e)
        Terminal _ ->
            ("", Err ReachedTerminal)


fuzzyScore : String -> String -> (Int, List Bool)
fuzzyScore target input =
    let
        maybeFunction : (Char, String) -> (Char, String) -> (Int, List Bool)
        maybeFunction (t, tRest) (i, iRest) =
            let
                (score, match) =
                    if Char.toLower t == Char.toLower i then
                        (1, True)
                    else
                        (0, False)

                restInput = if match then iRest else input

                (restScore, restMatches) = fuzzyScore tRest restInput
            in
                (score + restScore, match :: restMatches)
    in
        case Maybe.map2 maybeFunction (String.uncons target) (String.uncons input) of
            Just val ->
                val
            Nothing ->
                (0, List.repeat (String.length target) False)



fuzzyMatch :
    (String -> String -> (Int, List Bool))
    -> List String
    -> String
    -> List (String, List Bool)
fuzzyMatch scoringFunction options query =
    let
        withScores =
            List.map2 (\x y -> (x,y)) options
                <| List.map 
                    (\option -> scoringFunction option query) options
    in
        List.map (\(string, (_, matched)) -> (string, matched))
        <| List.sortBy (\(_, (score, _)) -> -score) withScores
