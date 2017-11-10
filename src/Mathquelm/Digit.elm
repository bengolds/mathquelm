module Mathquelm.Digit exposing (Digit(..), fromChar, parse, toInt)


type Digit
    = Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine


toInt : List Digit -> Int
toInt digits =
    List.foldl
        (\digit acc ->
            10 * acc + parse digit
        )
        0
        digits


fromChar : Char -> Maybe Digit
fromChar char =
    case char of
        '0' ->
            Just Zero

        '1' ->
            Just One

        '2' ->
            Just Two

        '3' ->
            Just Three

        '4' ->
            Just Four

        '5' ->
            Just Five

        '6' ->
            Just Six

        '7' ->
            Just Seven

        '8' ->
            Just Eight

        '9' ->
            Just Nine

        _ ->
            Nothing


parse : Digit -> Int
parse digit =
    case digit of
        Zero ->
            0

        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9
