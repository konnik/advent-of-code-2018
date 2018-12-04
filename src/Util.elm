module Util exposing (..)

int : String -> Int
int str = String.toInt str |> Maybe.withDefault 0  