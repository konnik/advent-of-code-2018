module Util exposing (..)

int : String -> Int
int str = String.toInt str |> Maybe.withDefault 0  

minimum : List number -> number
minimum list = 
    case (List.minimum list) of
        Just value -> value
        Nothing -> Debug.todo "should not happen" 
