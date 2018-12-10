module Util exposing (..)

int : String -> Int
int str = 
    case String.toInt (String.trim str) of
        Just value -> value
        Nothing -> Debug.todo ("Parse error: " ++ str) 


minimum : List number -> number
minimum list = 
    case (List.minimum list) of
        Just value -> value
        Nothing -> Debug.todo "should not happen" 

maximum : List number -> number
maximum list = 
    case (List.maximum list) of
        Just value -> value
        Nothing -> Debug.todo "should not happen" 
