module Utils exposing
    ( KeyEvent
    , formatNumber
    , getEntryHeight
    , getIndex
    , getNextIndex
    , getPrevIndex
    , insertOnce
    , modelToStoredModel
    , needsTitles
    , queryCharMin
    , removeItem
    , rx
    , updateItem
    )

import List exposing (length, map)
import Model exposing (Filter(..), Model, StoredModel)
import Regex exposing (Regex)
import Set


type alias KeyEvent =
    { key : String, control : Bool, meta : Bool }


queryCharMin : Int
queryCharMin =
    4


rx : String -> Regex
rx =
    Regex.fromString >> Maybe.withDefault Regex.never


formatNumber : Int -> String
formatNumber =
    String.fromInt >> Regex.replace (rx "\\B(?=(\\d{3})+(?!\\d))") (always ",")


insertOnce : List comparable -> comparable -> List comparable
insertOnce list x =
    Set.toList <| Set.insert x (Set.fromList list)


removeItem : List comparable -> comparable -> List comparable
removeItem list x =
    Set.toList <| Set.remove x (Set.fromList list)


updateItem : List a -> a -> a -> List a
updateItem list old new =
    map
        (\x ->
            if x == old then
                new

            else
                x
        )
        list


getIndex : List a -> a -> Int
getIndex list item =
    let
        f l target n =
            case l of
                [] ->
                    -1

                x :: xs ->
                    if x == target then
                        n

                    else
                        f xs target (n + 1)
    in
    f list item 0


getNextIndex : List a -> a -> Int
getNextIndex list item =
    let
        idx =
            getIndex list item + 1
    in
    if idx == length list then
        0

    else
        idx


getPrevIndex : List a -> a -> Int
getPrevIndex list item =
    let
        idx =
            getIndex list item
    in
    if idx == 0 then
        length list - 1

    else
        idx - 1


modelToStoredModel : Model -> StoredModel
modelToStoredModel model =
    { entries = model.entries
    , currentEntry = model.currentEntry
    , hiddenEntries = Set.toList model.hiddenEntries
    }


needsTitles : Model -> Bool
needsTitles model =
    model.filterType
        /= TitleFilter
        || model.filterValue
        == Nothing


getEntryHeight : Bool -> Int
getEntryHeight hasTitle =
    if hasTitle then
        90

    else
        60
