module Utils exposing (getIndex, insertOnce)

import Set


insertOnce : List comparable -> comparable -> List comparable
insertOnce list x =
    Set.toList <| Set.insert x (Set.fromList list)


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
