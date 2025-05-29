module DictByTypeVariableFromContextInternal exposing (DictByTypeVariableFromContext(..), InnerDictByTypeVariableFromContext(..), VisitQueue, fromSortedList, unconsBiggest, unconsBiggestWhileDroppingGT)

import TypeVariableFromContext exposing (TypeVariableFromContext)



-- The color of a node. Leaves are considered False.


type InnerDictByTypeVariableFromContext v
    = InnerNode {- True = Red, False = Black -} Bool TypeVariableFromContext v (InnerDictByTypeVariableFromContext v) (InnerDictByTypeVariableFromContext v)
    | Leaf


type DictByTypeVariableFromContext v
    = DictByTypeVariableFromContext Int (InnerDictByTypeVariableFromContext v)


{-| Builds a Dict from an already sorted list.

WARNING: This does _not_ check that the list is sorted.

-}
fromSortedList :
    { length : Int, list : List ( TypeVariableFromContext, v ) }
    -> DictByTypeVariableFromContext v
fromSortedList associationList =
    let
        redLayer : Int
        redLayer =
            floor (logBase 2 (toFloat associationList.length))
    in
    fromSortedListHelp redLayer 0 0 associationList.length associationList.list
        |> Tuple.first
        |> DictByTypeVariableFromContext associationList.length


fromSortedListHelp : Int -> Int -> Int -> Int -> List ( TypeVariableFromContext, v ) -> ( InnerDictByTypeVariableFromContext v, List ( TypeVariableFromContext, v ) )
fromSortedListHelp redLayer layer fromIncluded toExcluded acc =
    -- IGNORE TCO
    if fromIncluded >= toExcluded then
        ( Leaf, acc )

    else
        let
            mid : Int
            mid =
                fromIncluded + (toExcluded - fromIncluded) // 2

            ( lchild, accAfterLeft ) =
                fromSortedListHelp redLayer (layer + 1) fromIncluded mid acc
        in
        case accAfterLeft of
            [] ->
                ( Leaf, acc )

            ( k, v ) :: tail ->
                let
                    ( rchild, accAfterRight ) =
                        fromSortedListHelp redLayer (layer + 1) (mid + 1) toExcluded tail
                in
                ( InnerNode (layer > 0 && (layer - redLayer == 0)) k v lchild rchild
                , accAfterRight
                )


{-| This is a list of nodes that are going to be visited.
-}
type alias VisitQueue v =
    List (InnerDictByTypeVariableFromContext v)


{-| Try getting the biggest key/value pair from the visit queue
-}
unconsBiggest : VisitQueue v -> Maybe ( TypeVariableFromContext, v, VisitQueue v )
unconsBiggest queue =
    case queue of
        [] ->
            Nothing

        h :: t ->
            case h of
                InnerNode _ key value Leaf Leaf ->
                    Just ( key, value, t )

                InnerNode _ key value childLT Leaf ->
                    Just ( key, value, childLT :: t )

                InnerNode color key value childLT childGT ->
                    unconsBiggest (childGT :: InnerNode color key value childLT Leaf :: t)

                Leaf ->
                    unconsBiggest t


{-| Try getting the biggest key/value pair from the visit queue, while dropping all values greater than the given key
-}
unconsBiggestWhileDroppingGT : TypeVariableFromContext -> VisitQueue v -> Maybe ( TypeVariableFromContext, v, VisitQueue v )
unconsBiggestWhileDroppingGT compareKey queue =
    case queue of
        [] ->
            Nothing

        h :: t ->
            case h of
                InnerNode color key value childLT childGT ->
                    if TypeVariableFromContext.greaterThan key compareKey then
                        unconsBiggestWhileDroppingGT compareKey (childLT :: t)

                    else if TypeVariableFromContext.equals key compareKey then
                        Just ( key, value, childLT :: t )

                    else
                        case childGT of
                            Leaf ->
                                Just ( key, value, childLT :: t )

                            InnerNode _ _ _ _ _ ->
                                unconsBiggestWhileDroppingGT compareKey (childGT :: InnerNode color key value childLT Leaf :: t)

                Leaf ->
                    unconsBiggestWhileDroppingGT compareKey t
