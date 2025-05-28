module DictByTypeVariableFromContext exposing
    ( DictByTypeVariableFromContext
    , empty, singleton, twoDistinct, insert, update, remove
    , isEmpty, member, get, size, equals, any
    , getMinKey, getMin, getMaxKey, getMax
    , popMin, popMax
    , keys, values, toList, fromList
    , map, foldl, foldr, filter, partition
    , union, intersect, diff, merge
    , toCoreDict, fromCoreDict
    , restructure
    )

{-| A dictionary mapping unique keys to values. The keys can be any TypeVariableFromContext
type. This includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or
lists of TypeVariableFromContext types.

Insert, remove, and query operations all take _O(log n)_ time.


@docs DictByTypeVariableFromContext


# Build

@docs empty, singleton, twoDistinct, insert, update, remove


# Query

@docs isEmpty, member, get, size, equals, any


# Min / Max

@docs getMinKey, getMin, getMaxKey, getMax

@docs popMin, popMax


# Lists

@docs keys, values, toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition


# Combine

@docs union, intersect, diff, merge


# Interoperability

@docs toCoreDict, fromCoreDict


# Advanced functions

@docs restructure

-}

import Dict
import DictByTypeVariableFromContextInternal exposing (DictByTypeVariableFromContext(..))
import ListWithLength exposing (ListWithLength)
import TypeVariableFromContext exposing (TypeVariableFromContext)



{- Parts of this file (the documentation and API, and much of the implementation) are copied or adapted from `elm/core`, and thus they are Copyright 2014-present Evan Czaplicki -}
-- DICTIONARIES


{-| A dictionary of keys and values. So a `Dict String User` is a dictionary
that lets you look up a `String` (such as user names) and find the associated
`User`.

    import FastDict as Dict exposing (Dict)

    users : Dict String User
    users =
        Dict.fromList
            [ ( "Alice", User "Alice" 28 1.65 )
            , ( "Bob", User "Bob" 19 1.82 )
            , ( "Chuck", User "Chuck" 33 1.75 )
            ]

    type alias User =
        { name : String
        , age : Int
        , height : Float
        }

-}
type alias DictByTypeVariableFromContext v =
    DictByTypeVariableFromContextInternal.DictByTypeVariableFromContext v


{-| Create an empty dictionary.
-}
empty : DictByTypeVariableFromContext v_
empty =
    DictByTypeVariableFromContext 0 DictByTypeVariableFromContextInternal.Leaf


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals : Dict String String
    animals =
        fromList [ ("Tom", "Cat"), ("Jerry", "Mouse") ]

    get "Tom"   animals
    --> Just "Cat"

    get "Jerry" animals
    --> Just "Mouse"

    get "Spike" animals
    --> Nothing

-}
get : TypeVariableFromContext -> DictByTypeVariableFromContext v -> Maybe v
get targetKey (DictByTypeVariableFromContextInternal.DictByTypeVariableFromContext _ dict) =
    getInner targetKey dict


getInner : TypeVariableFromContext -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v -> Maybe v
getInner targetKey dict =
    case dict of
        DictByTypeVariableFromContextInternal.Leaf ->
            Nothing

        DictByTypeVariableFromContextInternal.InnerNode _ key value left right ->
            case TypeVariableFromContext.compare targetKey key of
                LT ->
                    getInner targetKey left

                EQ ->
                    Just value

                GT ->
                    getInner targetKey right


{-| Determine if a key is in a dictionary.
-}
member : TypeVariableFromContext -> DictByTypeVariableFromContext v_ -> Bool
member key dict =
    case get key dict of
        Just _ ->
            True

        Nothing ->
            False


{-| Determine the number of key-value pairs in the dictionary.
-}
size : DictByTypeVariableFromContext v_ -> Int
size (DictByTypeVariableFromContextInternal.DictByTypeVariableFromContext sz _) =
    sz


{-| Determine if two dictionaries are equal. This is needed because the structure could be different depending on insertion order.
-}
equals : DictByTypeVariableFromContext v -> DictByTypeVariableFromContext v -> Bool
equals (DictByTypeVariableFromContextInternal.DictByTypeVariableFromContext lsz lRoot) (DictByTypeVariableFromContextInternal.DictByTypeVariableFromContext rsz rRoot) =
    (lsz - rsz == 0)
        && equalsHelp (DictByTypeVariableFromContextInternal.unconsBiggest [ lRoot ])
            (DictByTypeVariableFromContextInternal.unconsBiggest [ rRoot ])


equalsHelp : Maybe ( TypeVariableFromContext, v, DictByTypeVariableFromContextInternal.VisitQueue v ) -> Maybe ( TypeVariableFromContext, v, DictByTypeVariableFromContextInternal.VisitQueue v ) -> Bool
equalsHelp lList rList =
    case lList of
        Nothing ->
            case rList of
                Nothing ->
                    True

                Just _ ->
                    False

        Just ( lk, lv, lRest ) ->
            case rList of
                Nothing ->
                    False

                Just ( rk, rv, rRest ) ->
                    -- for TCO
                    if TypeVariableFromContext.equals lk rk && (lv == rv) then
                        equalsHelp (DictByTypeVariableFromContextInternal.unconsBiggest lRest) (DictByTypeVariableFromContextInternal.unconsBiggest rRest)

                    else
                        False


{-| Gets the smallest key in the dictionary.

    [ ( 1, 'z' ), ( 2, 'a' ) ]
        |> fromList
        |> getMinKey
    --> Just 1


    empty
        |> getMinKey
    --> Nothing

-}
getMinKey : DictByTypeVariableFromContext v_ -> Maybe TypeVariableFromContext
getMinKey (DictByTypeVariableFromContextInternal.DictByTypeVariableFromContext _ dict) =
    let
        go : DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v -> Maybe TypeVariableFromContext
        go n =
            case n of
                DictByTypeVariableFromContextInternal.Leaf ->
                    Nothing

                DictByTypeVariableFromContextInternal.InnerNode _ k _ DictByTypeVariableFromContextInternal.Leaf _ ->
                    Just k

                DictByTypeVariableFromContextInternal.InnerNode _ _ _ l _ ->
                    go l
    in
    go dict


{-| Gets the biggest key in the dictionary.

    [ ( 1, 'z' ), ( 2, 'a' ) ]
        |> fromList
        |> getMaxKey
    --> Just 2


    empty
        |> getMaxKey
    --> Nothing

-}
getMaxKey : DictByTypeVariableFromContext v_ -> Maybe TypeVariableFromContext
getMaxKey (DictByTypeVariableFromContextInternal.DictByTypeVariableFromContext _ dict) =
    let
        go : DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v -> Maybe TypeVariableFromContext
        go n =
            case n of
                DictByTypeVariableFromContextInternal.Leaf ->
                    Nothing

                DictByTypeVariableFromContextInternal.InnerNode _ k _ _ DictByTypeVariableFromContextInternal.Leaf ->
                    Just k

                DictByTypeVariableFromContextInternal.InnerNode _ _ _ _ r ->
                    go r
    in
    go dict


{-| Gets the key-value pair with the smallest key.

    [ ( 1, 'z' ), ( 2, 'a' ) ]
        |> fromList
        |> getMin
    --> Just ( 1, 'z' )


    empty
        |> getMin
    --> Nothing

-}
getMin : DictByTypeVariableFromContext v -> Maybe ( TypeVariableFromContext, v )
getMin (DictByTypeVariableFromContextInternal.DictByTypeVariableFromContext _ dict) =
    getMinInner dict


getMinInner : DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v -> Maybe ( TypeVariableFromContext, v )
getMinInner n =
    case n of
        DictByTypeVariableFromContextInternal.Leaf ->
            Nothing

        DictByTypeVariableFromContextInternal.InnerNode _ k v DictByTypeVariableFromContextInternal.Leaf _ ->
            Just ( k, v )

        DictByTypeVariableFromContextInternal.InnerNode _ _ _ l _ ->
            getMinInner l


{-| Gets the key-value pair with the biggest key.

    [ ( 1, 'z' ), ( 2, 'a' ) ]
        |> fromList
        |> getMax
    --> Just ( 2, 'a' )


    empty
        |> getMax
    --> Nothing

-}
getMax : DictByTypeVariableFromContext v -> Maybe ( TypeVariableFromContext, v )
getMax (DictByTypeVariableFromContextInternal.DictByTypeVariableFromContext _ dict) =
    let
        go : DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v -> Maybe ( TypeVariableFromContext, v )
        go n =
            case n of
                DictByTypeVariableFromContextInternal.Leaf ->
                    Nothing

                DictByTypeVariableFromContextInternal.InnerNode _ k v _ DictByTypeVariableFromContextInternal.Leaf ->
                    Just ( k, v )

                DictByTypeVariableFromContextInternal.InnerNode _ _ _ _ r ->
                    go r
    in
    go dict


any :
    (TypeVariableFromContext -> value -> Bool)
    -> DictByTypeVariableFromContext value
    -> Bool
any isNeedle (DictByTypeVariableFromContextInternal.DictByTypeVariableFromContext _ dict) =
    anyInner isNeedle dict


anyInner :
    (TypeVariableFromContext -> value -> Bool)
    -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext value
    -> Bool
anyInner isNeedle dict =
    -- IGNORE TCO
    case dict of
        DictByTypeVariableFromContextInternal.Leaf ->
            False

        DictByTypeVariableFromContextInternal.InnerNode _ key value left right ->
            -- not all in one || to get a bit of TCO action
            if
                isNeedle key value
                    || anyInner isNeedle left
            then
                True

            else
                anyInner isNeedle right


{-| Removes the key-value pair with the smallest key from the dictionary, and returns it.

    [ ( 1, 'z' ), ( 2, 'a' ) ]
        |> fromList
        |> popMin
    --> Just ( ( 1, 'z' ), fromList [ ( 2, 'a' ) ] )


    empty
        |> popMin
    --> Nothing

-}
popMin : DictByTypeVariableFromContext v -> Maybe ( ( TypeVariableFromContext, v ), DictByTypeVariableFromContext v )
popMin dict =
    -- TODO: make faster by adapting `remove`
    Maybe.map
        (\(( k, _ ) as kv) ->
            ( kv, remove k dict )
        )
        (getMin dict)


{-| Removes the key-value pair with the biggest key from the dictionary, and returns it.

    [ ( 1, 'z' ), ( 2, 'a' ) ]
        |> fromList
        |> popMax
    --> Just ( ( 2, 'a' ), fromList [ ( 1, 'z' ) ] )


    empty
        |> popMax
    --> Nothing

-}
popMax : DictByTypeVariableFromContext v -> Maybe ( ( TypeVariableFromContext, v ), DictByTypeVariableFromContext v )
popMax dict =
    -- TODO: make faster by adapting `remove`
    Maybe.map
        (\(( k, _ ) as kv) ->
            ( kv, remove k dict )
        )
        (getMax dict)


{-| Determine if a dictionary is empty.

    isEmpty empty
    --> True

-}
isEmpty : DictByTypeVariableFromContext v_ -> Bool
isEmpty (DictByTypeVariableFromContextInternal.DictByTypeVariableFromContext _ dict) =
    case dict of
        DictByTypeVariableFromContextInternal.Leaf ->
            True

        DictByTypeVariableFromContextInternal.InnerNode _ _ _ _ _ ->
            False


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : TypeVariableFromContext -> v -> DictByTypeVariableFromContext v -> DictByTypeVariableFromContext v
insert key value (DictByTypeVariableFromContextInternal.DictByTypeVariableFromContext sz dict) =
    let
        ( result, isNew ) =
            insertInner key value dict
    in
    if isNew then
        DictByTypeVariableFromContext (sz + 1) result

    else
        DictByTypeVariableFromContext sz result


insertNoReplace : TypeVariableFromContext -> v -> DictByTypeVariableFromContext v -> DictByTypeVariableFromContext v
insertNoReplace key value (DictByTypeVariableFromContextInternal.DictByTypeVariableFromContext sz dict) =
    let
        ( result, isNew ) =
            insertInnerNoReplace key value dict
    in
    if isNew then
        DictByTypeVariableFromContext (sz + 1) result

    else
        DictByTypeVariableFromContext sz result


insertInner : TypeVariableFromContext -> v -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v -> ( DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v, Bool )
insertInner key value dict =
    -- Root node is always False
    case insertHelp key value dict of
        ( DictByTypeVariableFromContextInternal.InnerNode True k v l r, isNew ) ->
            ( DictByTypeVariableFromContextInternal.InnerNode False k v l r, isNew )

        x ->
            x


insertInnerNoReplace : TypeVariableFromContext -> v -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v -> ( DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v, Bool )
insertInnerNoReplace key value dict =
    -- Root node is always False
    case insertHelpNoReplace key value dict of
        ( DictByTypeVariableFromContextInternal.InnerNode True k v l r, isNew ) ->
            ( DictByTypeVariableFromContextInternal.InnerNode False k v l r, isNew )

        x ->
            x


insertHelp : TypeVariableFromContext -> v -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v -> ( DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v, Bool )
insertHelp key value dict =
    -- IGNORE TCO
    case dict of
        DictByTypeVariableFromContextInternal.Leaf ->
            -- New nodes are always red. If it violates the rules, it will be fixed
            -- when balancing.
            ( DictByTypeVariableFromContextInternal.InnerNode True key value DictByTypeVariableFromContextInternal.Leaf DictByTypeVariableFromContextInternal.Leaf, True )

        DictByTypeVariableFromContextInternal.InnerNode nColor nKey nValue nLeft nRight ->
            case TypeVariableFromContext.compare key nKey of
                LT ->
                    let
                        ( newLeft, isNew ) =
                            insertHelp key value nLeft
                    in
                    ( balance nColor nKey nValue newLeft nRight, isNew )

                EQ ->
                    ( DictByTypeVariableFromContextInternal.InnerNode nColor nKey value nLeft nRight, False )

                GT ->
                    let
                        ( newRight, isNew ) =
                            insertHelp key value nRight
                    in
                    ( balance nColor nKey nValue nLeft newRight, isNew )


insertHelpNoReplace : TypeVariableFromContext -> v -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v -> ( DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v, Bool )
insertHelpNoReplace key value dict =
    -- IGNORE TCO
    case dict of
        DictByTypeVariableFromContextInternal.Leaf ->
            -- New nodes are always red. If it violates the rules, it will be fixed
            -- when balancing.
            ( DictByTypeVariableFromContextInternal.InnerNode True key value DictByTypeVariableFromContextInternal.Leaf DictByTypeVariableFromContextInternal.Leaf, True )

        DictByTypeVariableFromContextInternal.InnerNode nColor nKey nValue nLeft nRight ->
            case TypeVariableFromContext.compare key nKey of
                LT ->
                    let
                        ( newLeft, isNew ) =
                            insertHelpNoReplace key value nLeft
                    in
                    ( balance nColor nKey nValue newLeft nRight, isNew )

                EQ ->
                    ( dict, False )

                GT ->
                    let
                        ( newRight, isNew ) =
                            insertHelpNoReplace key value nRight
                    in
                    ( balance nColor nKey nValue nLeft newRight, isNew )


balance : Bool -> TypeVariableFromContext -> v -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v
balance color key value left right =
    case right of
        DictByTypeVariableFromContextInternal.InnerNode True rK rV rLeft rRight ->
            case left of
                DictByTypeVariableFromContextInternal.InnerNode True lK lV lLeft lRight ->
                    DictByTypeVariableFromContextInternal.InnerNode
                        True
                        key
                        value
                        (DictByTypeVariableFromContextInternal.InnerNode False lK lV lLeft lRight)
                        (DictByTypeVariableFromContextInternal.InnerNode False rK rV rLeft rRight)

                _ ->
                    DictByTypeVariableFromContextInternal.InnerNode color rK rV (DictByTypeVariableFromContextInternal.InnerNode True key value left rLeft) rRight

        _ ->
            case left of
                DictByTypeVariableFromContextInternal.InnerNode True lK lV (DictByTypeVariableFromContextInternal.InnerNode True llK llV llLeft llRight) lRight ->
                    DictByTypeVariableFromContextInternal.InnerNode
                        True
                        lK
                        lV
                        (DictByTypeVariableFromContextInternal.InnerNode False llK llV llLeft llRight)
                        (DictByTypeVariableFromContextInternal.InnerNode False key value lRight right)

                _ ->
                    DictByTypeVariableFromContextInternal.InnerNode color key value left right


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : TypeVariableFromContext -> DictByTypeVariableFromContext v -> DictByTypeVariableFromContext v
remove key ((DictByTypeVariableFromContextInternal.DictByTypeVariableFromContext sz dict) as orig) =
    let
        ( result, wasMember ) =
            removeInner key dict
    in
    if wasMember then
        DictByTypeVariableFromContext (sz - 1) result

    else
        orig


removeInner : TypeVariableFromContext -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v -> ( DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v, Bool )
removeInner key dict =
    -- Root node is always False
    case removeHelp key dict of
        ( DictByTypeVariableFromContextInternal.InnerNode True k v l r, wasMember ) ->
            ( DictByTypeVariableFromContextInternal.InnerNode False k v l r, wasMember )

        x ->
            x


{-| The easiest thing to remove from the tree, is a red node. However, when searching for the
node to remove, we have no way of knowing if it will be red or not. This remove implementation
makes sure that the bottom node is red by moving red colors down the tree through rotation
and color flips. Any violations this will cause, can easily be fixed by balancing on the way
up again.
-}
removeHelp : TypeVariableFromContext -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v -> ( DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v, Bool )
removeHelp targetKey dict =
    -- IGNORE TCO
    case dict of
        DictByTypeVariableFromContextInternal.Leaf ->
            ( DictByTypeVariableFromContextInternal.Leaf, False )

        DictByTypeVariableFromContextInternal.InnerNode color key value left right ->
            if TypeVariableFromContext.lessThan targetKey key then
                case left of
                    DictByTypeVariableFromContextInternal.InnerNode False _ _ lLeft _ ->
                        case lLeft of
                            DictByTypeVariableFromContextInternal.InnerNode True _ _ _ _ ->
                                let
                                    ( newLeft, wasMember ) =
                                        removeHelp targetKey left
                                in
                                ( DictByTypeVariableFromContextInternal.InnerNode color key value newLeft right, wasMember )

                            _ ->
                                let
                                    res : { color : Bool, k : TypeVariableFromContext, v : v, left : DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v, right : DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v }
                                    res =
                                        moveRedLeft color key value left right

                                    ( newLeft, wasMember ) =
                                        removeHelp targetKey res.left
                                in
                                ( balance res.color res.k res.v newLeft res.right, wasMember )

                    _ ->
                        let
                            ( newLeft, wasMember ) =
                                removeHelp targetKey left
                        in
                        ( DictByTypeVariableFromContextInternal.InnerNode color key value newLeft right, wasMember )

            else
                removeHelpEQGT targetKey (removeHelpPrepEQGT dict color key value left right)


removeHelpPrepEQGT : DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v -> Bool -> TypeVariableFromContext -> v -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v
removeHelpPrepEQGT dict color key value left right =
    case left of
        DictByTypeVariableFromContextInternal.InnerNode True lK lV lLeft lRight ->
            DictByTypeVariableFromContextInternal.InnerNode
                color
                lK
                lV
                lLeft
                (DictByTypeVariableFromContextInternal.InnerNode True key value lRight right)

        DictByTypeVariableFromContextInternal.InnerNode False lK lV lLeft lRight ->
            case right of
                DictByTypeVariableFromContextInternal.InnerNode False rK rV ((DictByTypeVariableFromContextInternal.InnerNode False _ _ _ _) as rLeft) rRight ->
                    moveRedRight key value lK lV lLeft lRight rK rV rLeft rRight

                DictByTypeVariableFromContextInternal.InnerNode False rK rV DictByTypeVariableFromContextInternal.Leaf rRight ->
                    moveRedRight key value lK lV lLeft lRight rK rV DictByTypeVariableFromContextInternal.Leaf rRight

                _ ->
                    dict

        DictByTypeVariableFromContextInternal.Leaf ->
            dict


{-| When we find the node we are looking for, we can remove by replacing the key-value
pair with the key-value pair of the left-most node on the right side (the closest pair).
-}
removeHelpEQGT : TypeVariableFromContext -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v -> ( DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v, Bool )
removeHelpEQGT targetKey dict =
    case dict of
        DictByTypeVariableFromContextInternal.InnerNode color key value left right ->
            if TypeVariableFromContext.equals targetKey key then
                case getMinInner right of
                    Just ( minKey, minValue ) ->
                        ( balance color minKey minValue left (removeMin right), True )

                    Nothing ->
                        ( DictByTypeVariableFromContextInternal.Leaf, True )

            else
                let
                    ( newRight, wasMember ) =
                        removeHelp targetKey right
                in
                ( balance color key value left newRight, wasMember )

        DictByTypeVariableFromContextInternal.Leaf ->
            ( DictByTypeVariableFromContextInternal.Leaf, False )


removeMin : DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v
removeMin dict =
    -- IGNORE TCO
    case dict of
        DictByTypeVariableFromContextInternal.InnerNode color key value ((DictByTypeVariableFromContextInternal.InnerNode lColor _ _ lLeft _) as left) right ->
            if lColor then
                DictByTypeVariableFromContextInternal.InnerNode color key value (removeMin left) right

            else
                case lLeft of
                    DictByTypeVariableFromContextInternal.InnerNode True _ _ _ _ ->
                        DictByTypeVariableFromContextInternal.InnerNode color key value (removeMin left) right

                    _ ->
                        let
                            res : { color : Bool, k : TypeVariableFromContext, v : v, left : DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v, right : DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v }
                            res =
                                moveRedLeft color key value left right
                        in
                        balance res.color res.k res.v (removeMin res.left) res.right

        _ ->
            DictByTypeVariableFromContextInternal.Leaf


moveRedLeft :
    Bool
    -> TypeVariableFromContext
    -> v
    -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v
    -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v
    ->
        { color : Bool
        , k : TypeVariableFromContext
        , v : v
        , left : DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v
        , right : DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v
        }
moveRedLeft clr k v left right =
    case left of
        DictByTypeVariableFromContextInternal.InnerNode _ lK lV lLeft lRight ->
            case right of
                DictByTypeVariableFromContextInternal.InnerNode _ rK rV (DictByTypeVariableFromContextInternal.InnerNode True rlK rlV rlL rlR) rRight ->
                    { color = True
                    , k = rlK
                    , v = rlV
                    , left = DictByTypeVariableFromContextInternal.InnerNode False k v (DictByTypeVariableFromContextInternal.InnerNode True lK lV lLeft lRight) rlL
                    , right = DictByTypeVariableFromContextInternal.InnerNode False rK rV rlR rRight
                    }

                DictByTypeVariableFromContextInternal.InnerNode _ rK rV rLeft rRight ->
                    { color = False
                    , k = k
                    , v = v
                    , left = DictByTypeVariableFromContextInternal.InnerNode True lK lV lLeft lRight
                    , right = DictByTypeVariableFromContextInternal.InnerNode True rK rV rLeft rRight
                    }

                _ ->
                    { color = clr, k = k, v = v, left = left, right = right }

        DictByTypeVariableFromContextInternal.Leaf ->
            { color = clr, k = k, v = v, left = left, right = right }


moveRedRight :
    TypeVariableFromContext
    -> v
    -> TypeVariableFromContext
    -> v
    -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v
    -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v
    -> TypeVariableFromContext
    -> v
    -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v
    -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v
    -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v
moveRedRight key value lK lV lLeft lRight rK rV rLeft rRight =
    case lLeft of
        DictByTypeVariableFromContextInternal.InnerNode True llK llV llLeft llRight ->
            DictByTypeVariableFromContextInternal.InnerNode
                True
                lK
                lV
                (DictByTypeVariableFromContextInternal.InnerNode False llK llV llLeft llRight)
                (DictByTypeVariableFromContextInternal.InnerNode False key value lRight (DictByTypeVariableFromContextInternal.InnerNode True rK rV rLeft rRight))

        _ ->
            DictByTypeVariableFromContextInternal.InnerNode
                False
                key
                value
                (DictByTypeVariableFromContextInternal.InnerNode True lK lV lLeft lRight)
                (DictByTypeVariableFromContextInternal.InnerNode True rK rV rLeft rRight)


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : TypeVariableFromContext -> (Maybe v -> Maybe v) -> DictByTypeVariableFromContext v -> DictByTypeVariableFromContext v
update targetKey alter dictionary =
    case alter (get targetKey dictionary) of
        Just value ->
            insert targetKey value dictionary

        Nothing ->
            remove targetKey dictionary


{-| Create a dictionary with one key-value pair.
-}
singleton : TypeVariableFromContext -> v -> DictByTypeVariableFromContext v
singleton key value =
    -- Root node is always False
    DictByTypeVariableFromContext 1
        (DictByTypeVariableFromContextInternal.InnerNode
            False
            key
            value
            DictByTypeVariableFromContextInternal.Leaf
            DictByTypeVariableFromContextInternal.Leaf
        )


{-| Faster equivalent of `singleton aKey aValue |> insert bKey bValue`
in case you know with certainty that `aKey` and `bKey` are not equal
-}
twoDistinct :
    TypeVariableFromContext
    -> v
    -> TypeVariableFromContext
    -> v
    -> DictByTypeVariableFromContext v
twoDistinct aKey aValue bKey bValue =
    DictByTypeVariableFromContext 2
        (if TypeVariableFromContext.lessThan aKey bKey then
            DictByTypeVariableFromContextInternal.InnerNode
                False
                bKey
                bValue
                (DictByTypeVariableFromContextInternal.InnerNode
                    True
                    aKey
                    aValue
                    DictByTypeVariableFromContextInternal.Leaf
                    DictByTypeVariableFromContextInternal.Leaf
                )
                DictByTypeVariableFromContextInternal.Leaf

         else
            DictByTypeVariableFromContextInternal.InnerNode
                False
                aKey
                aValue
                (DictByTypeVariableFromContextInternal.InnerNode
                    True
                    bKey
                    bValue
                    DictByTypeVariableFromContextInternal.Leaf
                    DictByTypeVariableFromContextInternal.Leaf
                )
                DictByTypeVariableFromContextInternal.Leaf
        )



-- COMBINE


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : DictByTypeVariableFromContext v -> DictByTypeVariableFromContext v -> DictByTypeVariableFromContext v
union ((DictByTypeVariableFromContextInternal.DictByTypeVariableFromContext s1 _) as t1) ((DictByTypeVariableFromContextInternal.DictByTypeVariableFromContext s2 _) as t2) =
    -- -- TODO: Find a data-based heuristic instead of the vibe-based "2 *"
    -- if s1 > 2 * s2 then
    --     foldl insertNoReplace t1 t2
    -- else if s2 > 2 * s1 then
    --     foldl insert t2 t1
    -- else
    --     Union.union t1 t2
    if s1 - s2 > 0 then
        foldl insertNoReplace t1 t2

    else
        foldl insert t2 t1


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : DictByTypeVariableFromContext v -> DictByTypeVariableFromContext v -> DictByTypeVariableFromContext v
intersect (DictByTypeVariableFromContextInternal.DictByTypeVariableFromContext sz1 t1) (DictByTypeVariableFromContextInternal.DictByTypeVariableFromContext sz2 t2) =
    -- possible optimization: sz1 * sz2 == 0
    if sz1 == 0 || sz2 == 0 then
        empty

    else
        -- Now t1 and t2 are never leaves, so we have an invariant that queues never contain leaves
        intersectFromZipper
            ListWithLength.empty
            (DictByTypeVariableFromContextInternal.unconsBiggest [ t1 ])
            (DictByTypeVariableFromContextInternal.unconsBiggest [ t2 ])
            |> DictByTypeVariableFromContextInternal.fromSortedList


intersectFromZipper :
    ListWithLength ( TypeVariableFromContext, v )
    -> Maybe ( TypeVariableFromContext, v, DictByTypeVariableFromContextInternal.VisitQueue v )
    -> Maybe ( TypeVariableFromContext, v, DictByTypeVariableFromContextInternal.VisitQueue v )
    -> ListWithLength ( TypeVariableFromContext, v )
intersectFromZipper dacc lleft rleft =
    case lleft of
        Nothing ->
            dacc

        Just ( lkey, lvalue, ltail ) ->
            case rleft of
                Nothing ->
                    dacc

                Just ( rkey, _, rtail ) ->
                    if TypeVariableFromContext.greaterThan lkey rkey then
                        intersectFromZipper dacc (DictByTypeVariableFromContextInternal.unconsBiggestWhileDroppingGT rkey ltail) rleft

                    else if TypeVariableFromContext.greaterThan rkey lkey then
                        intersectFromZipper dacc lleft (DictByTypeVariableFromContextInternal.unconsBiggestWhileDroppingGT lkey rtail)

                    else
                        intersectFromZipper (ListWithLength.cons ( lkey, lvalue ) dacc) (DictByTypeVariableFromContextInternal.unconsBiggest ltail) (DictByTypeVariableFromContextInternal.unconsBiggest rtail)


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : DictByTypeVariableFromContext a -> DictByTypeVariableFromContext b_ -> DictByTypeVariableFromContext a
diff ((DictByTypeVariableFromContextInternal.DictByTypeVariableFromContext sz1 _) as t1) t2 =
    if sz1 == 0 then
        empty

    else
        foldl (\k _ t -> remove k t) t1 t2


{-| The most general way of combining two dictionaries. You provide three
accumulators for when a given key appears:

1.  Only in the left dictionary.
2.  In both dictionaries.
3.  Only in the right dictionary.

You then traverse all the keys from lowest to highest, building up whatever
you want.

-}
merge :
    (TypeVariableFromContext -> a -> result -> result)
    -> (TypeVariableFromContext -> a -> b -> result -> result)
    -> (TypeVariableFromContext -> b -> result -> result)
    -> DictByTypeVariableFromContext a
    -> DictByTypeVariableFromContext b
    -> result
    -> result
merge leftStep bothStep rightStep leftDict rightDict initialResult =
    let
        stepState : TypeVariableFromContext -> b -> ( List ( TypeVariableFromContext, a ), result ) -> ( List ( TypeVariableFromContext, a ), result )
        stepState rKey rValue ( list, result ) =
            case list of
                [] ->
                    ( list, rightStep rKey rValue result )

                ( lKey, lValue ) :: rest ->
                    if TypeVariableFromContext.lessThan lKey rKey then
                        stepState rKey rValue ( rest, leftStep lKey lValue result )

                    else if TypeVariableFromContext.greaterThan lKey rKey then
                        ( list, rightStep rKey rValue result )

                    else
                        ( rest, bothStep lKey lValue rValue result )

        ( leftovers, intermediateResult ) =
            foldl stepState ( toList leftDict, initialResult ) rightDict
    in
    List.foldl (\( k, v ) result -> leftStep k v result) intermediateResult leftovers



-- TRANSFORM


{-| Apply a function to all values in a dictionary.
-}
map : (TypeVariableFromContext -> a -> b) -> DictByTypeVariableFromContext a -> DictByTypeVariableFromContext b
map func (DictByTypeVariableFromContextInternal.DictByTypeVariableFromContext sz dict) =
    DictByTypeVariableFromContext sz (mapInner func dict)


mapInner : (TypeVariableFromContext -> a -> b) -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext a -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext b
mapInner func dict =
    -- IGNORE TCO
    case dict of
        DictByTypeVariableFromContextInternal.Leaf ->
            DictByTypeVariableFromContextInternal.Leaf

        DictByTypeVariableFromContextInternal.InnerNode color key value left right ->
            DictByTypeVariableFromContextInternal.InnerNode color key (func key value) (mapInner func left) (mapInner func right)


{-| Fold over the key-value pairs in a dictionary from lowest key to highest key.

    getAges : Dict String Int -> List Int
    getAges usersDict =
        FastDict.foldl addAge [] usersDict

    addAge : String -> Int -> List Int -> List Int
    addAge _ age ages =
        age :: ages

    users : Dict String Int
    users =
        FastDict.fromList
            [ ( "Abe", 28 )
            , ( "Beatrix", 19 )
            , ( "Charlotte", 33 )
            ]

    -- Note that the _fold_ is from lowest to highest,
    -- but because we're adding items to the beginning of the list
    -- the result will be from highest to lowest.

    getAges users
    --> [ 33, 19, 28 ]

-}
foldl : (TypeVariableFromContext -> v -> b -> b) -> b -> DictByTypeVariableFromContext v -> b
foldl func acc (DictByTypeVariableFromContextInternal.DictByTypeVariableFromContext _ dict) =
    foldlInner func acc dict


foldlInner : (TypeVariableFromContext -> v -> b -> b) -> b -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v -> b
foldlInner func acc dict =
    case dict of
        DictByTypeVariableFromContextInternal.Leaf ->
            acc

        DictByTypeVariableFromContextInternal.InnerNode _ key value left right ->
            foldlInner func (func key value (foldlInner func acc left)) right


{-| Fold over the key-value pairs in a dictionary from highest key to lowest key.

    getAges : Dict String Int -> List Int
    getAges usersDict =
        FastDict.foldr addAge [] usersDict

    addAge : String -> Int -> List Int -> List Int
    addAge _ age ages =
        age :: ages

    users : Dict String Int
    users =
        FastDict.fromList
            [ ( "Abe", 28 )
            , ( "Beatrix", 19 )
            , ( "Charlotte", 33 )
            ]

    -- Note that the _fold_ is from highest to lowest,
    -- but because we're adding items to the beginning of the list
    -- the result will be from lowest to highest.

    getAges users
    --> [ 28, 19, 33 ]

-}
foldr : (TypeVariableFromContext -> v -> b -> b) -> b -> DictByTypeVariableFromContext v -> b
foldr func acc (DictByTypeVariableFromContextInternal.DictByTypeVariableFromContext _ dict) =
    foldrInner func acc dict


foldrInner :
    (TypeVariableFromContext -> v -> b -> b)
    -> b
    -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext v
    -> b
foldrInner func acc t =
    case t of
        DictByTypeVariableFromContextInternal.Leaf ->
            acc

        DictByTypeVariableFromContextInternal.InnerNode _ key value left right ->
            foldrInner func (func key value (foldrInner func acc right)) left


{-| Keep only the key-value pairs that pass the given test.
-}
filter : (TypeVariableFromContext -> v -> Bool) -> DictByTypeVariableFromContext v -> DictByTypeVariableFromContext v
filter isGood dict =
    foldl
        (\k v d ->
            if isGood k v then
                insert k v d

            else
                d
        )
        empty
        dict


{-| Partition a dictionary according to some test. The first dictionary
contains all key-value pairs which passed the test, and the second contains
the pairs that did not.
-}
partition : (TypeVariableFromContext -> v -> Bool) -> DictByTypeVariableFromContext v -> ( DictByTypeVariableFromContext v, DictByTypeVariableFromContext v )
partition isGood dict =
    let
        add : TypeVariableFromContext -> v -> ( DictByTypeVariableFromContext v, DictByTypeVariableFromContext v ) -> ( DictByTypeVariableFromContext v, DictByTypeVariableFromContext v )
        add key value ( t1, t2 ) =
            if isGood key value then
                ( insert key value t1, t2 )

            else
                ( t1, insert key value t2 )
    in
    foldl add ( empty, empty ) dict



-- LISTS


{-| Get all of the keys in a dictionary, sorted from lowest to highest.

    keys (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ])
    --> [ 0, 1 ]

-}
keys : DictByTypeVariableFromContext v_ -> List TypeVariableFromContext
keys dict =
    foldr (\key _ keyList -> key :: keyList) [] dict


{-| Get all of the values in a dictionary, in the order of their keys.

    values (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ])
    --> [ "Alice", "Bob" ]

-}
values : DictByTypeVariableFromContext v -> List v
values dict =
    foldr (\_ value valueList -> value :: valueList) [] dict


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : DictByTypeVariableFromContext v -> List ( TypeVariableFromContext, v )
toList dict =
    foldr (\key value list -> ( key, value ) :: list) [] dict


{-| Convert an association list into a dictionary.
-}
fromList : List ( TypeVariableFromContext, v ) -> DictByTypeVariableFromContext v
fromList assocs =
    List.foldl (\( key, value ) dict -> insert key value dict) empty assocs


{-| Convert an association list into a dictionary.
-}
fromListFast : List ( TypeVariableFromContext, v ) -> DictByTypeVariableFromContext v
fromListFast assocs =
    let
        dedup : List ( TypeVariableFromContext, v ) -> ListWithLength ( TypeVariableFromContext, v )
        dedup xs =
            case xs of
                [] ->
                    ListWithLength.empty

                head :: tail ->
                    dedupHelp head tail ListWithLength.empty

        dedupHelp : ( TypeVariableFromContext, v ) -> List ( TypeVariableFromContext, v ) -> ListWithLength ( TypeVariableFromContext, v ) -> ListWithLength ( TypeVariableFromContext, v )
        dedupHelp (( lastKey, _ ) as last) todo acc =
            case todo of
                [] ->
                    ListWithLength.cons last acc

                (( todoHeadKey, _ ) as todoHead) :: todoTail ->
                    let
                        newAcc : ListWithLength ( TypeVariableFromContext, v )
                        newAcc =
                            if TypeVariableFromContext.equals todoHeadKey lastKey then
                                acc

                            else
                                ListWithLength.cons last acc
                    in
                    dedupHelp todoHead todoTail newAcc
    in
    assocs
        |> -- Intentionally swap k1 and k2 here to have a reverse sort so we can do dedup in one pass
           List.sortWith (\( k1, _ ) ( k2, _ ) -> TypeVariableFromContext.compare k2 k1)
        |> dedup
        |> DictByTypeVariableFromContextInternal.fromSortedList



-- INTEROPERABILITY


{-| Convert the dictionary into an equivalent one from elm/core.
-}
toCoreDict : DictByTypeVariableFromContext v -> Dict.Dict TypeVariableFromContext v
toCoreDict dict =
    foldl Dict.insert Dict.empty dict


{-| Convert the dictionary from an equivalent one from elm/core.
-}
fromCoreDict : Dict.Dict TypeVariableFromContext v -> DictByTypeVariableFromContext v
fromCoreDict dict =
    Dict.foldl insert empty dict



-- ADVANCED


{-| This allows you to take advantage of the tree structure of the dictionary to do some operations more efficiently.

Calling `left` will give the result of calling `restructure` on the left subtree (lower keys), `right` on the right one (higher keys).

If this is confusing you probably don't need this function!

    any dict =
        -- Notice how if `value` is `True` we don't call `left` nor `right`,
        -- and if `value` is `False` but `left ()` is `True` we don't call right.
        restructure False (\{ value, left, right } -> value || left () || right ())

-}
restructure :
    acc
    -> ({ key : TypeVariableFromContext, value : value, left : () -> acc, right : () -> acc } -> acc)
    -> DictByTypeVariableFromContext value
    -> acc
restructure leafFunc nodeFunc (DictByTypeVariableFromContextInternal.DictByTypeVariableFromContext _ dict) =
    restructureInner leafFunc nodeFunc dict


restructureInner :
    acc
    -> ({ key : TypeVariableFromContext, value : value, left : () -> acc, right : () -> acc } -> acc)
    -> DictByTypeVariableFromContextInternal.InnerDictByTypeVariableFromContext value
    -> acc
restructureInner leafFunc nodeFunc dict =
    -- IGNORE TCO
    case dict of
        DictByTypeVariableFromContextInternal.Leaf ->
            leafFunc

        DictByTypeVariableFromContextInternal.InnerNode _ key value left right ->
            nodeFunc
                { key = key
                , value = value
                , left = \() -> restructureInner leafFunc nodeFunc left
                , right = \() -> restructureInner leafFunc nodeFunc right
                }
