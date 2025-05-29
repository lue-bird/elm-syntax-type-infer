module DictByTypeVariableFromContext exposing
    ( DictByTypeVariableFromContext
    , empty, singleton, twoDistinct, insert, update, remove
    , isEmpty, member, get, size, equals, any
    , getMinKey, getMin, getMaxKey, getMax
    , keys, values, toList, fromList
    , map, foldl, foldr, foldlWhileOkFrom, filter
    , union, intersect, diff, merge
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


# Lists

@docs keys, values, toList, fromList


# Transform

@docs map, foldl, foldr, foldlWhileOkFrom, filter


# Combine

@docs union, intersect, diff, merge

-}

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
type DictByTypeVariableFromContext v
    = DictByTypeVariableFromContext Int (InnerDictByTypeVariableFromContext v)



-- The color of a node. Leaves are considered False.


type InnerDictByTypeVariableFromContext v
    = InnerNode {- True = Red, False = Black -} Bool TypeVariableFromContext v (InnerDictByTypeVariableFromContext v) (InnerDictByTypeVariableFromContext v)
    | Leaf


{-| Builds a Dict from an already sorted list.

WARNING: This does _not_ check that the list is sorted.

-}
innerFromSortedList :
    { length : Int, list : List ( TypeVariableFromContext, v ) }
    -> DictByTypeVariableFromContext v
innerFromSortedList associationList =
    let
        redLayer : Int
        redLayer =
            floor (logBase 2 (toFloat associationList.length))
    in
    innerFromSortedListHelp redLayer 0 0 associationList.length associationList.list
        |> Tuple.first
        |> DictByTypeVariableFromContext associationList.length


innerFromSortedListHelp : Int -> Int -> Int -> Int -> List ( TypeVariableFromContext, v ) -> ( InnerDictByTypeVariableFromContext v, List ( TypeVariableFromContext, v ) )
innerFromSortedListHelp redLayer layer fromIncluded toExcluded acc =
    -- IGNORE TCO
    if fromIncluded >= toExcluded then
        ( Leaf, acc )

    else
        let
            mid : Int
            mid =
                fromIncluded + (toExcluded - fromIncluded) // 2

            ( lchild, accAfterLeft ) =
                innerFromSortedListHelp redLayer (layer + 1) fromIncluded mid acc
        in
        case accAfterLeft of
            [] ->
                ( Leaf, acc )

            ( k, v ) :: tail ->
                let
                    ( rchild, accAfterRight ) =
                        innerFromSortedListHelp redLayer (layer + 1) (mid + 1) toExcluded tail
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


{-| Create an empty dictionary.
-}
empty : DictByTypeVariableFromContext v_
empty =
    DictByTypeVariableFromContext 0 Leaf


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
get targetKey (DictByTypeVariableFromContext _ dict) =
    getInner targetKey dict


getInner : TypeVariableFromContext -> InnerDictByTypeVariableFromContext v -> Maybe v
getInner targetKey dict =
    case dict of
        Leaf ->
            Nothing

        InnerNode _ key value left right ->
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
member targetKey (DictByTypeVariableFromContext _ dict) =
    memberInner targetKey dict


memberInner : TypeVariableFromContext -> InnerDictByTypeVariableFromContext v_ -> Bool
memberInner targetKey dict =
    case dict of
        Leaf ->
            False

        InnerNode _ key _ left right ->
            case TypeVariableFromContext.compare targetKey key of
                LT ->
                    memberInner targetKey left

                EQ ->
                    True

                GT ->
                    memberInner targetKey right


{-| Determine the number of key-value pairs in the dictionary.
-}
size : DictByTypeVariableFromContext v_ -> Int
size (DictByTypeVariableFromContext sz _) =
    sz


{-| Determine if two dictionaries are equal. This is needed because the structure could be different depending on insertion order.
-}
equals : DictByTypeVariableFromContext v -> DictByTypeVariableFromContext v -> Bool
equals (DictByTypeVariableFromContext lsz lRoot) (DictByTypeVariableFromContext rsz rRoot) =
    (lsz - rsz == 0)
        && equalsHelp (unconsBiggest [ lRoot ])
            (unconsBiggest [ rRoot ])


equalsHelp : Maybe ( TypeVariableFromContext, v, VisitQueue v ) -> Maybe ( TypeVariableFromContext, v, VisitQueue v ) -> Bool
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
                        equalsHelp (unconsBiggest lRest) (unconsBiggest rRest)

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
getMinKey (DictByTypeVariableFromContext _ dict) =
    getMinKeyInner dict


getMinKeyInner : InnerDictByTypeVariableFromContext v_ -> Maybe TypeVariableFromContext
getMinKeyInner dict =
    case dict of
        Leaf ->
            Nothing

        InnerNode _ k _ left _ ->
            case left of
                Leaf ->
                    Just k

                _ ->
                    getMinKeyInner left


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
getMaxKey (DictByTypeVariableFromContext _ dict) =
    getMaxKeyInner dict


getMaxKeyInner : InnerDictByTypeVariableFromContext v_ -> Maybe TypeVariableFromContext
getMaxKeyInner dict =
    case dict of
        Leaf ->
            Nothing

        InnerNode _ k _ _ right ->
            case right of
                Leaf ->
                    Just k

                _ ->
                    getMaxKeyInner right


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
getMin (DictByTypeVariableFromContext _ dict) =
    getMinInner dict


getMinInner : InnerDictByTypeVariableFromContext v -> Maybe ( TypeVariableFromContext, v )
getMinInner n =
    case n of
        Leaf ->
            Nothing

        InnerNode _ k v left _ ->
            case left of
                Leaf ->
                    Just ( k, v )

                _ ->
                    getMinInner left


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
getMax (DictByTypeVariableFromContext _ dict) =
    getMaxInner dict


getMaxInner : InnerDictByTypeVariableFromContext v -> Maybe ( TypeVariableFromContext, v )
getMaxInner dict =
    case dict of
        Leaf ->
            Nothing

        InnerNode _ k v _ right ->
            case right of
                Leaf ->
                    Just ( k, v )

                _ ->
                    getMaxInner right


any :
    (TypeVariableFromContext -> value -> Bool)
    -> DictByTypeVariableFromContext value
    -> Bool
any isNeedle (DictByTypeVariableFromContext _ dict) =
    anyInner isNeedle dict


anyInner :
    (TypeVariableFromContext -> value -> Bool)
    -> InnerDictByTypeVariableFromContext value
    -> Bool
anyInner isNeedle dict =
    -- IGNORE TCO
    case dict of
        Leaf ->
            False

        InnerNode _ key value left right ->
            -- not all in one || to get a bit of TCO action
            if
                isNeedle key value
                    || anyInner isNeedle left
            then
                True

            else
                anyInner isNeedle right


{-| Determine if a dictionary is empty.

    isEmpty empty
    --> True

-}
isEmpty : DictByTypeVariableFromContext v_ -> Bool
isEmpty (DictByTypeVariableFromContext dictSize _) =
    dictSize == 0


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : TypeVariableFromContext -> v -> DictByTypeVariableFromContext v -> DictByTypeVariableFromContext v
insert key value (DictByTypeVariableFromContext sz dict) =
    let
        ( result, isNew ) =
            insertInner key value dict
    in
    if isNew then
        DictByTypeVariableFromContext (sz + 1) result

    else
        DictByTypeVariableFromContext sz result


insertNoReplace : TypeVariableFromContext -> v -> DictByTypeVariableFromContext v -> DictByTypeVariableFromContext v
insertNoReplace key value (DictByTypeVariableFromContext sz dict) =
    let
        ( result, isNew ) =
            insertInnerNoReplace key value dict
    in
    if isNew then
        DictByTypeVariableFromContext (sz + 1) result

    else
        DictByTypeVariableFromContext sz result


insertInner : TypeVariableFromContext -> v -> InnerDictByTypeVariableFromContext v -> ( InnerDictByTypeVariableFromContext v, Bool )
insertInner key value dict =
    -- Root node is always False
    case insertHelp key value dict of
        ( InnerNode True k v l r, isNew ) ->
            ( InnerNode False k v l r, isNew )

        x ->
            x


insertInnerNoReplace : TypeVariableFromContext -> v -> InnerDictByTypeVariableFromContext v -> ( InnerDictByTypeVariableFromContext v, Bool )
insertInnerNoReplace key value dict =
    -- Root node is always False
    case insertHelpNoReplace key value dict of
        ( InnerNode True k v l r, isNew ) ->
            ( InnerNode False k v l r, isNew )

        x ->
            x


insertHelp : TypeVariableFromContext -> v -> InnerDictByTypeVariableFromContext v -> ( InnerDictByTypeVariableFromContext v, Bool )
insertHelp key value dict =
    -- IGNORE TCO
    case dict of
        Leaf ->
            -- New nodes are always red. If it violates the rules, it will be fixed
            -- when balancing.
            ( InnerNode True key value Leaf Leaf, True )

        InnerNode nColor nKey nValue nLeft nRight ->
            case TypeVariableFromContext.compare key nKey of
                LT ->
                    let
                        ( newLeft, isNew ) =
                            insertHelp key value nLeft
                    in
                    ( balance nColor nKey nValue newLeft nRight, isNew )

                EQ ->
                    ( InnerNode nColor nKey value nLeft nRight, False )

                GT ->
                    let
                        ( newRight, isNew ) =
                            insertHelp key value nRight
                    in
                    ( balance nColor nKey nValue nLeft newRight, isNew )


insertHelpNoReplace : TypeVariableFromContext -> v -> InnerDictByTypeVariableFromContext v -> ( InnerDictByTypeVariableFromContext v, Bool )
insertHelpNoReplace key value dict =
    -- IGNORE TCO
    case dict of
        Leaf ->
            -- New nodes are always red. If it violates the rules, it will be fixed
            -- when balancing.
            ( InnerNode True key value Leaf Leaf, True )

        InnerNode nColor nKey nValue nLeft nRight ->
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


balance : Bool -> TypeVariableFromContext -> v -> InnerDictByTypeVariableFromContext v -> InnerDictByTypeVariableFromContext v -> InnerDictByTypeVariableFromContext v
balance color key value left right =
    case right of
        InnerNode True rK rV rLeft rRight ->
            case left of
                InnerNode True lK lV lLeft lRight ->
                    InnerNode
                        True
                        key
                        value
                        (InnerNode False lK lV lLeft lRight)
                        (InnerNode False rK rV rLeft rRight)

                _ ->
                    InnerNode color rK rV (InnerNode True key value left rLeft) rRight

        _ ->
            case left of
                InnerNode True lK lV (InnerNode True llK llV llLeft llRight) lRight ->
                    InnerNode
                        True
                        lK
                        lV
                        (InnerNode False llK llV llLeft llRight)
                        (InnerNode False key value lRight right)

                _ ->
                    InnerNode color key value left right


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : TypeVariableFromContext -> DictByTypeVariableFromContext v -> DictByTypeVariableFromContext v
remove key ((DictByTypeVariableFromContext sz dict) as orig) =
    let
        ( result, wasMember ) =
            removeInner key dict
    in
    if wasMember then
        DictByTypeVariableFromContext (sz - 1) result

    else
        orig


removeInner : TypeVariableFromContext -> InnerDictByTypeVariableFromContext v -> ( InnerDictByTypeVariableFromContext v, Bool )
removeInner key dict =
    -- Root node is always False
    case removeHelp key dict of
        ( InnerNode True k v l r, wasMember ) ->
            ( InnerNode False k v l r, wasMember )

        x ->
            x


{-| The easiest thing to remove from the tree, is a red node. However, when searching for the
node to remove, we have no way of knowing if it will be red or not. This remove implementation
makes sure that the bottom node is red by moving red colors down the tree through rotation
and color flips. Any violations this will cause, can easily be fixed by balancing on the way
up again.
-}
removeHelp : TypeVariableFromContext -> InnerDictByTypeVariableFromContext v -> ( InnerDictByTypeVariableFromContext v, Bool )
removeHelp targetKey dict =
    -- IGNORE TCO
    case dict of
        Leaf ->
            ( Leaf, False )

        InnerNode color key value left right ->
            if TypeVariableFromContext.lessThan targetKey key then
                case left of
                    InnerNode False _ _ lLeft _ ->
                        case lLeft of
                            InnerNode True _ _ _ _ ->
                                let
                                    ( newLeft, wasMember ) =
                                        removeHelp targetKey left
                                in
                                ( InnerNode color key value newLeft right, wasMember )

                            _ ->
                                let
                                    res : { color : Bool, k : TypeVariableFromContext, v : v, left : InnerDictByTypeVariableFromContext v, right : InnerDictByTypeVariableFromContext v }
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
                        ( InnerNode color key value newLeft right, wasMember )

            else
                removeHelpEQGT targetKey (removeHelpPrepEQGT dict color key value left right)


removeHelpPrepEQGT : InnerDictByTypeVariableFromContext v -> Bool -> TypeVariableFromContext -> v -> InnerDictByTypeVariableFromContext v -> InnerDictByTypeVariableFromContext v -> InnerDictByTypeVariableFromContext v
removeHelpPrepEQGT dict color key value left right =
    case left of
        InnerNode True lK lV lLeft lRight ->
            InnerNode
                color
                lK
                lV
                lLeft
                (InnerNode True key value lRight right)

        InnerNode False lK lV lLeft lRight ->
            case right of
                InnerNode False rK rV ((InnerNode False _ _ _ _) as rLeft) rRight ->
                    moveRedRight key value lK lV lLeft lRight rK rV rLeft rRight

                InnerNode False rK rV Leaf rRight ->
                    moveRedRight key value lK lV lLeft lRight rK rV Leaf rRight

                _ ->
                    dict

        Leaf ->
            dict


{-| When we find the node we are looking for, we can remove by replacing the key-value
pair with the key-value pair of the left-most node on the right side (the closest pair).
-}
removeHelpEQGT : TypeVariableFromContext -> InnerDictByTypeVariableFromContext v -> ( InnerDictByTypeVariableFromContext v, Bool )
removeHelpEQGT targetKey dict =
    case dict of
        InnerNode color key value left right ->
            if TypeVariableFromContext.equals targetKey key then
                case getMinInner right of
                    Just ( minKey, minValue ) ->
                        ( balance color minKey minValue left (removeMin right), True )

                    Nothing ->
                        ( Leaf, True )

            else
                let
                    ( newRight, wasMember ) =
                        removeHelp targetKey right
                in
                ( balance color key value left newRight, wasMember )

        Leaf ->
            ( Leaf, False )


removeMin : InnerDictByTypeVariableFromContext v -> InnerDictByTypeVariableFromContext v
removeMin dict =
    -- IGNORE TCO
    case dict of
        InnerNode color key value ((InnerNode lColor _ _ lLeft _) as left) right ->
            if lColor then
                InnerNode color key value (removeMin left) right

            else
                case lLeft of
                    InnerNode True _ _ _ _ ->
                        InnerNode color key value (removeMin left) right

                    _ ->
                        let
                            res : { color : Bool, k : TypeVariableFromContext, v : v, left : InnerDictByTypeVariableFromContext v, right : InnerDictByTypeVariableFromContext v }
                            res =
                                moveRedLeft color key value left right
                        in
                        balance res.color res.k res.v (removeMin res.left) res.right

        _ ->
            Leaf


moveRedLeft :
    Bool
    -> TypeVariableFromContext
    -> v
    -> InnerDictByTypeVariableFromContext v
    -> InnerDictByTypeVariableFromContext v
    ->
        { color : Bool
        , k : TypeVariableFromContext
        , v : v
        , left : InnerDictByTypeVariableFromContext v
        , right : InnerDictByTypeVariableFromContext v
        }
moveRedLeft clr k v left right =
    case left of
        InnerNode _ lK lV lLeft lRight ->
            case right of
                InnerNode _ rK rV (InnerNode True rlK rlV rlL rlR) rRight ->
                    { color = True
                    , k = rlK
                    , v = rlV
                    , left = InnerNode False k v (InnerNode True lK lV lLeft lRight) rlL
                    , right = InnerNode False rK rV rlR rRight
                    }

                InnerNode _ rK rV rLeft rRight ->
                    { color = False
                    , k = k
                    , v = v
                    , left = InnerNode True lK lV lLeft lRight
                    , right = InnerNode True rK rV rLeft rRight
                    }

                _ ->
                    { color = clr, k = k, v = v, left = left, right = right }

        Leaf ->
            { color = clr, k = k, v = v, left = left, right = right }


moveRedRight :
    TypeVariableFromContext
    -> v
    -> TypeVariableFromContext
    -> v
    -> InnerDictByTypeVariableFromContext v
    -> InnerDictByTypeVariableFromContext v
    -> TypeVariableFromContext
    -> v
    -> InnerDictByTypeVariableFromContext v
    -> InnerDictByTypeVariableFromContext v
    -> InnerDictByTypeVariableFromContext v
moveRedRight key value lK lV lLeft lRight rK rV rLeft rRight =
    case lLeft of
        InnerNode True llK llV llLeft llRight ->
            InnerNode
                True
                lK
                lV
                (InnerNode False llK llV llLeft llRight)
                (InnerNode False key value lRight (InnerNode True rK rV rLeft rRight))

        _ ->
            InnerNode
                False
                key
                value
                (InnerNode True lK lV lLeft lRight)
                (InnerNode True rK rV rLeft rRight)


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
        (InnerNode
            False
            key
            value
            Leaf
            Leaf
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
            InnerNode
                False
                bKey
                bValue
                (InnerNode
                    True
                    aKey
                    aValue
                    Leaf
                    Leaf
                )
                Leaf

         else
            InnerNode
                False
                aKey
                aValue
                (InnerNode
                    True
                    bKey
                    bValue
                    Leaf
                    Leaf
                )
                Leaf
        )


foldlWhileOkFrom :
    ok
    -> (TypeVariableFromContext -> value -> ok -> Result err ok)
    -> DictByTypeVariableFromContext value
    -> Result err ok
foldlWhileOkFrom initialFolded reduceToResult (DictByTypeVariableFromContext _ dict) =
    dict |> foldlWhileOkFromInner reduceToResult initialFolded


foldlWhileOkFromInner :
    (TypeVariableFromContext -> value -> ok -> Result err ok)
    -> ok
    -> InnerDictByTypeVariableFromContext value
    -> Result err ok
foldlWhileOkFromInner func initialFolded dict =
    -- IGNORE TCO
    case dict of
        Leaf ->
            Ok initialFolded

        InnerNode _ key value left right ->
            case foldlWhileOkFromInner func initialFolded left of
                Err error ->
                    Err error

                Ok leftFolded ->
                    case func key value leftFolded of
                        Err error ->
                            Err error

                        Ok leftAndCurrentFolded ->
                            foldlWhileOkFromInner func leftAndCurrentFolded right



-- COMBINE


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : DictByTypeVariableFromContext v -> DictByTypeVariableFromContext v -> DictByTypeVariableFromContext v
union ((DictByTypeVariableFromContext s1 _) as t1) ((DictByTypeVariableFromContext s2 _) as t2) =
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
intersect (DictByTypeVariableFromContext sz1 t1) (DictByTypeVariableFromContext sz2 t2) =
    -- possible optimization: sz1 * sz2 == 0
    if sz1 == 0 || sz2 == 0 then
        empty

    else
        -- Now t1 and t2 are never leaves, so we have an invariant that queues never contain leaves
        intersectFromZipper
            listWithLengthEmpty
            (unconsBiggest [ t1 ])
            (unconsBiggest [ t2 ])
            |> innerFromSortedList


intersectFromZipper :
    ListWithLength ( TypeVariableFromContext, v )
    -> Maybe ( TypeVariableFromContext, v, VisitQueue v )
    -> Maybe ( TypeVariableFromContext, v, VisitQueue v )
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
                        intersectFromZipper dacc (unconsBiggestWhileDroppingGT rkey ltail) rleft

                    else if TypeVariableFromContext.greaterThan rkey lkey then
                        intersectFromZipper dacc lleft (unconsBiggestWhileDroppingGT lkey rtail)

                    else
                        intersectFromZipper (listWithLengthCons ( lkey, lvalue ) dacc) (unconsBiggest ltail) (unconsBiggest rtail)


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : DictByTypeVariableFromContext a -> DictByTypeVariableFromContext b_ -> DictByTypeVariableFromContext a
diff ((DictByTypeVariableFromContext sz1 _) as t1) t2 =
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
        ( leftovers, intermediateResult ) =
            foldl
                (\rKey rValue soFar ->
                    mergeStep leftStep bothStep rightStep rKey rValue soFar
                )
                ( toList leftDict, initialResult )
                rightDict
    in
    List.foldl (\( k, v ) result -> leftStep k v result) intermediateResult leftovers


mergeStep :
    (TypeVariableFromContext -> a -> result -> result)
    -> (TypeVariableFromContext -> a -> b -> result -> result)
    -> (TypeVariableFromContext -> b -> result -> result)
    -> TypeVariableFromContext
    -> b
    -> ( List ( TypeVariableFromContext, a ), result )
    -> ( List ( TypeVariableFromContext, a ), result )
mergeStep leftStep bothStep rightStep rKey rValue ( list, result ) =
    case list of
        [] ->
            ( list, rightStep rKey rValue result )

        ( lKey, lValue ) :: rest ->
            case TypeVariableFromContext.compare lKey rKey of
                LT ->
                    mergeStep leftStep
                        bothStep
                        rightStep
                        rKey
                        rValue
                        ( rest, leftStep lKey lValue result )

                GT ->
                    ( list, rightStep rKey rValue result )

                EQ ->
                    ( rest, bothStep lKey lValue rValue result )



-- TRANSFORM


{-| Apply a function to all values in a dictionary.
-}
map : (TypeVariableFromContext -> a -> b) -> DictByTypeVariableFromContext a -> DictByTypeVariableFromContext b
map func (DictByTypeVariableFromContext sz dict) =
    DictByTypeVariableFromContext sz (mapInner func dict)


mapInner : (TypeVariableFromContext -> a -> b) -> InnerDictByTypeVariableFromContext a -> InnerDictByTypeVariableFromContext b
mapInner func dict =
    -- IGNORE TCO
    case dict of
        Leaf ->
            Leaf

        InnerNode color key value left right ->
            InnerNode color key (func key value) (mapInner func left) (mapInner func right)


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
foldl func acc (DictByTypeVariableFromContext _ dict) =
    foldlInner func acc dict


foldlInner : (TypeVariableFromContext -> v -> b -> b) -> b -> InnerDictByTypeVariableFromContext v -> b
foldlInner func acc dict =
    -- IGNORE TCO
    case dict of
        Leaf ->
            acc

        InnerNode _ key value left right ->
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
foldr func acc (DictByTypeVariableFromContext _ dict) =
    foldrInner func acc dict


foldrInner :
    (TypeVariableFromContext -> v -> b -> b)
    -> b
    -> InnerDictByTypeVariableFromContext v
    -> b
foldrInner func acc t =
    -- IGNORE TCO
    case t of
        Leaf ->
            acc

        InnerNode _ key value left right ->
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


type alias ListWithLength a =
    { length : Int, list : List a }


listWithLengthEmpty : ListWithLength a_
listWithLengthEmpty =
    { length = 0, list = [] }


{-| Prepend an element to the list.

This function is O(1).

-}
listWithLengthCons : a -> ListWithLength a -> ListWithLength a
listWithLengthCons newHead listWithLength =
    { length = listWithLength.length + 1
    , list = newHead :: listWithLength.list
    }
