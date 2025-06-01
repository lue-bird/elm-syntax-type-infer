module TypeVariableFromContext exposing (TypeVariableFromContext, compare, equals, greaterThan, lessThan)

import Elm.Syntax.Range


type alias TypeVariableFromContext =
    ( -- combined Range from all uses
      Elm.Syntax.Range.Range
    , String
    )


equals : TypeVariableFromContext -> TypeVariableFromContext -> Bool
equals ( aRange, aName ) ( bRange, bName ) =
    rangeEquals aRange bRange
        && (aName == bName)


rangeEquals : Elm.Syntax.Range.Range -> Elm.Syntax.Range.Range -> Bool
rangeEquals a b =
    locationEquals a.start b.start
        && locationEquals a.end b.end


locationEquals : Elm.Syntax.Range.Location -> Elm.Syntax.Range.Location -> Bool
locationEquals a b =
    (a.row - b.row == 0)
        && (a.column - b.column == 0)


compare : TypeVariableFromContext -> TypeVariableFromContext -> Order
compare ( aRange, aName ) ( bRange, bName ) =
    if aRange.start.row - bRange.start.row < 0 then
        LT

    else if aRange.start.row - bRange.start.row > 0 then
        GT

    else if aRange.start.column - bRange.start.column < 0 then
        LT

    else if aRange.start.column - bRange.start.column > 0 then
        GT

    else
    -- aRange.start == bRange.start
    if
        aRange.end.row - bRange.end.row < 0
    then
        LT

    else if aRange.end.row - bRange.end.row > 0 then
        GT

    else if aRange.end.column - bRange.end.column < 0 then
        LT

    else if aRange.end.column - bRange.end.column > 0 then
        GT

    else
        -- aRange == bRange
        Basics.compare aName bName


lessThan : TypeVariableFromContext -> TypeVariableFromContext -> Bool
lessThan ( aRange, aName ) ( bRange, bName ) =
    if aRange.start.row - bRange.start.row < 0 then
        True

    else if aRange.start.row - bRange.start.row > 0 then
        False

    else
    -- a.start.row == b.start.row
    if
        aRange.start.column - bRange.start.column < 0
    then
        True

    else if aRange.start.column - bRange.start.column > 0 then
        False

    else
    -- b.start == b.end
    if
        locationEquals aRange.end bRange.end
    then
        aName < bName

    else
        locationLessThen aRange.end bRange.end


locationLessThen : Elm.Syntax.Range.Location -> Elm.Syntax.Range.Location -> Bool
locationLessThen a b =
    (a.row - b.row < 0)
        || ((a.row - b.row == 0)
                && (a.column - b.column < 0)
           )


greaterThan : TypeVariableFromContext -> TypeVariableFromContext -> Bool
greaterThan ( aRange, aName ) ( bRange, bName ) =
    if aRange.start.row - bRange.start.row > 0 then
        True

    else if aRange.start.row - bRange.start.row < 0 then
        False

    else
    -- a.start.row == b.start.row
    if
        aRange.start.column - bRange.start.column > 0
    then
        True

    else if aRange.start.column - bRange.start.column < 0 then
        False

    else
    -- a.start == b.start
    if
        locationEquals aRange.end bRange.end
    then
        aName > bName

    else
        locationGreaterThen aRange.end bRange.end


locationGreaterThen : Elm.Syntax.Range.Location -> Elm.Syntax.Range.Location -> Bool
locationGreaterThen a b =
    (a.row - b.row > 0)
        || ((a.row - b.row == 0)
                && (a.column - b.column > 0)
           )
