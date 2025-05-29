module TypeVariableFromContext exposing (TypeVariableFromContext, compare, equals, greaterThan, lessThan, rangeCompare, rangeEquals, rangeGreaterThan, rangeLessThan)

import Elm.Syntax.Range


type alias TypeVariableFromContext =
    ( -- combined Range from all uses
      Elm.Syntax.Range.Range
    , String
    )


equals : TypeVariableFromContext -> TypeVariableFromContext -> Bool
equals ( aRange, aName ) ( bRange, bName ) =
    (aName == bName)
        && rangeEquals aRange bRange


rangeEquals : Elm.Syntax.Range.Range -> Elm.Syntax.Range.Range -> Bool
rangeEquals a b =
    locationEquals a.start b.start
        && locationEquals a.end b.end


locationEquals : Elm.Syntax.Range.Location -> Elm.Syntax.Range.Location -> Bool
locationEquals a b =
    (a.row - b.row == 0)
        && (a.column - b.column == 0)


rangeCompare : Elm.Syntax.Range.Range -> Elm.Syntax.Range.Range -> Order
rangeCompare a b =
    if a.start.row - b.start.row < 0 then
        LT

    else if a.start.row - b.start.row > 0 then
        GT

    else if a.start.column - b.start.column < 0 then
        LT

    else if a.start.column - b.start.column > 0 then
        GT

    else
        locationCompare a.end b.end


locationCompare : Elm.Syntax.Range.Location -> Elm.Syntax.Range.Location -> Order
locationCompare a b =
    if a.row - b.row < 0 then
        LT

    else if a.row - b.row > 0 then
        GT

    else if a.column - b.column < 0 then
        LT

    else if a.column - b.column > 0 then
        GT

    else
        EQ


compare : TypeVariableFromContext -> TypeVariableFromContext -> Order
compare ( aRange, aName ) ( bRange, bName ) =
    if rangeEquals aRange bRange then
        Basics.compare aName bName

    else
        rangeCompare aRange bRange


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


rangeLessThan : Elm.Syntax.Range.Range -> Elm.Syntax.Range.Range -> Bool
rangeLessThan aRange bRange =
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


rangeGreaterThan : Elm.Syntax.Range.Range -> Elm.Syntax.Range.Range -> Bool
rangeGreaterThan aRange bRange =
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
        locationGreaterThen aRange.end bRange.end


locationGreaterThen : Elm.Syntax.Range.Location -> Elm.Syntax.Range.Location -> Bool
locationGreaterThen a b =
    (a.row - b.row > 0)
        || ((a.row - b.row == 0)
                && (a.column - b.column > 0)
           )
