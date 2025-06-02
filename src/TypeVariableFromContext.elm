module TypeVariableFromContext exposing (TypeVariableFromContext, compare, equals, greaterThan, lessThan)

import Elm.Syntax.Range


type alias TypeVariableFromContext =
    { useRange : Elm.Syntax.Range.Range
    , name : String
    }


equals : TypeVariableFromContext -> TypeVariableFromContext -> Bool
equals a b =
    rangeEquals a.useRange b.useRange
        && (a.name == b.name)


rangeEquals : Elm.Syntax.Range.Range -> Elm.Syntax.Range.Range -> Bool
rangeEquals a b =
    locationEquals a.start b.start
        && locationEquals a.end b.end


locationEquals : Elm.Syntax.Range.Location -> Elm.Syntax.Range.Location -> Bool
locationEquals a b =
    (a.row - b.row == 0)
        && (a.column - b.column == 0)


compare : TypeVariableFromContext -> TypeVariableFromContext -> Order
compare a b =
    if a.useRange.start.row - b.useRange.start.row < 0 then
        LT

    else if a.useRange.start.row - b.useRange.start.row > 0 then
        GT

    else if a.useRange.start.column - b.useRange.start.column < 0 then
        LT

    else if a.useRange.start.column - b.useRange.start.column > 0 then
        GT

    else
    -- a.useRange.start == b.useRange.start
    if
        a.useRange.end.row - b.useRange.end.row < 0
    then
        LT

    else if a.useRange.end.row - b.useRange.end.row > 0 then
        GT

    else if a.useRange.end.column - b.useRange.end.column < 0 then
        LT

    else if a.useRange.end.column - b.useRange.end.column > 0 then
        GT

    else
        -- a.useRange == b.useRange
        Basics.compare a.name b.name


lessThan : TypeVariableFromContext -> TypeVariableFromContext -> Bool
lessThan a b =
    if a.useRange.start.row - b.useRange.start.row < 0 then
        True

    else if a.useRange.start.row - b.useRange.start.row > 0 then
        False

    else
    -- a.start.row == b.start.row
    if
        a.useRange.start.column - b.useRange.start.column < 0
    then
        True

    else if a.useRange.start.column - b.useRange.start.column > 0 then
        False

    else
    -- b.start == b.end
    if
        locationEquals a.useRange.end b.useRange.end
    then
        a.name < b.name

    else
        locationLessThen a.useRange.end b.useRange.end


locationLessThen : Elm.Syntax.Range.Location -> Elm.Syntax.Range.Location -> Bool
locationLessThen a b =
    (a.row - b.row < 0)
        || ((a.row - b.row == 0)
                && (a.column - b.column < 0)
           )


greaterThan : TypeVariableFromContext -> TypeVariableFromContext -> Bool
greaterThan a b =
    if a.useRange.start.row - b.useRange.start.row > 0 then
        True

    else if a.useRange.start.row - b.useRange.start.row < 0 then
        False

    else
    -- a.start.row == b.start.row
    if
        a.useRange.start.column - b.useRange.start.column > 0
    then
        True

    else if a.useRange.start.column - b.useRange.start.column < 0 then
        False

    else
    -- a.start == b.start
    if
        locationEquals a.useRange.end b.useRange.end
    then
        a.name > b.name

    else
        locationGreaterThen a.useRange.end b.useRange.end


locationGreaterThen : Elm.Syntax.Range.Location -> Elm.Syntax.Range.Location -> Bool
locationGreaterThen a b =
    (a.row - b.row > 0)
        || ((a.row - b.row == 0)
                && (a.column - b.column > 0)
           )
