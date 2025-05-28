module TypeVariableFromContext exposing (TypeVariableFromContext, compare, equals, greaterThan, lessThan)


type alias TypeVariableFromContext =
    ( -- combined Range from all uses
      RangeAsComparable
    , String
    )


type alias RangeAsComparable =
    ( -- start
      LocationAsComparable
    , -- end
      LocationAsComparable
    )


type alias LocationAsComparable =
    ( -- row
      Int
    , -- column
      Int
    )


equals : TypeVariableFromContext -> TypeVariableFromContext -> Bool
equals ( aRangeAsComparable, aName ) ( bRangeAsComparable, bName ) =
    (aName == bName)
        && rangeAsComparableEquals aRangeAsComparable bRangeAsComparable


rangeAsComparableEquals : RangeAsComparable -> RangeAsComparable -> Bool
rangeAsComparableEquals ( aStart, aEnd ) ( bStart, bEnd ) =
    locationAsComparableEquals aStart bStart
        && locationAsComparableEquals aEnd bEnd


locationAsComparableEquals : LocationAsComparable -> LocationAsComparable -> Bool
locationAsComparableEquals ( aRow, aColumn ) ( bRow, bColumn ) =
    (aRow - bRow == 0)
        && (aColumn - bColumn == 0)


rangeAsComparableCompare : RangeAsComparable -> RangeAsComparable -> Order
rangeAsComparableCompare ( aStart, aEnd ) ( bStart, bEnd ) =
    let
        ( aStartRow, aStartColumn ) =
            aStart

        ( bStartRow, bStartColumn ) =
            bStart
    in
    if aStartRow - bStartRow < 0 then
        LT

    else if aStartRow - bStartRow > 0 then
        GT

    else if aStartColumn - bStartColumn < 0 then
        LT

    else if aStartColumn - bStartColumn > 0 then
        GT

    else
        locationAsComparableCompare aEnd bEnd


locationAsComparableCompare : LocationAsComparable -> LocationAsComparable -> Order
locationAsComparableCompare ( aRow, aColumn ) ( bRow, bColumn ) =
    if aRow - bRow < 0 then
        LT

    else if aRow - bRow > 0 then
        GT

    else if aColumn - bColumn < 0 then
        LT

    else if aColumn - bColumn > 0 then
        GT

    else
        EQ


compare : TypeVariableFromContext -> TypeVariableFromContext -> Order
compare ( aRangeAsComparable, aName ) ( bRangeAsComparable, bName ) =
    if rangeAsComparableEquals aRangeAsComparable bRangeAsComparable then
        Basics.compare aName bName

    else
        rangeAsComparableCompare aRangeAsComparable bRangeAsComparable


lessThan : TypeVariableFromContext -> TypeVariableFromContext -> Bool
lessThan ( ( aStart, aEnd ), aName ) ( ( bStart, bEnd ), bName ) =
    let
        ( aStartRow, aStartColumn ) =
            aStart

        ( bStartRow, bStartColumn ) =
            bStart
    in
    if aStartRow - bStartRow < 0 then
        True

    else if aStartRow - bStartRow > 0 then
        False

    else
    -- aStartRow == bStartRow
    if
        aStartColumn - bStartColumn < 0
    then
        True

    else if aStartColumn - bStartColumn > 0 then
        False

    else
    -- bStart == bEnd
    if
        locationAsComparableEquals aEnd bEnd
    then
        aName < bName

    else
        locationAsComparableLessThen aEnd bEnd


locationAsComparableLessThen : LocationAsComparable -> LocationAsComparable -> Bool
locationAsComparableLessThen ( aRow, aColumn ) ( bRow, bColumn ) =
    (aRow - bRow < 0)
        || ((aRow - bRow == 0)
                && (aColumn - bColumn < 0)
           )


greaterThan : TypeVariableFromContext -> TypeVariableFromContext -> Bool
greaterThan ( ( aStart, aEnd ), aName ) ( ( bStart, bEnd ), bName ) =
    let
        ( aStartRow, aStartColumn ) =
            aStart

        ( bStartRow, bStartColumn ) =
            bStart
    in
    if aStartRow - bStartRow > 0 then
        True

    else if aStartRow - bStartRow < 0 then
        False

    else
    -- aStartRow == bStartRow
    if
        aStartColumn - bStartColumn > 0
    then
        True

    else if aStartColumn - bStartColumn < 0 then
        False

    else
    -- aStart == bStart
    if
        locationAsComparableEquals aEnd bEnd
    then
        aName > bName

    else
        locationAsComparableGreaterThen aEnd bEnd


locationAsComparableGreaterThen : LocationAsComparable -> LocationAsComparable -> Bool
locationAsComparableGreaterThen ( aRow, aColumn ) ( bRow, bColumn ) =
    (aRow - bRow > 0)
        || ((aRow - bRow == 0)
                && (aColumn - bColumn > 0)
           )
