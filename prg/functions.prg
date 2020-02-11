#include "error.ch"

#include "functions.ch"

FUNCTION max_of_array(axArray)

    LOCAL cType
    LOCAL xMax
    LOCAL nLength
    LOCAL i

    assert_type(axArray, 'A')

    nLength := Len(axArray)

    IF nLength > 0
        xMax := axArray[1]
        cType := ValType(axArray[1])
    ELSE
        throw(RUNTIME_EXCEPTION)
    ENDIF

    FOR i := 2 TO nLength
        IF ValType(axArray[i]) != cType
            throw(RUNTIME_EXCEPTION)
        ENDIF
        xMax := Max(axArray[i], xMax)
    NEXT

RETURN xMax

FUNCTION min_of_array(axArray)

    LOCAL cType
    LOCAL xMin
    LOCAL nLength
    LOCAL i

    assert_type(axArray, 'A')

    nLength := Len(axArray)

    IF nLength > 0
        xMin := axArray[1]
        cType := ValType(axArray[1])
    ELSE
        throw(RUNTIME_EXCEPTION)
    ENDIF

    FOR i := 2 TO nLength
        IF ValType(axArray[i]) != cType
            throw(RUNTIME_EXCEPTION)
        ENDIF
        xMin := Min(axArray[i], xMin)
    NEXT

RETURN xMin

FUNCTION max_of_array_index(axArray)

    LOCAL nMaxInd := 1
    LOCAL cType
    LOCAL nLength
    LOCAL i

    assert_type(axArray, 'A')

    nLength := Len(axArray)

    IF nLength == 0
        RETURN nLength
    ENDIF

    cType := ValType(axArray[nMaxInd])
    FOR i := 2 TO nLength
        IF ValType(axArray[i]) != cType
            throw(RUNTIME_EXCEPTION)
        ENDIF

        IF axArray[i] > axArray[nMaxInd]
            nMaxInd := i
        ENDIF
    NEXT

RETURN nMaxInd

FUNCTION min_of_array_index(axArray)

    LOCAL nMinInd := 1
    LOCAL cType
    LOCAL nLength
    LOCAL i

    assert_type(axArray, 'A')

    nLength := Len(axArray)

    IF nLength == 0
        RETURN nLength
    ENDIF

    cType := ValType(axArray[nMinInd])
    FOR i := 2 TO nLength
        IF ValType(axArray[i]) != cType
            throw(RUNTIME_EXCEPTION)
        ENDIF

        IF axArray[i] > axArray[nMinInd]
            nMinInd := i
        ENDIF
    NEXT

RETURN nMinInd

FUNCTION length_array(axArray)

    LOCAL anArrayOfLengths
    LOCAL nLength
    LOCAL i

    assert_type(axArray, 'A')

    nLength := Len(axArray)
    anArrayOfLengths := Array(nLength)

    FOR i := 1 TO nLength
        IF ValType(axArray[i]) $ 'C;A;H'
            anArrayOfLengths[i] := Len(axArray[i])
        ELSE
            throw(RUNTIME_EXCEPTION)
        ENDIF
    NEXT

RETURN anArrayOfLengths

FUNCTION clone_objects_array(aoArray)

    LOCAL aoNewArray
    LOCAL i

    assert_type(aoArray, 'A')
    aoNewArray := Array(Len(aoArray))

    FOR i := 1 TO Len(aoArray)
        IF ValType(aoArray[i]) != 'O'
            throw(RUNTIME_EXCEPTION)
        ENDIF
        aoNewArray[i] := __objClone(aoArray[i])
    NEXT

RETURN aoNewArray

FUNCTION get_function_from_picture(cPicture)

    LOCAL cFunction := ''
    LOCAL cCharacter

    IF PCount() != 1
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ENDIF

    assert_type(cPicture, 'C')

    IF Left(cPicture, 1) == '@'

        FOR EACH cCharacter IN cPicture
            IF cCharacter == ' '
                EXIT
            ENDIF

            cFunction += cCharacter
        NEXT
    ENDIF

RETURN cFunction

FUNCTION get_template_from_picture(cPicture)

    LOCAL cTemplate := ''
    LOCAL i := 1

    IF PCount() != 1
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ENDIF

    assert_type(cPicture, 'C')

    IF Empty(cPicture)
        RETURN cTemplate
    ELSEIF Left(cPicture, 1) == '@'
        i := At(' ', cPicture) + 1
        IF i == 1
            RETURN cTemplate
        ENDIF
    ENDIF

    DO WHILE i != Len(cPicture)
        cTemplate += SubStr(cPicture, i, 1)
        ++i
    ENDDO

RETURN cTemplate

FUNCTION cast(xVar, cTo, lAllowSpaces)

    LOCAL xResult := NIL
    LOCAL cType

    IF PCount() < 2 .OR. PCount() > 3
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ENDIF

    IF ValType(lAllowSpaces) == 'U'
        lAllowSpaces := .T.
    ELSE
        assert_type(lAllowSpaces, 'L')
    ENDIF

    assert_data_type(cTo)

    cType := ValType(xVar)

    IF cType == cTo
        RETURN xVar
    ELSEIF cTo == 'U' 
        RETURN NIL
    ELSEIF cTo == 'O' .OR. cType == 'O'
        RETURN NIL
    ELSEIF cType $ 'A;O;H;P;T;S'
        RETURN NIL
    ENDIF

    SWITCH cType
        CASE 'C'
            SWITCH cTo
                CASE 'C'
                    RETURN xVar
                CASE 'D'
                    IF is_date_string(xVar, lAllowSpaces)
                        RETURN SToD(xVar)
                    ELSE
                        RETURN NIL
                    ENDIF
                CASE 'N'
                    IF is_number_string(xVar, lAllowSpaces)
                        RETURN Val(xVar)
                    ELSE
                        RETURN NIL
                    ENDIF
                CASE 'L'
                    IF is_logical_string(xVar, lAllowSpaces)
                        RETURN IF(xVar == '.T.' .OR. xVar == 'T' .OR. xVar == 'Y', .T., .F.) 
                    ELSE
                        RETURN NIL
                    ENDIF
                CASE 'M'
                    RETURN xVar
                CASE 'U'
                    RETURN ''
                CASE 'B'
                    RETURN NIL
                CASE 'A'
                    RETURN {xVar}
                CASE 'H'
                    RETURN {'' => xVar}
            ENDSWITCH
        CASE 'D'
            SWITCH cTo
                CASE 'C'
                    RETURN DToC(xVar)
                CASE 'N'
                    RETURN NIL
                CASE 'L'
                    RETURN NIL
                CASE 'M'
                    RETURN DToS(xVar)
                CASE 'U'
                    RETURN d"0000-00-00"
                CASE 'B'
                    RETURN NIL
                CASE 'A'
                    RETURN {xVar}
                CASE 'H'
                    RETURN {'' => xVar}
            ENDSWITCH
        CASE 'N'
            SWITCH cTo
                CASE 'C'
                    RETURN Str(xVar)
                CASE 'D'
                    RETURN NIL
                CASE 'L'
                    IF xVar == 0
                        RETURN .F.
                    ELSE
                        RETURN .T.
                    ENDIF
                CASE 'M'
                    RETURN Str(xVar)
                CASE 'U'
                    RETURN 0
                CASE 'B'
                    RETURN NIL
                CASE 'A'
                    RETURN {xVar}
                CASE 'H'
                    RETURN {'' => xVar}
            ENDSWITCH
        CASE 'L'
            SWITCH cTo
                CASE 'C'
                    RETURN Transform(xVar, 'L')
                CASE 'D'
                    RETURN NIL
                CASE 'N'
                    IF xVar
                        RETURN 1
                    ELSE
                        RETURN 0
                    ENDIF
                CASE 'M'
                    RETURN Transform(xVar, 'Y')
                CASE 'U'
                    RETURN .F.
                CASE 'B'
                    RETURN NIL
                CASE 'A'
                    RETURN {xVar}
                CASE 'H'
                    RETURN {'' => xVar}
            ENDSWITCH
        CASE 'M'
            SWITCH cTo
                CASE 'C'
                    RETURN xVar
                CASE 'D'
                    IF is_date_string(xVar, lAllowSpaces)
                        RETURN SToD(xVar)
                    ELSE
                        RETURN NIL
                    ENDIF
                CASE 'N'
                    IF is_number_string(xVar, lAllowSpaces)
                        RETURN Val(xVar)
                    ELSE
                        RETURN NIL
                    ENDIF
                CASE 'L'
                    IF is_logical_string(xVar, lAllowSpaces)
                        RETURN IF(xVar == '.T.' .OR. xVar == 'T' .OR. xVar == 'Y', .T., .F.) 
                    ELSE
                        RETURN NIL
                    ENDIF
                CASE 'U'
                    RETURN ''
                CASE 'B'
                    RETURN NIL
                CASE 'A'
                    RETURN {xVar}
                CASE 'H'
                    RETURN {'' => xVar}
            ENDSWITCH
        CASE 'U'
            SWITCH cTo
                CASE 'C'
                    RETURN ''
                CASE 'D'
                    RETURN d"0000-00-00"
                CASE 'N'
                    RETURN 0
                CASE 'L'
                    RETURN .F.
                CASE 'M'
                    RETURN ''
                CASE 'B'
                    RETURN {|| nothing()}
                CASE 'A'
                    RETURN {}
                CASE 'H'
                    RETURN {=>}
            ENDSWITCH
    ENDSWITCH

RETURN xResult

FUNCTION true()
RETURN .T.

FUNCTION false()
RETURN .F.

FUNCTION nothing()
RETURN NIL

FUNCTION array_equals(axFirst, axSecond)

    LOCAL nLength
    LOCAL i

    IF PCount() != 2
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ELSEIF ValType(axFirst) != 'A' .OR. ValType(axSecond) != 'A'
        throw(ARGUMENT_TYPE_EXCEPTION)
    ENDIF

    nLength := Len(axFirst)

    IF Len(axFirst) != Len(axSecond)
        RETURN .F.
    ENDIF

    FOR i := 1 TO nLength
        DO CASE
            CASE ValType(axFirst[i]) != ValType(axSecond[i])
                RETURN .F.
            CASE ValType(axFirst[i]) == 'A' .AND. !array_equals(axFirst[i], axSecond[i])
                RETURN .F.
            CASE ValType(axFirst[i]) == 'H' .AND. !hash_equals(axFirst[i], axSecond[i])
                RETURN .F.
            CASE ValType(axFirst[i]) == 'O' .AND. !objects_have_same_messages(axFirst[i], axSecond[i])
                RETURN .F.
            CASE axFirst[i] != axSecond[i]
                RETURN .F.
        ENDCASE
    NEXT

RETURN .T.

FUNCTION hash_equals(hFirst, hSecond)

    LOCAL axKeys
    LOCAL xKey

    IF PCount() != 2
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ELSEIF ValType(hFirst) != 'H' .OR. ValType(hSecond) != 'H'
        throw(ARGUMENT_TYPE_EXCEPTION)
    ENDIF

    axKeys := hb_hKeys(hFirst)

    IF Len(hFirst) != Len(hSecond)
        RETURN .F.
    ENDIF

    FOR EACH xKey IN axKeys
        DO CASE
            CASE !hb_hHasKey(hSecond, xKey)
                RETURN .F.
            CASE ValType(hFirst[xKey]) == 'A' .AND. !array_equals(hFirst[xKey], hSecond[xKey])
                RETURN .F.
            CASE ValType(hFirst[xKey]) == 'H' .AND. !hash_equals(hFirst[xKey], hSecond[xKey])
                RETURN .F.
            CASE ValType(hFirst[xKey]) == 'O' .AND. !objects_have_same_messages(hFirst[xKey], hSecond[xKey])
                RETURN .F.
            CASE hFirst[xKey] != hSecond[xKey]
                RETURN .F.
        ENDCASE
    NEXT

RETURN .T.

FUNCTION objects_have_same_messages(oFirst, oSecond)

    IF PCount() != 2
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ELSEIF ValType(oFirst) != 'O' .OR. ValType(oSecond) != 'O'
        throw(ARGUMENT_TYPE_EXCEPTION)
    ENDIF

RETURN array_equals(__objGetMsgList(oFirst), __objGetMsgList(oSecond))

FUNCTION AMerge(axTarget, axAdd, lDistinct)

    LOCAL xItem
    LOCAL nLength

    assert_type(axTarget, 'A')
    assert_type(axAdd, 'A')

    IF ValType(lDistinct) == 'U'
        lDistinct := .F.
    ELSEIF ValType(lDistinct) != 'L'
        throw(ARGUMENT_TYPE_EXCEPTION)
    ENDIF

    IF PCount() > 3 .OR. PCount() < 2
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ENDIF

    IF lDistinct
        FOR EACH xItem IN axAdd
            IF hb_AScan(axTarget, xItem, , , .T.) == 0
                AAdd(axTarget, xItem)
            ENDIF
        NEXT
    ELSE
        nLength := Len(axTarget)
        ASize(axTarget, nLength + Len(axAdd))
        FOR EACH xItem IN axAdd
            axTarget[++nLength] := xItem
        NEXT
    ENDIF

RETURN axTarget
