#include "dbstruct.ch"
#include "error.ch"
#include "box.ch"

#include "functions.ch"

#define BLINKING_PART '\*?'
#define INTENSITY_PART '\+?'
#define COLOR_PART '((RB)|(GR)|(BG)|([UIXWNRGB]))'

PROCEDURE throw(cDescription)

    LOCAL oError := ErrorNew()
    LOCAL n := 0

    oError:description := cDescription
    oError:severity := ES_ERROR
    oError:cargo := 'EXCEPTION: '

    DO WHILE !Empty(ProcName(++n))
        oError:cargo += hb_StrFormat('Called from %1$s(%2$d)' + hb_OsNewLine(), ProcName(n), ProcLine(n))
    ENDDO

BREAK oError //-es2 and -w3 flags makes RETURN impossible here

PROCEDURE assert_type(xValue, xType, cDescription)

    LOCAL nPCount := PCount()

    IF nPCount < 2 .OR. nPCount > 3
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ENDIF

    IF ValType(xType) == 'C'
        xType := {xType}
    ELSEIF ValType(xType) != 'A'
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF

    AEval(xType, {| cElement | IF(is_data_type(cElement), , throw(ARGUMENT_VALUE_EXCEPTION))})

    IF AScan(xType, ValType(xValue)) == 0
        IF nPCount == 3
            assert_type(cDescription, 'C')
            throw(cDescription)
        ELSE
            throw(ARGUMENT_TYPE_EXCEPTION)
        ENDIF
    ENDIF

RETURN 

PROCEDURE assert_length(xValue, nLength, cDescription)

    LOCAL nPCount := PCount()

    IF nPCount < 2 .OR. nPCount > 3
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ELSEIF !(ValType(xValue) $ 'A;H;C')
        throw(ARGUMENT_TYPE_EXCEPTION)
    ELSEIF Len(xValue) != nLength
        IF nPCount == 3
            assert_type(cDescription, 'C')
            throw(cDescription)
        ELSE
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
    ENDIF

RETURN

PROCEDURE assert_data_type(cType, cDescription)

    LOCAL nPCount := PCount()

    IF nPCount < 1 .OR. nPCount > 2
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ELSEIF !is_data_type(cType)
        IF nPCount == 2
            assert_type(cDescription, 'C')
            throw(cDescription)
        ELSE
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
    ENDIF

RETURN

FUNCTION is_data_type(cType)

    IF PCount() != 1
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ELSEIF ValType(cType) != 'C' .OR. Len(cType) != 1
        RETURN .F.
    ENDIF

RETURN cType $ 'A;O;H;L;N;D;C;U;B;M;P;T;S'

FUNCTION is_number_string(cVariable, lAllowSpaces)

    LOCAL lWasDecimalPoint := .F.
    LOCAL lWasMinusSign := .F.
    LOCAL lWasDigit := .F.
    LOCAL cDigit

    IF PCount() != 2 .AND. PCount() != 1 
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ELSEIF PCount() == 2 .AND. ValType(lAllowSpaces) != 'L'
        throw(ARGUMENT_TYPE_EXCEPTION)
    ENDIF

    IF hb_PIsByRef(1)
        throw(PASS_BY_REFERENCE_EXCEPTION)
    ENDIF

    IF ValType(cVariable) != 'C'
        RETURN .F.
    ENDIF

    IF PCount() == 2 .AND. lAllowSpaces
        cVariable := AllTrim(cVariable)
    ENDIF

    FOR EACH cDigit IN cVariable
        IF IsDigit(cDigit)
            lWasDigit := .T.
        ELSE
            IF lWasDecimalPoint .AND. cDigit == '.'
                RETURN .F.
            ELSEIF lWasMinusSign .AND. cDigit == '-'
                RETURN .F.
            ELSEIF !lWasDecimalPoint .AND. cDigit == '.'
                lWasDecimalPoint := .T.
            ELSEIF !lWasMinusSign .AND. cDigit == '-'
                IF lWasDecimalPoint .OR. lWasDigit
                    RETURN .F.    
                ELSE
                    lWasMinusSign := .T.
                ENDIF
            ELSE
                RETURN .F.
            ENDIF
        ENDIF
    NEXT

RETURN .T.

FUNCTION is_date_string(cVariable, lAllowSpaces)

    IF PCount() == 2
        assert_type(lAllowSpaces, 'L')
    ENDIF

    IF ValType(cVariable) != 'C'
        RETURN .F.
    ENDIF

    IF hb_PIsByRef(1)
        throw(PASS_BY_REFERENCE_EXCEPTION)
    ENDIF

    IF PCount() == 2 .AND. lAllowSpaces
        cVariable := AllTrim(cVariable)
    ENDIF

    IF !is_number_string(cVariable)
        RETURN .F.
    ENDIF

    IF Len(cVariable) != 8
        RETURN .F.
    ENDIF

RETURN .T.

FUNCTION is_logical_string(cVariable, lAllowSpaces)

    IF PCount() == 2
        assert_type(lAllowSpaces, 'L')
    ENDIF

    IF ValType(cVariable) != 'C'
        RETURN .F.
    ENDIF

    IF hb_PIsByRef(1)
        throw(PASS_BY_REFERENCE_EXCEPTION)
    ENDIF

    IF PCount() == 2 .AND. lAllowSpaces
        cVariable := AllTrim(cVariable)
    ENDIF

    IF Len(cVariable) == 3 .AND. cVariable == '.T.' .OR. cVariable == '.F.'
        RETURN .T.
    ELSEIF Len(cVariable) == 1 .AND. cVariable $ 'T;F;Y;N'
        RETURN .T.
    ENDIF

RETURN .F.

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

FUNCTION is_color(cColorString, lAllowSpaces, cPattern)

    LOCAL acColors
    LOCAL nSlash
    LOCAL cColor
    LOCAL pRegEx

    IF PCount() != 1 .AND. PCount() != 2 .AND. PCount() != 3
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ENDIF

    IF PCount() > 1
        assert_type(lAllowSpaces, 'L')
    ENDIF

    IF PCount() > 2
        assert_type(cPattern, 'C')
        assert_length(cPattern, 3)
    ENDIF

    IF ValType(cColorString) != 'C'
        RETURN .F.
    ENDIF

    IF hb_PIsByRef(1)
        throw(PASS_BY_REFERENCE_EXCEPTION)
    ENDIF

    IF PCount() >= 2 .AND. lAllowSpaces
        cColorString := AllTrim(cColorString)
    ENDIF

    IF PCount() == 3
        pRegEx := create_color_regexp(cPattern)
    ELSE
        pRegEx := create_color_regexp(REGEXP_COLOR_PART + REGEXP_INTENSITY_PART + REGEXP_BLINKING_PART)
    ENDIF

    acColors := hb_ATokens(cColorString, ',')

    FOR EACH cColor IN acColors
        IF Empty(cColor)
            LOOP
        ENDIF

        nSlash := At('/', cColor)

        IF nSlash == 0
            RETURN .F.
        ENDIF

        IF !hb_RegExLike(pRegEx, Left(cColor, nSlash - 1)) .OR. !hb_RegExLike(pRegEx, SubStr(cColor, nSlash + 1))
            RETURN .F.
        ENDIF
    NEXT

RETURN .T.

STATIC FUNCTION create_color_regexp(cPattern)
    
    LOCAL nLength := Len(cPattern)
    LOCAL cRegEx := ''
    LOCAL cLetter
    LOCAL i

    FOR i := 1 TO nLength
        cLetter := SubStr(cPattern, i, 1)
        DO CASE
            CASE cLetter == REGEXP_COLOR_PART
                cRegEx += COLOR_PART
            CASE cLetter == REGEXP_BLINKING_PART
                cRegEx += BLINKING_PART
            CASE cLetter == REGEXP_INTENSITY_PART
                cRegEx += INTENSITY_PART
            OTHERWISE
                throw(ARGUMENT_VALUE_EXCEPTION)
        ENDCASE
    NEXT

RETURN hb_RegExComp(cRegEx)

FUNCTION is_box(cBoxString)

    IF PCount() != 1
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ENDIF

RETURN ValType(cBoxString) == 'C' //.AND. Len(cBoxString) == STD_BOX_STRING_LENGTH TODO Doesn't work with UTF8 and when Len(cBoxString) == 9

FUNCTION is_style(cStyleString)

    IF PCount() != 1
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ENDIF

RETURN ValType(cStyleString) == 'C' .AND. Len(cStyleString) == STD_STYLE_STRING_LENGTH

FUNCTION is_picture(cPicture)

    IF PCount() != 1
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ENDIF

RETURN ValType(cPicture) == 'C' .AND. is_function_picture(get_function_from_picture(cPicture)) .AND. is_template_picture(get_template_from_picture(cPicture))

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

FUNCTION is_function_picture(cFunction)

    LOCAL lScrolling := .F.
    LOCAL lCheckNextIsDigit := .F.
    LOCAL cCharacter

    IF PCount() != 1
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ENDIF

    IF ValType(cFunction) != 'C'
        RETURN .F.
    ENDIF

    FOR EACH cCharacter IN cFunction

        IF lCheckNextIsDigit
            lCheckNextIsDigit := .F.
            IF !isDigit(cCharacter)
                RETURN .F.
            ENDIF
            CONTINUE
        ELSEIF cCharacter == 'S'
            lScrolling := .T.
            lCheckNextIsDigit := .T.
        ELSEIF lScrolling .AND. isDigit(cCharacter)
            CONTINUE
        ELSEIF cCharacter $ 'A;B;C;D;E;K;R;X;Z(;);!;@'
            lScrolling := .F.
            CONTINUE
        ELSE
            RETURN .F.
        ENDIF
    NEXT

RETURN .T.

FUNCTION is_template_picture(cTemplate)

    LOCAL cCharacter

    IF PCount() != 1
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ENDIF

    IF ValType(cTemplate) != 'C'
        RETURN .F.
    ENDIF

    FOR EACH cCharacter IN cTemplate
        IF cCharacter $ 'A;N;X;9;#;L;Y;!;$;*;.;,'
            CONTINUE
        ELSE
            RETURN .F.
        ENDIF
    NEXT

RETURN .T.

FUNCTION cast(xVar, cTo)

    LOCAL xResult := NIL
    LOCAL cType

    IF PCount() != 2
        throw(ARGUMENTS_NUMBER_EXCEPTION)
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

    DO CASE
        CASE cType == 'C'
            DO CASE
                CASE cTo == 'D'
                    IF is_date_string(xVar)
                        RETURN SToD(xVar)
                    ELSE
                        RETURN NIL
                    ENDIF
                CASE cTo == 'N'
                    IF is_number_string(xVar)
                        RETURN Val(xVar)
                    ELSE
                        RETURN NIL
                    ENDIF
                CASE cTo == 'L'
                    IF is_logical_string(xVar)
                        RETURN IF(xVar == '.T.' .OR. xVar == 'T' .OR. xVar == 'Y', .T., .F.) 
                    ELSE
                        RETURN NIL
                    ENDIF
                CASE cTo == 'M'
                    RETURN xVar
                CASE cTo == 'U'
                    RETURN ''
                CASE cTo == 'B'
                    RETURN NIL
                CASE cTo == 'A'
                    RETURN {xVar}
                CASE cTo == 'H'
                    RETURN {'' => xVar}
            ENDCASE
        CASE cType == 'D'
            DO CASE
                CASE cTo == 'C'
                    RETURN DToC(xVar)
                CASE cTo == 'N'
                    RETURN NIL
                CASE cTo == 'L'
                    RETURN NIL
                CASE cTo == 'M'
                    RETURN DToS(xVar)
                CASE cTo == 'U'
                    RETURN d"0000-00-00"
                CASE cTo == 'B'
                    RETURN NIL
                CASE cTo == 'A'
                    RETURN {xVar}
                CASE cTo == 'H'
                    RETURN {'' => xVar}
            ENDCASE
        CASE cType == 'N'
            DO CASE
                CASE cTo == 'C'
                    RETURN Str(xVar)
                CASE cTo == 'D'
                    RETURN NIL
                CASE cTo == 'L'
                    IF xVar == 0
                        RETURN .F.
                    ELSE
                        RETURN .T.
                    ENDIF
                CASE cTo == 'M'
                    RETURN Str(xVar)
                CASE cTo == 'U'
                    RETURN 0
                CASE cTo == 'B'
                    RETURN NIL
                CASE cTo == 'A'
                    RETURN {xVar}
                CASE cTo == 'H'
                    RETURN {'' => xVar}
            ENDCASE
        CASE cType == 'L'
            DO CASE
                CASE cTo == 'C'
                    RETURN Transform(xVar, 'L')
                CASE cTo == 'D'
                    RETURN NIL
                CASE cTo == 'N'
                    IF xVar
                        RETURN 1
                    ELSE
                        RETURN 0
                    ENDIF
                CASE cTo == 'M'
                    RETURN Transform(xVar, 'Y')
                CASE cTo == 'U'
                    RETURN .F.
                CASE cTo == 'B'
                    RETURN NIL
                CASE cTo == 'A'
                    RETURN {xVar}
                CASE cTo == 'H'
                    RETURN {'' => xVar}
            ENDCASE
        CASE cType == 'M'
            DO CASE
                CASE cTo == 'C'
                    RETURN xVar
                CASE cTo == 'D'
                    IF is_date_string(xVar)
                        RETURN SToD(xVar)
                    ELSE
                        RETURN NIL
                    ENDIF
                CASE cTo == 'N'
                    IF is_number_string(xVar)
                        RETURN Val(xVar)
                    ELSE
                        RETURN NIL
                    ENDIF
                CASE cTo == 'L'
                    IF is_logical_string(xVar)
                        RETURN IF(xVar == '.T.' .OR. xVar == 'T' .OR. xVar == 'Y', .T., .F.) 
                    ELSE
                        RETURN NIL
                    ENDIF
                CASE cTo == 'U'
                    RETURN ''
                CASE cTo == 'B'
                    RETURN NIL
                CASE cTo == 'A'
                    RETURN {xVar}
                CASE cTo == 'H'
                    RETURN {'' => xVar}
            ENDCASE
        CASE cType == 'U'
            DO CASE
                CASE cTo == 'C'
                    RETURN ''
                CASE cTo == 'D'
                    RETURN d"0000-00-00"
                CASE cTo == 'N'
                    RETURN 0
                CASE cTo == 'L'
                    RETURN .F.
                CASE cTo == 'M'
                    RETURN ''
                CASE cTo == 'B'
                    RETURN {|| nothing()}
                CASE cTo == 'A'
                    RETURN {}
                CASE cTo == 'H'
                    RETURN {=>}
            ENDCASE
    ENDCASE

RETURN xResult

FUNCTION true()
RETURN .T.

FUNCTION false()
RETURN .F.

FUNCTION nothing()
RETURN NIL

FUNCTION row_to_hash(acOmmit, hAdd, nRecNo, cAlias)

    LOCAL nOldSelect := Select()
    LOCAL nOldRecNo := RecNo()
    LOCAL hHash := hb_Hash()
    LOCAL axStructure
    LOCAL cKey
    LOCAL axRow

    IF Alias(Select(nOldSelect)) == ''
        throw(RUNTIME_EXCEPTION)
    ELSEIF PCount() > 4
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ELSEIF !(ValType(acOmmit) $ 'A;U')
        throw(ARGUMENT_TYPE_EXCEPTION)
    ELSEIF !(ValType(hAdd) $ 'H;U')
        throw(ARGUMENT_TYPE_EXCEPTION)
    ELSEIF !(ValType(cAlias) $ 'C;U')
        throw(ARGUMENT_TYPE_EXCEPTION)
    ELSEIF !(ValType(nRecNo) $ 'N;U')
        throw(ARGUMENT_TYPE_EXCEPTION)
    ELSEIF ValType(acOmmit) == 'A'
        AEval(acOmmit, {| cElement | assert_type(cElement, 'C')})
    ELSEIF ValType(hAdd) == 'H'
        FOR EACH cKey IN hb_hKeys(hAdd)
            assert_type(cKey, 'C')
            IF !(ValType(hAdd[cKey]) $ 'C;N;D;L')
                throw(ARGUMENT_TYPE_EXCEPTION)
            ENDIF
        NEXT
    ENDIF

    IF ValType(cAlias) == 'C'
        SELECT (cAlias)
    ENDIF

    IF ValType(nRecNo) == 'N'
        GO nRecNo
    ENDIF

    axStructure := dbStruct()

    IF ValType(acOmmit) == 'A'
        FOR EACH axRow IN axStructure
            IF AScan(acOmmit, axRow[DBS_NAME]) == 0
                hHash[axRow[DBS_NAME]] := field->&(axRow[DBS_NAME])
            ENDIF
        NEXT
    ELSE
        FOR EACH axRow IN axStructure
            hHash[axRow[DBS_NAME]] := field->&(axRow[DBS_NAME])
        NEXT
    ENDIF

    IF ValType(hAdd) == 'H'
        FOR EACH cKey IN hb_hKeys(hAdd)
            IF hb_hHasKey(hHash, cKey)
                throw(RUNTIME_EXCEPTION)
            ENDIF

            hHash[cKey] := hAdd[cKey]
        NEXT
    ENDIF

    SELECT (nOldSelect)
    GO nOldRecNo

RETURN hHash

FUNCTION hash_to_row(hHash, lAppendBlank, lSubset)

    LOCAL axStructure := dbStruct()
    LOCAL axField

    IF PCount() != 2 .AND. PCount() != 3
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ELSEIF ValType(hHash) != 'H'
        throw(ARGUMENT_TYPE_EXCEPTION)
    ELSEIF ValType(lAppendBlank) != 'L'
        throw(ARGUMENT_TYPE_EXCEPTION)
    ELSEIF !(ValType(lSubset) $ 'L;U')
        throw(ARGUMENT_TYPE_EXCEPTION)
    ENDIF

    IF ValType(lSubset) == 'U'
        lSubset := .F.
    ENDIF

    IF !lSubset
        FOR EACH axField IN axStructure
            IF !hb_hHasKey(hHash, axField[DBS_NAME])
                throw(RUNTIME_EXCEPTION)
            ENDIF
        NEXT
    ENDIF

    IF lAppendBlank
        dbAppend(.F.)

        IF NetErr()
            RETURN .F.
        ENDIF
    ELSE
        IF !dbRLock(RecNo())
            RETURN .F.
        ENDIF
    ENDIF

    FOR EACH axField IN axStructure
        field->&(axField[DBS_NAME]) := hHash[axField[DBS_NAME]]
    NEXT

RETURN .T.

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
