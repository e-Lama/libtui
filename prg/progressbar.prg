#include "hbclass.ch"

#include "functions.ch"

#include "progressbar.ch"

#define AN_ALL_POSITIONS {N_POSITION_NONE, N_POSITION_RIGHT, N_POSITION_CENTER,;
                          N_POSITION_LEFT, N_POSITION_BOTTOM, N_POSITION_TOP, N_POSITION_CHASE;
                         }

CREATE CLASS Element STATIC

EXPORTED:

    METHOD new(cPattern, cColor) CONSTRUCTOR
    METHOD print(nRow, nCol)
    
    VAR cColor AS CHARACTER INIT Config():get_config('DefaultColor')
    VAR cPattern AS CHARACTER

ENDCLASS LOCK

METHOD new(cPattern, cColor) CLASS Element

    ::cPattern := cPattern

    IF cColor != NIL
        ::cColor := cColor
    ENDIF

RETURN Self

METHOD print(nRow, nCol) CLASS Element
    @ nRow, nCol SAY ::cPattern COLOR ::cColor
RETURN NIL

CREATE CLASS Progress_bar

EXPORTED:

    METHOD new(nRow, nLeft, nRight, nActualValue, nTargetValue, nDisplayPosition, nFormat, nAccuracy, lReverse, cDescriptionColor, cSlash, acPatterns, acColors) CONSTRUCTOR

    METHOD increment(nHowMany)
    METHOD decrement(nHowMany)
    METHOD display()
    METHOD actual_value(nActualValue) SETGET
    METHOD target_value(nTargetValue) SETGET
    METHOD reverse(lReverse) SETGET
    METHOD description_color(cDescriptionColor) SETGET
    METHOD slash(cSlash) SETGET
    METHOD pattern(nIndex, cPattern) SETGET
    METHOD color(nIndex, cColor) SETGET

HIDDEN:

    VAR nRow AS NUMERIC
    VAR nLeft AS NUMERIC
    VAR nRight AS NUMERIC
    VAR nActualValue AS NUMERIC INIT 0
    VAR nTargetValue AS NUMERIC INIT 100
    VAR nDisplayPosition AS NUMERIC INIT N_POSITION_RIGHT
    VAR nFormat AS NUMERIC INIT N_FORMAT_PERCENT
    VAR lReverse AS LOGICAL INIT .F.
    VAR cDescriptionColor AS CHARACTER INIT Config():get_config('DefaultColor')
    VAR nAccuracy AS NUMERIC INIT 0
    VAR cSlash AS CHARACTER INIT '/'
    VAR aoElements AS ARRAY INIT {Element():new(Config():get_config('ProgressBarFirstCharacter'));
                                  , Element():new(Config():get_config('ProgressBarFilledCharacter'));
                                  , Element():new(Config():get_config('ProgressBarEmptyCharacter'));
                                  , Element():new(Config():get_config('ProgressBarLastCharacter'));
                                  , Element():new(Config():get_config('ProgressBarFinishedCharacter'));
                                 }

    METHOD print_bar(cDescription)
    METHOD print_description(cDescription)
    METHOD create_description()
    METHOD last_filled(nStart, nEnd) INLINE Round((nEnd - nStart - 1);
                                            * IF(::lReverse, ::nTargetValue - ::nActualValue, ::nActualValue) / ::nTargetValue, 0)

ENDCLASS LOCK

METHOD color(nIndex, cColor) CLASS Progress_bar

    LOCAL cOldColor

    assert_type(nIndex, 'N')

    IF nIndex < 1 .OR. nIndex > N_FINISHED .OR. Int(nIndex) != nIndex
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF

    cOldColor := ::aoElements[nIndex]

    IF cColor != NIL
        assert_type(cColor, 'C')
        IF !Empty(cColor) .AND. !is_color(cColor)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
        
        ::aoElements[nIndex]:cColor := cColor
    ENDIF

RETURN cOldColor

METHOD pattern(nIndex, cPattern) CLASS Progress_bar

    LOCAL cOldPattern

    assert_type(nIndex, 'N')

    IF nIndex < 1 .OR. nIndex > N_FINISHED .OR. Int(nIndex) != nIndex
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF

    cOldPattern := ::aoElements[nIndex]

    IF cPattern != NIL
        assert_type(cPattern, 'C')
        IF !Empty(cPattern) .AND. !is_color(cPattern)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
        
        ::aoElements[nIndex]:cPattern := cPattern
    ENDIF

RETURN cOldPattern

METHOD slash(cSlash) CLASS Progress_bar

    LOCAL cOldSlash := ::cSlash

    IF cSlash != NIL
        assert_type(cSlash, 'C')
        IF Len(cSlash) != 1
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
        ::cSlash := cSlash
    ENDIF

RETURN cOldSlash

METHOD description_color(cDescriptionColor) CLASS Progress_bar

    LOCAL cOldDescriptionColor := ::cDescriptionColor

    IF cDescriptionColor != NIL
        assert_type(cDescriptionColor, 'C')
        IF !is_color(cDescriptionColor)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
        ::cDescriptionColor := cDescriptionColor
    ENDIF

RETURN cOldDescriptionColor

METHOD actual_value(nActualValue) CLASS Progress_bar

    LOCAL nOldActualValue := ::nActualValue

    IF nActualValue != NIL
        assert_type(nActualValue, 'N')
        IF nActualValue < 0
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
        ::nActualValue := Min(nActualValue, ::nTargetValue)
    ENDIF

RETURN nOldActualValue

METHOD target_value(nTargetValue) CLASS Progress_bar
    
    LOCAL nOldTargetValue := ::nTargetValue

    IF nTargetValue != NIL
        assert_type(nTargetValue, 'N')
        IF nTargetValue < 0 .OR. nTargetValue < ::nActualValue
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
        ::nTargetValue := nTargetValue
    ENDIF

RETURN nOldTargetValue

METHOD reverse(lReverse) CLASS Progress_bar

    LOCAL lOldReverse := ::lReverse

    IF lReverse != NIL
        assert_type(lReverse, 'L')
        ::lReverse := lReverse
    ENDIF

RETURN lOldReverse

METHOD new(nRow, nLeft, nRight, nActualValue, nTargetValue, nDisplayPosition, nFormat, nAccuracy, lReverse, cDescriptionColor, cSlash, acPatterns, acColors) CLASS Progress_bar

    LOCAL cDescription
    LOCAL i

    assert_type(nRow, 'N')
    assert_type(nLeft, 'N')
    assert_type(nRight, 'N')

    IF nRight <= nLeft
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF

    ::nRow := nRow
    ::nLeft := nLeft
    ::nRight := nRight

    IF nActualValue != NIL
        assert_type(nActualValue, 'N')
        IF nActualValue < 0
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
        ::nActualValue := nActualValue
    ENDIF

    IF nTargetValue != NIL
        assert_type(nTargetValue, 'N')
        IF nTargetValue < 0
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
        ::nTargetValue := nTargetValue
    ENDIF
    
    IF ::nActualValue > ::nTargetValue
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF

    IF nDisplayPosition != NIL
        assert_type(nDisplayPosition, 'N')
        IF AScan(AN_ALL_POSITIONS, nDisplayPosition) == 0
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
        ::nDisplayPosition := nDisplayPosition
    ENDIF

    IF nFormat != NIL
        assert_type(nFormat, 'N')
        IF AScan({N_FORMAT_SLASH, N_FORMAT_PERCENT}, nFormat) == 0
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
        ::nFormat := nFormat
    ENDIF

    IF nAccuracy != NIL
        assert_type(nAccuracy, 'N')
        IF AScan({0, 1, 2}, nAccuracy) == 0
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
        ::nAccuracy := nAccuracy
    ENDIF

    IF lReverse != NIL
        assert_type(lReverse, 'L')
        ::lReverse := lReverse
    ENDIF

    IF cDescriptionColor != NIL
        assert_type(cDescriptionColor, 'C')
        IF !is_color(cDescriptionColor)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
        ::cDescriptionColor := cDescriptionColor
    ENDIF

    IF cSlash != NIL
        assert_type(cSlash, 'C')
        IF Len(cSlash) != 1
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
        ::cSlash := cSlash
    ENDIF

    IF acPatterns != NIL
        assert_type(acPatterns, 'A')
        AEval(acPatterns, {| cPattern | IF(cPattern == NIL, , (assert_type(cPattern, 'C'), IF(Len(cPattern) == 1, , throw(ARGUMENT_VALUE_EXCEPTION))))})
        FOR i := 1 TO Len(acPatterns)
            IF acPatterns[i] != NIL
                ::aoElements[i]:cPattern := acPatterns[i]
            ENDIF
        NEXT
    ENDIF

    IF acColors != NIL
        assert_type(acColors, 'A')
        AEval(acColors, {| cColor | IF(cColor == NIL, , (assert_type(cColor, 'C'), IF(is_color(cColor), , throw(ARGUMENT_VALUE_EXCEPTION))))})
        FOR i := 1 TO Len(acColors)
            IF acColors[i] != NIL
                ::aoElements[i]:cColor := acColors[i]
            ENDIF
        NEXT
    ENDIF

    cDescription := ::create_description()
    IF nLeft + Len(cDescription) >= nRight
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF

RETURN Self

METHOD increment(nHowMany) CLASS Progress_bar

    IF nHowMany == NIL
        ::nActualValue := Min(::nActualValue + 1, ::nTargetValue)
    ELSE
        assert_type(nHowMany, 'N')
        ::nActualValue := Min(Max(::nActualValue + nHowMany, 0), ::nTargetValue)
    ENDIF

RETURN NIL

METHOD decrement(nHowMany) CLASS Progress_bar

    IF nHowMany == NIL
        ::increment(-1)
    ELSE
        ::increment(nHowMany)
    ENDIF

RETURN NIL

METHOD display() CLASS Progress_bar

    LOCAL cDescription := ::create_description()

    DispBegin()
    ::print_bar(cDescription)
    ::print_description(cDescription)
    DispEnd()

RETURN NIL

METHOD print_description(cDescription) CLASS Progress_bar

    LOCAL nLastFilled

    DO CASE
        CASE ::nDisplayPosition == N_POSITION_NONE
        CASE ::nDisplayPosition == N_POSITION_RIGHT
            @ ::nRow, ::nRight - Len(cDescription) + 1 SAY cDescription COLOR ::cDescriptionColor
        CASE ::nDisplayPosition == N_POSITION_CENTER
            @ ::nRow, Int((::nRight + ::nLeft - Len(cDescription)) / 2) SAY cDescription COLOR ::cDescriptionColor
        CASE ::nDisplayPosition == N_POSITION_LEFT
            @ ::nRow, ::nLeft SAY cDescription COLOR ::cDescriptionColor
        CASE ::nDisplayPosition == N_POSITION_BOTTOM
            @ ::nRow + 1, Int((::nRight + ::nLeft - Len(cDescription)) / 2) SAY cDescription COLOR ::cDescriptionColor
        CASE ::nDisplayPosition == N_POSITION_TOP
            @ ::nRow - 1, Int((::nRight + ::nLeft - Len(cDescription)) / 2) SAY cDescription COLOR ::cDescriptionColor
        CASE ::nDisplayPosition == N_POSITION_CHASE
            nLastFilled := ::nLeft + ::last_filled(::nLeft, ::nRight - Len(cDescription) - 1)
            IF nLastFilled + Len(cDescription) + 1 < ::nRight - Len(cDescription)
                @ ::nRow, nLastFilled + 1 SAY cDescription COLOR ::cDescriptionColor
            ELSE
                @ ::nRow, ::nRight - Len(cDescription) + 1 SAY cDescription COLOR ::cDescriptionColor
            ENDIF
    ENDCASE

RETURN NIL

METHOD create_description() CLASS Progress_bar

    LOCAL cString

    DO CASE
        CASE ::nFormat == N_FORMAT_SLASH
            cString := Transform(::nActualValue, Replicate('9', Len(LTrim(Str(::nTargetValue)))));
                       + ::cSlash;
                       + Transform(::nTargetValue, Replicate('9', Len(LTrim(Str(::nTargetValue)))))
        CASE ::nFormat == N_FORMAT_PERCENT
            cString := Transform(Round(100 * ::nActualValue / ::nTargetValue, ::nAccuracy);
                       , '999' + IF(::nAccuracy > 0, '.' + Replicate('9', ::nAccuracy), '')) + '%'
    ENDCASE

RETURN cString

METHOD print_bar(cDescription) CLASS Progress_bar

    LOCAL nStart := ::nLeft
    LOCAL nEnd := ::nRight
    LOCAL nLastFilled
    LOCAL nCurrentCol 

    @ ::nRow, ::nLeft CLEAR TO ::nRow, ::nRight

    IF ::nDisplayPosition == N_POSITION_LEFT
        nStart += Len(cDescription) + 1
    ELSEIF ::nDisplayPosition == N_POSITION_RIGHT .OR. ::nDisplayPosition == N_POSITION_CHASE
        nEnd -= Len(cDescription) + 1
    ENDIF

    nCurrentCol := nStart
    ::aoElements[N_START]:print(::nRow, nCurrentCol)

    nLastFilled := nStart + ::last_filled(nStart, nEnd)

    ++nCurrentCol
    IF (::lReverse .AND. ::nActualValue == 0) .OR. (!::lReverse .AND. ::nActualValue == ::nTargetValue)
        DO WHILE nCurrentCol <= nLastFilled
            ::aoElements[N_FINISHED]:print(::nRow, nCurrentCol)
            ++nCurrentCol
        ENDDO
    ELSE
        DO WHILE nCurrentCol <= nLastFilled
            ::aoElements[N_FILLED]:print(::nRow, nCurrentCol)
            ++nCurrentCol
        ENDDO
    ENDIF

    DO WHILE nCurrentCol < nEnd
        ::aoElements[N_EMPTY]:print(::nRow, nCurrentCol)
        ++nCurrentCol
    ENDDO

    ::aoElements[N_END]:print(::nRow, nEnd)

RETURN NIL
