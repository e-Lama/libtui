#include "hbclass.ch"

#include "functions.ch"
#include "progressbar.ch"

#include "setup.ch"

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

    METHOD new(nRow, nLeft, nRight, nActualValue, nTargetValue, nDisplayPosition, nFormat, nAccuracy;
               , lReverse, cDescriptionColor, cSlash, acPatterns, acColors) CONSTRUCTOR

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

    VAR __nRow AS NUMERIC
    VAR __nLeft AS NUMERIC
    VAR __nRight AS NUMERIC
    VAR __nActualValue AS NUMERIC INIT 0
    VAR __nTargetValue AS NUMERIC INIT 100
    VAR __nDisplayPosition AS NUMERIC INIT N_POSITION_RIGHT
    VAR __nFormat AS NUMERIC INIT N_FORMAT_PERCENT
    VAR __lReverse AS LOGICAL INIT .F.
    VAR __cDescriptionColor AS CHARACTER INIT Config():get_config('DefaultColor')
    VAR __nAccuracy AS NUMERIC INIT 0
    VAR __cSlash AS CHARACTER INIT '/'
    VAR __aoElements AS ARRAY INIT {Element():new(Config():get_config('ProgressBarFirstCharacter'));
                                  , Element():new(Config():get_config('ProgressBarFilledCharacter'));
                                  , Element():new(Config():get_config('ProgressBarEmptyCharacter'));
                                  , Element():new(Config():get_config('ProgressBarLastCharacter'));
                                  , Element():new(Config():get_config('ProgressBarFinishedCharacter'));
                                 }

    METHOD __print_bar(cDescription)
    METHOD __print_description(cDescription)
    METHOD __create_description()
    METHOD __last_filled(nStart, nEnd) INLINE Round((nEnd - nStart - 1);
                                            * IF(::__lReverse, ::__nTargetValue - ::__nActualValue, ::__nActualValue) / ::__nTargetValue, 0)

ENDCLASS LOCK

METHOD color(nIndex, cColor) CLASS Progress_bar

    LOCAL cOldColor

#ifdef USE_VALIDATORS
    assert_type(nIndex, 'N')

    IF nIndex < 1 .OR. nIndex > N_FINISHED .OR. Int(nIndex) != nIndex
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF
#endif

    cOldColor := ::__aoElements[nIndex]:cColor

    IF cColor != NIL
#ifdef USE_VALIDATORS
        assert_type(cColor, 'C')
        IF !Empty(cColor) .AND. !is_color(cColor)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif
        
        ::__aoElements[nIndex]:cColor := cColor
    ENDIF

RETURN cOldColor

METHOD pattern(nIndex, cPattern) CLASS Progress_bar

    LOCAL cOldPattern

#ifdef USE_VALIDATORS
    assert_type(nIndex, 'N')

    IF nIndex < 1 .OR. nIndex > N_FINISHED .OR. Int(nIndex) != nIndex
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF
#endif

    cOldPattern := ::__aoElements[nIndex]

    IF cPattern != NIL
#ifdef USE_VALIDATORS
        assert_type(cPattern, 'C')
        IF !Empty(cPattern) .AND. !is_color(cPattern)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif
        
        ::__aoElements[nIndex]:cPattern := cPattern
    ENDIF

RETURN cOldPattern

METHOD slash(cSlash) CLASS Progress_bar

    LOCAL cOldSlash := ::__cSlash

    IF cSlash != NIL
#ifdef USE_VALIDATORS
        assert_type(cSlash, 'C')
        IF Len(cSlash) != 1
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif

        ::__cSlash := cSlash
    ENDIF

RETURN cOldSlash

METHOD description_color(cDescriptionColor) CLASS Progress_bar

    LOCAL cOldDescriptionColor := ::__cDescriptionColor

    IF cDescriptionColor != NIL
#ifdef USE_VALIDATORS
        assert_type(cDescriptionColor, 'C')
        IF !is_color(cDescriptionColor)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif

        ::__cDescriptionColor := cDescriptionColor
    ENDIF

RETURN cOldDescriptionColor

METHOD actual_value(nActualValue) CLASS Progress_bar

    LOCAL nOldActualValue := ::__nActualValue

    IF nActualValue != NIL
#ifdef USE_VALIDATORS
        assert_type(nActualValue, 'N')
        IF nActualValue < 0
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif

        ::__nActualValue := Min(nActualValue, ::__nTargetValue)
    ENDIF

RETURN nOldActualValue

METHOD target_value(nTargetValue) CLASS Progress_bar
    
    LOCAL nOldTargetValue := ::__nTargetValue

    IF nTargetValue != NIL
#ifdef USE_VALIDATORS
        assert_type(nTargetValue, 'N')
        IF nTargetValue < 0 .OR. nTargetValue < ::__nActualValue
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif

        ::__nTargetValue := nTargetValue
    ENDIF

RETURN nOldTargetValue

METHOD reverse(lReverse) CLASS Progress_bar

    LOCAL lOldReverse := ::__lReverse

    IF lReverse != NIL
#ifdef USE_VALIDATORS
        assert_type(lReverse, 'L')
#endif

        ::__lReverse := lReverse
    ENDIF

RETURN lOldReverse

METHOD new(nRow, nLeft, nRight, nActualValue, nTargetValue, nDisplayPosition, nFormat;
           , nAccuracy, lReverse, cDescriptionColor, cSlash, acPatterns, acColors;
          ) CLASS Progress_bar

#ifdef USE_VALIDATORS
    LOCAL cDescription
#endif
    LOCAL i

#ifdef USE_VALIDATORS
    assert_type(nRow, 'N')
    assert_type(nLeft, 'N')
    assert_type(nRight, 'N')

    IF nRight <= nLeft
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF
#endif

    ::__nRow := nRow
    ::__nLeft := nLeft
    ::__nRight := nRight

    IF nActualValue != NIL
#ifdef USE_VALIDATORS
        assert_type(nActualValue, 'N')
        IF nActualValue < 0
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif

        ::__nActualValue := nActualValue
    ENDIF

    IF nTargetValue != NIL
#ifdef USE_VALIDATORS
        assert_type(nTargetValue, 'N')
        IF nTargetValue < 0
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif

        ::__nTargetValue := nTargetValue
    ENDIF
    
#ifdef USE_VALIDATORS
    IF ::__nActualValue > ::__nTargetValue
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF
#endif

    IF nDisplayPosition != NIL
#ifdef USE_VALIDATORS
        assert_type(nDisplayPosition, 'N')
        IF AScan(AN_ALL_POSITIONS, nDisplayPosition) == 0
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif

        ::__nDisplayPosition := nDisplayPosition
    ENDIF

    IF nFormat != NIL
#ifdef USE_VALIDATORS
        assert_type(nFormat, 'N')
        IF AScan({N_FORMAT_SLASH, N_FORMAT_PERCENT}, nFormat) == 0
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif

        ::__nFormat := nFormat
    ENDIF

    IF nAccuracy != NIL
#ifdef USE_VALIDATORS
        assert_type(nAccuracy, 'N')
        IF AScan({0, 1, 2}, nAccuracy) == 0
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif

        ::__nAccuracy := nAccuracy
    ENDIF

    IF lReverse != NIL
#ifdef USE_VALIDATORS
        assert_type(lReverse, 'L')
#endif

        ::__lReverse := lReverse
    ENDIF

    IF cDescriptionColor != NIL
#ifdef USE_VALIDATORS
        assert_type(cDescriptionColor, 'C')
        IF !is_color(cDescriptionColor)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif

        ::__cDescriptionColor := cDescriptionColor
    ENDIF

    IF cSlash != NIL
#ifdef USE_VALIDATORS
        assert_type(cSlash, 'C')
        IF Len(cSlash) != 1
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif

        ::__cSlash := cSlash
    ENDIF

    IF acPatterns != NIL
#ifdef USE_VALIDATORS
        assert_type(acPatterns, 'A')
        AEval(acPatterns, {| cPattern | IF(cPattern == NIL, , (assert_type(cPattern, 'C'), IF(Len(cPattern) == 1, , throw(ARGUMENT_VALUE_EXCEPTION))))})
#endif

        FOR i := 1 TO Len(acPatterns)
            IF acPatterns[i] != NIL
                ::__aoElements[i]:cPattern := acPatterns[i]
            ENDIF
        NEXT
    ENDIF

    IF acColors != NIL
#ifdef USE_VALIDATORS
        assert_type(acColors, 'A')
        AEval(acColors, {| cColor | IF(cColor == NIL, , (assert_type(cColor, 'C'), IF(is_color(cColor), , throw(ARGUMENT_VALUE_EXCEPTION))))})
#endif

        FOR i := 1 TO Len(acColors)
            IF acColors[i] != NIL
                ::__aoElements[i]:cColor := acColors[i]
            ENDIF
        NEXT
    ENDIF

#ifdef USE_VALIDATORS
    cDescription := ::__create_description()

    IF nLeft + Len(cDescription) >= nRight
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF
#endif

RETURN Self

METHOD increment(nHowMany) CLASS Progress_bar

    IF nHowMany == NIL
        ::__nActualValue := Min(::__nActualValue + 1, ::__nTargetValue)
    ELSE
#ifdef USE_VALIDATORS
        assert_type(nHowMany, 'N')
#endif
        ::__nActualValue := Min(Max(::__nActualValue + nHowMany, 0), ::__nTargetValue)
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

    LOCAL cDescription := ::__create_description()

    DispBegin()
    ::__print_bar(cDescription)
    ::__print_description(cDescription)
    DispEnd()

RETURN NIL

METHOD __print_description(cDescription) CLASS Progress_bar

    LOCAL nLastFilled

    DO CASE
        CASE ::__nDisplayPosition == N_POSITION_NONE
        CASE ::__nDisplayPosition == N_POSITION_RIGHT
            @ ::__nRow, ::__nRight - Len(cDescription) + 1 SAY cDescription COLOR ::__cDescriptionColor
        CASE ::__nDisplayPosition == N_POSITION_CENTER
            @ ::__nRow, Int((::__nRight + ::__nLeft - Len(cDescription)) / 2) SAY cDescription COLOR ::__cDescriptionColor
        CASE ::__nDisplayPosition == N_POSITION_LEFT
            @ ::__nRow, ::__nLeft SAY cDescription COLOR ::__cDescriptionColor
        CASE ::__nDisplayPosition == N_POSITION_BOTTOM
            @ ::__nRow + 1, Int((::__nRight + ::__nLeft - Len(cDescription)) / 2) SAY cDescription COLOR ::__cDescriptionColor
        CASE ::__nDisplayPosition == N_POSITION_TOP
            @ ::__nRow - 1, Int((::__nRight + ::__nLeft - Len(cDescription)) / 2) SAY cDescription COLOR ::__cDescriptionColor
        CASE ::__nDisplayPosition == N_POSITION_CHASE
            nLastFilled := ::__nLeft + ::__last_filled(::__nLeft, ::__nRight - Len(cDescription) - 1)
            IF nLastFilled + Len(cDescription) + 1 < ::__nRight - Len(cDescription)
                @ ::__nRow, nLastFilled + 1 SAY cDescription COLOR ::__cDescriptionColor
            ELSE
                @ ::__nRow, ::__nRight - Len(cDescription) + 1 SAY cDescription COLOR ::__cDescriptionColor
            ENDIF
    ENDCASE

RETURN NIL

METHOD __create_description() CLASS Progress_bar

    LOCAL cString

    DO CASE
        CASE ::__nFormat == N_FORMAT_SLASH
            cString := Transform(::__nActualValue, Replicate('9', Len(LTrim(Str(::__nTargetValue)))));
                       + ::__cSlash;
                       + Transform(::__nTargetValue, Replicate('9', Len(LTrim(Str(::__nTargetValue)))))
        CASE ::__nFormat == N_FORMAT_PERCENT
            cString := Transform(Round(100 * ::__nActualValue / ::__nTargetValue, ::__nAccuracy);
                       , '999' + IF(::__nAccuracy > 0, '.' + Replicate('9', ::__nAccuracy), '')) + '%'
    ENDCASE

RETURN cString

METHOD __print_bar(cDescription) CLASS Progress_bar

    LOCAL nStart := ::__nLeft
    LOCAL nEnd := ::__nRight
    LOCAL nLastFilled
    LOCAL nCurrentCol 

    @ ::__nRow, ::__nLeft CLEAR TO ::__nRow, ::__nRight

    IF ::__nDisplayPosition == N_POSITION_LEFT
        nStart += Len(cDescription) + 1
    ELSEIF ::__nDisplayPosition == N_POSITION_RIGHT .OR. ::__nDisplayPosition == N_POSITION_CHASE
        nEnd -= Len(cDescription) + 1
    ENDIF

    nCurrentCol := nStart
    ::__aoElements[N_START]:print(::__nRow, nCurrentCol)

    nLastFilled := nStart + ::__last_filled(nStart, nEnd)

    ++nCurrentCol
    IF (::__lReverse .AND. ::__nActualValue == 0) .OR. (!::__lReverse .AND. ::__nActualValue == ::__nTargetValue)
        DO WHILE nCurrentCol <= nLastFilled
            ::__aoElements[N_FINISHED]:print(::__nRow, nCurrentCol)
            ++nCurrentCol
        ENDDO
    ELSE
        DO WHILE nCurrentCol <= nLastFilled
            ::__aoElements[N_FILLED]:print(::__nRow, nCurrentCol)
            ++nCurrentCol
        ENDDO
    ENDIF

    DO WHILE nCurrentCol < nEnd
        ::__aoElements[N_EMPTY]:print(::__nRow, nCurrentCol)
        ++nCurrentCol
    ENDDO

    ::__aoElements[N_END]:print(::__nRow, nEnd)

RETURN NIL
