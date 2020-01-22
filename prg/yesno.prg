#include "inkey.ch"
#include "color.ch"

#include "functions.ch"
#include "yesno.ch"

FUNCTION YesNo(xMessage, acAnwsers, cColor, lSafe, lAllowMove, nCurrentOption, cBorder)
    
    LOCAL lResult

    IF PCount() < 1 .OR. PCount() > 7
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ENDIF

    lSafe := without_config(lSafe)

    IF ValType(acAnwsers) == 'A' .AND. Len(acAnwsers) != 2
        throw(RUNTIME_EXCEPTION)
    ENDIF

    IF lSafe
        IF print_message(xMessage, IF(ValType(acAnwsers) == 'A', acAnwsers, {SAFE_YES, SAFE_NO}), IF(ValType(cColor) == 'C', cColor, YESNO_SAFE_COLOR), lSafe, NIL, .T., IF(ValType(lAllowMove) == 'L', lAllowMove, .T.), IF(ValType(nCurrentOption) == 'N', nCurrentOption, 1), IF(ValType(cBorder) == 'C', cBorder, NIL)) == 1
            lResult := .T.
        ELSE
            lResult := .F.
        ENDIF
    ELSE
        IF print_message(xMessage, IF(ValType(acAnwsers) == 'A', acAnwsers, {YES, NO}), IF(ValType(cColor) == 'C', cColor, YESNO_COLOR), .F., NIL, lSafe, IF(ValType(lAllowMove) == 'L', lAllowMove, .T.), IF(ValType(nCurrentOption) == 'N', nCurrentOption, 1), IF(ValType(cBorder) == 'C', cBorder, NIL)) == 1
            lResult := .T.
        ELSE
            lResult := .F.
        ENDIF
    ENDIF

RETURN lResult

FUNCTION NoYes(xMessage, acAnwsers, cColor, lSafe, lAllowMove, nCurrentOption, cBorder)

    LOCAL lResult

    IF PCount() < 1 .OR. PCount() > 7
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ENDIF

    lSafe := without_config(lSafe)

    IF ValType(acAnwsers) == 'A' .AND. Len(acAnwsers) != 2
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF

    IF lSafe
        IF print_message(xMessage, IF(ValType(acAnwsers) == 'A', acAnwsers, {SAFE_NO, SAFE_YES}), IF(ValType(cColor) == 'C', cColor, YESNO_SAFE_COLOR), .F., NIL, lSafe, IF(ValType(lAllowMove) == 'L', lAllowMove, .T.), IF(ValType(nCurrentOption) == 'N', nCurrentOption, 1), IF(ValType(cBorder) == 'C', cBorder, NIL)) == 1
            lResult := .F.
        ELSE
            lResult := .T.
        ENDIF
    ELSE
        IF print_message(xMessage, IF(ValType(acAnwsers) == 'A', acAnwsers, {NO, YES}), IF(ValType(cColor) == 'C', cColor, YESNO_COLOR), .F., NIL, lSafe, IF(ValType(lAllowMove) == 'L', lAllowMove, .T.), IF(ValType(nCurrentOption) == 'N', nCurrentOption, 1), IF(ValType(cBorder) == 'C', cBorder, NIL)) == 1
            lResult := .F.
        ELSE
            lResult := .T.
        ENDIF
    ENDIF

RETURN lResult

FUNCTION Dialog(xMessage, acOptions, cColor, lAllowEscape, nDelay, lSafe, lAllowMove, nCurrentOption, cBorder)

    LOCAL nResult

    IF PCount() < 1 .OR. PCount() > 9
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ENDIF

    lSafe := without_config(lSafe)

    IF lSafe
        nResult := print_message(xMessage, acOptions, IF(ValType(cColor) == 'C', cColor, DIALOG_SAFE_COLOR), IF(ValType(lAllowEscape) == 'L', lAllowEscape, .F.), nDelay, lSafe, IF(ValType(lAllowMove) == 'L', lAllowMove, .T.), IF(ValType(nCurrentOption) == 'N', nCurrentOption, 1), IF(ValType(cBorder) == 'C', cBorder, NIL))
    ELSE
        nResult := print_message(xMessage, acOptions, IF(ValType(cColor) == 'C', cColor, DIALOG_COLOR), IF(ValType(lAllowEscape) == 'L', lAllowEscape, .F.), nDelay, lSafe, IF(ValType(lAllowMove) == 'L', lAllowMove, .T.), IF(ValType(nCurrentOption) == 'N', nCurrentOption, 1), IF(ValType(cBorder) == 'C', cBorder, NIL))
    ENDIF

RETURN nResult

FUNCTION Inform(xMessage, cColor, lAllowEscape, nDelay, lSafe, lAllowMove, nCurrentOption, cBorder)

    LOCAL nResult

    IF PCount() < 1 .OR. PCount() > 8
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ENDIF

    lSafe := without_config(lSafe)
    
    IF lSafe
        nResult := print_message(xMessage, {SAFE_OK}, IF(ValType(cColor) == 'C', cColor, INFORM_SAFE_COLOR), IF(ValType(lAllowEscape) == 'L', lAllowEscape, .T.), nDelay, lSafe, IF(ValType(lAllowMove) == 'L', lAllowMove, .T.), IF(ValType(nCurrentOption) == 'N', nCurrentOption, 1), IF(ValType(cBorder) == 'C', cBorder, NIL))
    ELSE
        nResult := print_message(xMessage, {OK}, IF(ValType(cColor) == 'C', cColor, INFORM_COLOR), IF(ValType(lAllowEscape) == 'L', lAllowEscape, .T.), nDelay, lSafe, IF(ValType(lAllowMove) == 'L', lAllowMove, .T.), IF(ValType(nCurrentOption) == 'N', nCurrentOption, 1), IF(ValType(cBorder) == 'C', cBorder, NIL))
    ENDIF

RETURN nResult

STATIC FUNCTION without_config(lSafe)
RETURN ValType(lSafe) == 'L' .AND. lSafe

STATIC FUNCTION print_message(xMessage, acOptions, cColor, lAllowEscape, nDelay, lSafe, lAllowMove, nCurrentOption, cBorder)

    LOCAL acOptionsOk := Array(0)
    LOCAL nOldShadow := WSetShadow(-1)
    LOCAL nEval
    LOCAL nReturn

    assert_type(acOptions, 'A')
    assert_type(lSafe, 'L')

    IF !is_color(cColor, .T.)
        throw(RUNTIME_EXCEPTION)
    ENDIF

    IF !(ValType(nDelay) $ 'N;U')
        throw(RUNTIME_EXCEPTION)
    ELSEIF ValType(nDelay) == 'N' .AND. Int(nDelay) != nDelay
        throw(RUNTIME_EXCEPTION)
    ENDIF

    FOR nEval := 1 TO Len(acOptions)
        IF ValType(acOptions[nEval]) == 'C' .AND. !Empty(acOptions[nEval])
            AAdd(acOptionsOk, acOptions[nEval])
        ENDIF
    NEXT

    IF Len(acOptionsOk) == 0
        acOptionsOk := IF(lSafe, 'Ok', {Config():get_config('DefaultPrintMessageOption')})
    ENDIF

    nReturn := AlertLG():AlertLG(xMessage, acOptionsOk, hb_ColorIndex(cColor, CLR_STANDARD), hb_ColorIndex(CLR_ENHANCED), nDelay, NIL, NIL, NIL, nCurrentOption, lAllowEscape, lAllowMove, cBorder)

    WSetShadow(NToColor(nOldShadow))

RETURN nReturn
