#include "inkey.ch"
#include "color.ch"

#include "functions.ch"
#include "yesno.ch"

#include "setup.ch"

FUNCTION YesNo(xMessage, acAnwsers, cColor, lSafe, lAllowMove, nCurrentOption;
               , lCyclic, lAcceptFirstFounded, cBorder, cAlign, lAllowMouse;
              )
    
    LOCAL lResult

#ifdef USE_VALIDATORS
    IF PCount() < 1 .OR. PCount() > 11
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ENDIF

    IF ValType(acAnwsers) == 'A'
        assert_length(acAnwsers, 2)
    ENDIF
#endif

    hb_Default(@lSafe, .F.)

    IF lSafe
        lResult := IF(__print_message(xMessage, IF(ValType(acAnwsers) == 'A', acAnwsers, {SAFE_YES, SAFE_NO});
                          , IF(ValType(cColor) == 'C', cColor, YESNO_SAFE_COLOR), lSafe, NIL, .T.;
                          , IF(ValType(lAllowMove) == 'L', lAllowMove, .T.);
                          , IF(ValType(nCurrentOption) == 'N', nCurrentOption, 1);
                          , IF(ValType(lCyclic) == 'L', lCyclic, .T.);
                          , IF(ValType(lAcceptFirstFounded) == 'L', lAcceptFirstFounded, .F.);
                          , IF(ValType(cBorder) == 'C', cBorder, NIL);
                          , IF(ValType(cAlign) == 'C', cAlign, NIL);
                          , IF(ValType(lAllowMouse) == 'L', lAllowMouse, .T.);
                          ) == 1, .T., .F.;
                     )
    ELSE
        lResult := IF(__print_message(xMessage, IF(ValType(acAnwsers) == 'A', acAnwsers, {YES, NO});
                          , IF(ValType(cColor) == 'C', cColor, YESNO_COLOR), .F., NIL, lSafe;
                          , IF(ValType(lAllowMove) == 'L', lAllowMove, YESNO_MOVE);
                          , IF(ValType(nCurrentOption) == 'N', nCurrentOption, 1);
                          , IF(ValType(lCyclic) == 'L', lCyclic, YESNO_CYCLIC);
                          , IF(ValType(lAcceptFirstFounded) == 'L', lAcceptFirstFounded, YESNO_ACCEPT_FIRST);
                          , IF(ValType(cBorder) == 'C', cBorder, NIL);
                          , IF(ValType(cAlign) == 'C', cAlign, NIL);
                          , IF(ValType(lAllowMouse) == 'L', lAllowMouse, .T.);
                          ) == 1, .T., .F.;
                     )
    ENDIF

RETURN lResult

FUNCTION NoYes(xMessage, acAnwsers, cColor, lSafe, lAllowMove, nCurrentOption, lCyclic;
               , lAcceptFirstFounded, cBorder, cAlign, lAllowMouse;
              )

    LOCAL lResult

#ifdef USE_VALIDATORS
    IF PCount() < 1 .OR. PCount() > 11
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ENDIF

    IF ValType(acAnwsers) == 'A'
        assert_length(acAnwsers, 2)
    ENDIF
#endif

    hb_Default(@lSafe, .F.)

    IF lSafe
        lResult := IF(__print_message(xMessage, IF(ValType(acAnwsers) == 'A', acAnwsers, {SAFE_NO, SAFE_YES});
                          , IF(ValType(cColor) == 'C', cColor, YESNO_SAFE_COLOR), .F., NIL, lSafe;
                          , IF(ValType(lAllowMove) == 'L', lAllowMove, .T.);
                          , IF(ValType(nCurrentOption) == 'N', nCurrentOption, 1);
                          , IF(ValType(lCyclic) == 'L', lCyclic, .T.);
                          , IF(ValType(lAcceptFirstFounded) == 'L', lAcceptFirstFounded, .F.);
                          , IF(ValType(cBorder) == 'C', cBorder, NIL);
                          , IF(ValType(cAlign) == 'C', cAlign, NIL);
                          , IF(ValType(lAllowMouse) == 'L', lAllowMouse, .T.);
                          ) == 1, .F., .T.;
                     )
    ELSE
        lResult := IF(__print_message(xMessage, IF(ValType(acAnwsers) == 'A', acAnwsers, {NO, YES});
                          , IF(ValType(cColor) == 'C', cColor, YESNO_COLOR), .F., NIL, lSafe;
                          , IF(ValType(lAllowMove) == 'L', lAllowMove, YESNO_MOVE);
                          , IF(ValType(nCurrentOption) == 'N', nCurrentOption, 1);
                          , IF(ValType(lCyclic) == 'L', lCyclic, YESNO_CYCLIC);
                          , IF(ValType(lAcceptFirstFounded) == 'L', lAcceptFirstFounded, YESNO_ACCEPT_FIRST);
                          , IF(ValType(cBorder) == 'C', cBorder, NIL);
                          , IF(ValType(cAlign) == 'C', cAlign, NIL);
                          , IF(ValType(lAllowMouse) == 'L', lAllowMouse, .T.);
                          ) == 1, .F., .T.;
                     )
    ENDIF

RETURN lResult

FUNCTION Dialog(xMessage, acOptions, cColor, lAllowEscape, nDelay, lSafe, lAllowMove, nCurrentOption;
                , lCyclic, lAcceptFirstFounded, cBorder, cAlign, lAllowMouse;
               )

    LOCAL nResult

#ifdef USE_VALIDATORS
    IF PCount() < 1 .OR. PCount() > 13
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ENDIF
#endif

    hb_Default(@lSafe, .F.)

    IF lSafe
        nResult := __print_message(xMessage, acOptions, IF(ValType(cColor) == 'C', cColor, DIALOG_SAFE_COLOR);
                                  , IF(ValType(lAllowEscape) == 'L', lAllowEscape, .F.), nDelay, lSafe;
                                  , IF(ValType(lAllowMove) == 'L', lAllowMove, .T.);
                                  , IF(ValType(nCurrentOption) == 'N', nCurrentOption, 1);
                                  , IF(ValType(lCyclic) == 'L', lCyclic, .T.);
                                  , IF(ValType(lAcceptFirstFounded) == 'L', lAcceptFirstFounded, .F.);
                                  , IF(ValType(cBorder) == 'C', cBorder, NIL);
                                  , IF(ValType(cAlign) == 'C', cAlign, NIL);
                                  , IF(ValType(lAllowMouse) == 'L', lAllowMouse, .T.);
                                  )
    ELSE
        nResult := __print_message(xMessage, acOptions, IF(ValType(cColor) == 'C', cColor, DIALOG_COLOR);
                                  , IF(ValType(lAllowEscape) == 'L', lAllowEscape, .F.), nDelay, lSafe;
                                  , IF(ValType(lAllowMove) == 'L', lAllowMove, DIALOG_MOVE);
                                  , IF(ValType(nCurrentOption) == 'N', nCurrentOption, 1);
                                  , IF(ValType(lCyclic) == 'L', lCyclic, DIALOG_CYCLIC);
                                  , IF(ValType(lAcceptFirstFounded) == 'L', lAcceptFirstFounded, DIALOG_ACCEPT_FIRST);
                                  , IF(ValType(cBorder) == 'C', cBorder, NIL);
                                  , IF(ValType(cAlign) == 'C', cAlign, NIL);
                                  , IF(ValType(lAllowMouse) == 'L', lAllowMouse, .T.);
                                  )
    ENDIF

RETURN nResult

FUNCTION Inform(xMessage, cColor, lAllowEscape, nDelay, lSafe, lAllowMove, lCyclic;
                , lAcceptFirstFounded, cBorder, cAlign, lAllowMouse;
               )

    LOCAL nResult

#ifdef USE_VALIDATORS
    IF PCount() < 1 .OR. PCount() > 11
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ENDIF
#endif

    hb_Default(@lSafe, .F.)
    
    IF lSafe
        nResult := __print_message(xMessage, {SAFE_OK}, IF(ValType(cColor) == 'C', cColor, INFORM_SAFE_COLOR);
                                  , IF(ValType(lAllowEscape) == 'L', lAllowEscape, .T.), nDelay, lSafe;
                                  , IF(ValType(lAllowMove) == 'L', lAllowMove, .T.);
                                  , 1, IF(ValType(lCyclic) == 'L', lCyclic, .T.);
                                  , IF(ValType(lAcceptFirstFounded) == 'L', lAcceptFirstFounded, .F.);
                                  , IF(ValType(cBorder) == 'C', cBorder, NIL);
                                  , IF(ValType(cAlign) == 'C', cAlign, NIL);
                                  , IF(ValType(lAllowMouse) == 'L', lAllowMouse, .T.);
                                  )
    ELSE
        nResult := __print_message(xMessage, {OK}, IF(ValType(cColor) == 'C', cColor, INFORM_COLOR);
                                  , IF(ValType(lAllowEscape) == 'L', lAllowEscape, .T.), nDelay, lSafe;
                                  , IF(ValType(lAllowMove) == 'L', lAllowMove, INFORM_MOVE);
                                  , 1, IF(ValType(lCyclic) == 'L', INFORM_CYCLIC, .T.);
                                  , IF(ValType(lAcceptFirstFounded) == 'L', lAcceptFirstFounded, INFORM_ACCEPT_FIRST);
                                  , IF(ValType(cBorder) == 'C', cBorder, NIL);
                                  , IF(ValType(cAlign) == 'C', cAlign, NIL);
                                  , IF(ValType(lAllowMouse) == 'L', lAllowMouse, .T.);
                                  )
    ENDIF

RETURN nResult

STATIC FUNCTION __print_message(xMessage, acOptions, cColor, lAllowEscape, nDelay, lSafe;
                                , lAllowMove, nCurrentOption, lCyclic, lAcceptFirstFounded;
                                , cBorder, cAlign, lAllowMouse;
                               )

    LOCAL acOptionsOk := Array(0)
    LOCAL nOldShadow := WSetShadow(-1)
    LOCAL nEval
    LOCAL nReturn

#ifdef USE_VALIDATORS
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
#endif

    FOR nEval := 1 TO Len(acOptions)
        IF ValType(acOptions[nEval]) == 'C' .AND. !Empty(acOptions[nEval])
            AAdd(acOptionsOk, acOptions[nEval])
        ENDIF
    NEXT

    IF Len(acOptionsOk) == 0
        acOptionsOk := IF(lSafe, 'Ok', {Config():get_config('DefaultPrintMessageOption')})
    ENDIF

    nReturn := AlertLG():AlertLG(xMessage, acOptionsOk, hb_ColorIndex(cColor, CLR_STANDARD);
                                , hb_ColorIndex(CLR_ENHANCED), nDelay, NIL, NIL, NIL, nCurrentOption;
                                , lAllowEscape, lAllowMove, lCyclic, lAcceptFirstFounded, cBorder;
                                , cAlign, lAllowMouse;
                                )

    WSetShadow(NToColor(nOldShadow))

RETURN nReturn
