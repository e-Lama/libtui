#include "achoice.ch"
#include "inkey.ch"
#include "hbclass.ch"

#include "align.ch"
#include "functions.ch"
#include "menu.ch"

CREATE CLASS Menu MODULE FRIENDLY

EXPORTED:

    METHOD keys(anKeys) SETGET
    METHOD key(nPosition, nKey) SETGET

HIDDEN:

    CLASSVAR anKeys INIT {K_ENTER, K_ESC, K_ALT_UP, K_ALT_LEFT, K_ALT_DOWN, K_ALT_RIGHT, K_ALT_ENTER}

ENDCLASS LOCK

METHOD keys(anKeys) CLASS Menu

    LOCAL anOldKeys := AClone(anKeys)

    IF anKeys != NIL
        assert_type(anKeys, 'A')
        assert_length(anKeys, Len(::anKeys))
        AEval(anKeys, {| nKey | assert_type(nKey, 'N')})
        ::anKeys := anKeys
    ENDIF

RETURN anOldKeys

METHOD key(nPosition, nKey) CLASS Menu

    LOCAL nOldKey

    assert_type(nPosition, 'N')
    IF nPosition < 0 .OR. nPosition > Len(::anKeys) .OR. Int(nPosition) != nPosition
        throw(ARGUMENT_VALUE_EXCEPTION)
    ELSE
        nOldKey := ::anKeys[nPosition]
    ENDIF

    IF ValType(nKey) != 'U'
        assert_type(nKey, 'N')
        ::anKeys[nPosition] := nKey
    ENDIF

RETURN nOldKey

FUNCTION display_menu(nTop, nLeft, nBottom, nRight, acMenuItems, xSelectable, cUserFunctionName, nInitialItem, cColor, cBorder, cTitle, cTitleColor, cTitleAlign)

    LOCAL nOldWindow := WSelect()
    LOCAL nOldShadow
    LOCAL cOldColor
    LOCAL nSelectedItem

    assert_type(nTop, 'N')
    assert_type(nLeft, 'N')
    assert_type(nBottom, 'N')
    assert_type(nRight, 'N')
    assert_type(acMenuItems, 'A')

    WSelect(0)
    nOldShadow := WSetShadow(-1)

    IF nTop < 0 .OR. nTop >= MaxRow()
        throw(ARGUMENT_VALUE_EXCEPTION)
    ELSEIF nBottom < 0 .OR. nBottom > MaxRow()
        throw(ARGUMENT_VALUE_EXCEPTION)
    ELSEIF nLeft < 0 .OR. nLeft >= MaxCol()
        throw(ARGUMENT_VALUE_EXCEPTION)
    ELSEIF nRight < 0 .OR. nRight > MaxCol()
        throw(ARGUMENT_VALUE_EXCEPTION)
    ELSEIF nTop >= nBottom .OR. nLeft >= nRight
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF

    AEval(acMenuItems, {| cElement | assert_type(cElement, 'C')})

    IF ValType(xSelectable) == 'A'
        AEval(xSelectable, {| lElement | assert_type(lElement, 'L')})
    ELSEIF !(ValType(xSelectable) $ 'L;U')
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF

    IF !(ValType(nInitialItem) $ 'N;U')
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF

    IF ValType(cColor) == 'C' .AND. !is_color(cColor, .T.)
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF

    IF ValType(cTitleColor) == 'C' .AND. !is_color(cTitleColor, .T.)
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF

    IF ValType(cBorder) == 'C' .AND. !is_box(cBorder)
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF

    IF !(ValType(cTitle) $ 'C;U')
        throw(ARGUMENT_VALUE_EXCEPTION)
    ELSEIF ValType(cTitle) == 'U' .AND. (ValType(cTitleColor) != 'U' .OR. ValType(cTitleAlign) != 'U')
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF

    IF ValType(cTitleAlign) != 'C'
        cTitleAlign := ALIGN_LEFT
    ELSEIF AScan({ALIGN_LEFT, ALIGN_CENTER, ALIGN_RIGHT}, cTitleAlign) == 0
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF

    cOldColor := SetColor(cColor)
    WOpen(nTop, nLeft, nBottom, nRight)
    WBox(cBorder)

    IF ValType(cTitle) == 'C' .AND. nTop > 0
        DO CASE
            CASE cTitleAlign == ALIGN_LEFT
                IF is_color(cTitleColor)
                    @ -1, 0 SAY Left(cTitle, Min(Len(cTitle), nRight - nLeft)) COLOR cTitleColor
                ELSE
                    @ -1, 0 SAY Left(cTitle, Min(Len(cTitle), nRight - nLeft))
                ENDIF
            CASE cTitleAlign == ALIGN_CENTER
                IF is_color(cTitleColor)
                    @ -1, Max(Int((MaxCol() - Len(cTitle)) / 2), 0) SAY Left(cTitle, Min(Len(cTitle), nRight - nLeft)) COLOR cTitleColor
                ELSE
                    @ -1, Max(Int((MaxCol() - Len(cTitle)) / 2), 0) SAY Left(cTitle, Min(Len(cTitle), nRight - nLeft))
                ENDIF
            CASE cTitleAlign == ALIGN_RIGHT
                IF is_color(cTitleColor)
                    @ -1, Max(MaxCol() - Len(cTitle), 0) SAY Left(cTitle, Min(Len(cTitle), nRight - nLeft)) COLOR cTitleColor
                ELSE
                    @ -1, Max(MaxCol() - Len(cTitle), 0) SAY Left(cTitle, Min(Len(cTitle), nRight - nLeft))
                ENDIF
            OTHERWISE
                throw(RUNTIME_EXCEPTION)
        ENDCASE
    ENDIF

    nSelectedItem := AChoice(0, 0, MaxRow(), MaxCol(), acMenuItems, xSelectable, IF(ValType(cUserFunctionName) == 'C', cUserFunctionName, NIL), nInitialItem)

    WClose()
    WSelect(0)
    WSetShadow(NToColor(nOldShadow))
    SET COLOR TO (cOldColor)
    WSelect(nOldWindow)

RETURN nSelectedItem

FUNCTION display_menu_autosize(nTop, nLeft, acMenuItems, xSelectable, cUserFunctionName, nInitialItem, cColor, cBorder, cTitle, cTitleColor, cTitleAlign)

    LOCAL nBottom
    LOCAL nRight

    assert_type(nTop, 'N')
    assert_type(nLeft, 'N')
    assert_type(acMenuItems, 'C')

    AEval(acMenuItems, {| cElement | assert_type(cElement, 'C')})

    nBottom := nTop + Len(acMenuItems) + 1
    nRight := nLeft + max_of_array(length_array(acMenuItems)) + 1
    
    IF ValType(cTitle) == 'C'
        nRight := Max(nRight, nLeft + Len(cTitle))
    ENDIF

RETURN display_menu(nTop, nLeft, nBottom, nRight, acMenuItems, xSelectable, cUserFunctionName, nInitialItem, cColor, cBorder, cTitle, cTitleColor, cTitleAlign)

FUNCTION display_menu_center(nCenterRow, nCenterCol, nHeight, nWidth, acMenuItems, xSelectable, cUserFunctionName, nInitialItem, cColor, cBorder, cTitle, cTitleColor, cTitleAlign)

    LOCAL nTop
    LOCAL nLeft
    LOCAL nBottom
    LOCAL nRight

    assert_type(nCenterRow, 'N')
    assert_type(nCenterCol, 'N')
    assert_type(nHeight, 'N')
    assert_type(nWidth, 'N')

    nTop := nCenterRow - Int(nHeight / 2)
    nLeft := nCenterCol - Int(nWidth / 2)
    nBottom := nCenterRow + Int((nHeight + 1) / 2)
    nRight := nCenterCol + Int((nWidth + 1) / 2)

RETURN display_menu(nTop, nLeft, nBottom, nRight, acMenuItems, xSelectable, cUserFunctionName, nInitialItem, cColor, cBorder, cTitle, cTitleColor, cTitleAlign)

FUNCTION display_menu_center_autosize(nCenterRow, nCenterCol, acMenuItems, xSelectable, cUserFunctionName, nInitialItem, cColor, cBorder, cTitle, cTitleColor, cTitleAlign)

    LOCAL nHeight
    LOCAL nWidth

    assert_type(acMenuItems, 'A')
    AEval(acMenuItems, {| cElement | assert_type(cElement, 'C')})

    nHeight := Len(acMenuItems) + 1
    nWidth := max_of_array(length_array(acMenuItems)) + 1

    IF ValType(cTitle) == 'C'
        nWidth := Max(nWidth, Len(cTitle) + 1)
    ENDIF

RETURN display_menu_center(nCenterRow, nCenterCol, nHeight, nWidth, acMenuItems, xSelectable, cUserFunctionName, nInitialItem, cColor, cBorder, cTitle, cTitleColor, cTitleAlign)

FUNCTION menu_search_allow_exit(nMode)//, nCurrentElement, nRowPosition)

    LOCAL nReturnMode := AC_CONT    
    LOCAL nKey := LastKey()

    DO CASE
        CASE nMode == AC_EXCEPT
        DO CASE
            CASE nKey == Menu():anKeys[SELECT]
                nReturnMode := AC_SELECT
            CASE nKey == Menu():anKeys[ABORT]
                IF YesNo(Config():get_config('DefaultExit'))
                    nReturnMode := AC_ABORT
                ENDIF
            OTHERWISE
                nReturnMode := AC_GOTO
        ENDCASE
    ENDCASE

RETURN nReturnMode

FUNCTION menu_search_disallow_exit(nMode)//, nCurrentElement, nRowPosition)

    LOCAL nReturnMode := AC_CONT    
    LOCAL nKey := LastKey()

    DO CASE
        CASE nMode == AC_EXCEPT
        DO CASE
            CASE nKey == Menu():anKeys[SELECT]
                nReturnMode := AC_SELECT
            OTHERWISE
                nReturnMode := AC_GOTO
        ENDCASE
    ENDCASE

RETURN nReturnMode

FUNCTION menu_search_allow_exit_move(nMode)//, nCurrentElement, nRowPosition)

    LOCAL nReturnMode := AC_CONT    
    LOCAL nKey := LastKey()

    DO CASE
        CASE nMode == AC_EXCEPT
        DO CASE
            CASE nKey == Menu():anKeys[SELECT]
                nReturnMode := AC_SELECT
            CASE nKey == Menu():anKeys[ABORT]
                IF YesNo(Config():get_config('DefaultExit'))
                    nReturnMode := AC_ABORT
                ENDIF
            CASE nKey == Menu():anKeys[UP]
                WMove(WRow() - 1, WCol())
            CASE nKey == Menu():anKeys[DOWN]
                WMove(WRow() + 1, WCol())
            CASE nKey == Menu():anKeys[LEFT]
                WMove(WRow(), WCol() - 1)
            CASE nKey == Menu():anKeys[RIGHT]
                WMove(WRow(), WCol() + 1)
            CASE nKey == Menu():anKeys[SELECT]
                WCenter(.T.)
            OTHERWISE
                nReturnMode := AC_GOTO
        ENDCASE
    ENDCASE

RETURN nReturnMode

FUNCTION menu_search_disallow_exit_move(nMode)//, nCurrentElement, nRowPosition)

    LOCAL nReturnMode := AC_CONT    
    LOCAL nKey := LastKey()

    DO CASE
        CASE nMode == AC_EXCEPT
        DO CASE
            CASE nKey == Menu():anKeys[SELECT]
                nReturnMode := AC_SELECT
            CASE nKey == Menu():anKeys[UP]
                WMove(WRow() - 1, WCol())
            CASE nKey == Menu():anKeys[DOWN]
                WMove(WRow() + 1, WCol())
            CASE nKey == Menu():anKeys[LEFT]
                WMove(WRow(), WCol() - 1)
            CASE nKey == Menu():anKeys[RIGHT]
                WMove(WRow(), WCol() + 1)
            CASE nKey == Menu():anKeys[CENTER]
                WCenter(.T.)
            OTHERWISE
                nReturnMode := AC_GOTO
        ENDCASE
    ENDCASE

RETURN nReturnMode
