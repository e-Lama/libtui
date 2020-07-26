#include "achoice.ch"
#include "inkey.ch"
#include "hbclass.ch"
#include "button.ch"

#include "align.ch"
#include "functions.ch"
#include "menu.ch"

#include "setup.ch"

CREATE CLASS Menu MODULE FRIENDLY

EXPORTED:

    METHOD keys(anKeys) SETGET
    METHOD key(nPosition, nKey) SETGET
    METHOD user_function_handler(nMode, nCurrent, nPos, oScrollBar, xUserFunctionName)

    METHOD register_function(cFunctionName)
    METHOD unregister_function(cFunctionName)

HIDDEN:

    CLASSVAR __anKeys INIT {K_ENTER, K_ESC, K_ALT_UP, K_ALT_LEFT, K_ALT_DOWN, K_ALT_RIGHT, K_ALT_ENTER}
    CLASSVAR __acRegisteredUserFunctionNames INIT {'menu_search_allow_exit', 'menu_search_disallow_exit', 'menu_search_allow_exit_move', 'menu_search_disallow_exit_move'}

ENDCLASS LOCK

METHOD register_function(cFunctionName)

    LOCAL nIndex := 1
    LOCAL nArraySize := Len(::__acRegisteredUserFunctionNames)
    LOCAL nNilIndex := 0

#ifdef USE_VALIDATORS
    IF ValType(cFunctionName) != 'C' .OR. !is_function_name(cFunctionName)
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF
#endif

    DO WHILE nIndex <= nArraySize
        IF ::__acRegisteredUserFunctionNames[nIndex] == cFunctionName
          RETURN NIL
        ELSEIF ::__acRegisteredUserFunctionNames[nIndex] == NIL
          nNilIndex := nIndex
        ENDIF
    ENDDO

    IF nNilIndex == 0
        AAdd(::__acRegisteredUserFunctionNames, cFunctionName)
    ELSE
        ::__acRegisteredUserFunctionNames[nNilIndex] := cFunctionName
    ENDIF

RETURN NIL

METHOD unregister_function(cFunctionName)

    LOCAL nPosition

#ifdef USE_VALIDATORS
    IF ValType(cFunctionName) != 'C' .OR. !is_function_name(cFunctionName)
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF
#endif
    
    nPosition := AScan(::__acRegisteredUserFunctionNames, cFunctionName)

    IF nPosition != 0
        ADel(::__acRegisteredUserFunctionNames, cFunctionName)
    ENDIF

RETURN NIL

METHOD user_function_handler(nMode, nCurrent, nPos, oScrollBar, xUserFunctionName) CLASS Menu
RETURN Do(xUserFunctionName, nMode, nCurrent, nPos, oScrollBar)

METHOD keys(anKeys) CLASS Menu

    LOCAL anOldKeys := AClone(anKeys)

    IF anKeys != NIL
#ifdef USE_VALIDATORS
        assert_type(anKeys, 'A')
        assert_length(anKeys, Len(::__anKeys))
        AEval(anKeys, {| nKey | assert_type(nKey, 'N')})
#endif
        ::__anKeys := anKeys
    ENDIF

RETURN anOldKeys

METHOD key(nPosition, nKey) CLASS Menu

    LOCAL nOldKey

#ifdef USE_VALIDATORS
    assert_type(nPosition, 'N')
    IF nPosition < 0 .OR. nPosition > Len(::__anKeys) .OR. Int(nPosition) != nPosition
        throw(ARGUMENT_VALUE_EXCEPTION)
    ELSE
        nOldKey := ::__anKeys[nPosition]
    ENDIF
#else
    nOldKey := ::__anKeys[nPosition]
#endif

    IF ValType(nKey) != 'U'
#ifdef USE_VALIDATORS
        assert_type(nKey, 'N')
#endif
        ::__anKeys[nPosition] := nKey
    ENDIF

RETURN nOldKey

FUNCTION display_menu(nTop, nLeft, nBottom, nRight, acMenuItems, xSelectable, xUserFunctionName, nInitialItem, cColor, cBorder, cTitle, cTitleColor, cTitleAlign, lScrollBar, nScrollBarFrom, nScrollBarTo, cScrollBarColor, nScrollBarOrientation, xScrollBarCargo, cScrollBarStyle, bScrollBarSBlock)

    LOCAL nOldWindow := WSelect()
    LOCAL nOldShadow
    LOCAL cOldColor
    LOCAL nSelectedItem
    LOCAL oScrollBar

#ifdef USE_VALIDATORS
    assert_type(nTop, 'N')
    assert_type(nLeft, 'N')
    assert_type(nBottom, 'N')
    assert_type(nRight, 'N')
    assert_type(acMenuItems, 'A')
#endif

    WSelect(0)
    nOldShadow := WSetShadow(-1)

#ifdef USE_VALIDATORS
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
    //Undocumented handling. You can find more about that in the src/rtl/achoice.prg
    ELSEIF Empty(xUserFunctionName) .AND. ValType(xSelectable) $ 'C;B;S'
        xUserFunctionName := xSelectable
        xSelectable := NIL
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

    IF ValType(cBorder) == 'C' .AND. !is_box(hb_translate(cBorder, 'EN', hb_cdpSelect()))
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF

    IF !(ValType(cTitle) $ 'C;U')
        throw(ARGUMENT_VALUE_EXCEPTION)
    ELSEIF ValType(cTitle) == 'U' .AND. (ValType(cTitleColor) != 'U' .OR. ValType(cTitleAlign) != 'U')
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF
#endif

    IF ValType(cTitleAlign) != 'C'
        cTitleAlign := ALIGN_LEFT
#ifdef USE_VALIDATORS
    ELSEIF AScan({ALIGN_LEFT, ALIGN_CENTER, ALIGN_RIGHT}, cTitleAlign) == 0
        throw(ARGUMENT_VALUE_EXCEPTION)
#endif
    ENDIF

    IF ValType(lScrollBar) != 'L'
      lScrollBar := .F.
    ENDIF

    IF lScrollBar
#ifdef USE_VALIDATORS
        IF ValType(cScrollBarColor) == 'C' .AND. !is_color(cTitleColor, .T.)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF

        IF !(ValType(nScrollBarFrom) $ 'N;U')
          throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF

        IF !(ValType(nScrollBarTo) $ 'N;U')
          throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF

        IF ValType(nScrollBarOrientation) == 'N' .AND. AScan({SCROLLBAR_LEFT, SCROLLBAR_UP, SCROLLBAR_RIGHT, SCROLLBAR_DOWN}, nScrollBarOrientation) == 0
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF

        IF ValType(nScrollBarOrientation) == 'N'
            IF ValType(nScrollBarFrom) == 'N' 
                IF nScrollBarOrientation == SCROLLBAR_LEFT .OR. nScrollBarOrientation == SCROLLBAR_RIGHT
                    IF nScrollBarFrom >= nBottom .OR. nScrollBarFrom < nTop
                        throw(ARGUMENT_VALUE_EXCEPTION)
                    ENDIF
                ELSE
                    IF nScrollBarFrom >= nRight .OR. nScrollBarFrom < nLeft
                        throw(ARGUMENT_VALUE_EXCEPTION)
                    ENDIF
                ENDIF
            ENDIF

            IF ValType(nScrollBarTo) == 'N' 
                IF nScrollBarOrientation == SCROLLBAR_LEFT .OR. nScrollBarOrientation == SCROLLBAR_RIGHT
                    IF nScrollBarTo <= nTop .OR. nScrollBarTo > nBottom
                        throw(ARGUMENT_VALUE_EXCEPTION)
                    ENDIF
                ELSE
                    IF nScrollBarTo >= nLeft .OR. nScrollBarTo > nRight
                        throw(ARGUMENT_VALUE_EXCEPTION)
                    ENDIF
                ENDIF
            ENDIF
        ENDIF

        IF !(ValType(cScrollBarStyle)) $ 'C;U'
            throw(ARGUMENT_VALUE_EXCEPTION)
        ELSEIF ValType(cScrollBarStyle) == 'C' .AND. !is_scrollbar_style(cScrollBarStyle)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF

        IF !(ValType(bScrollBarSBlock) $ 'B;U')
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif
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

    IF lScrollBar

      IF ValType(nScrollBarOrientation) != 'N'
          nScrollBarOrientation := SCROLLBAR_RIGHT      
      ENDIF

      DO CASE
          CASE nScrollBarOrientation == SCROLLBAR_LEFT .OR. nScrollBarOrientation == SCROLLBAR_RIGHT
              IF ValType(nScrollBarFrom) != 'N'
                  nScrollBarFrom := 0
              ENDIF

              IF ValType(nScrollBarTo) != 'N'
                  nScrollBarTo := MaxRow()
              ENDIF
          CASE nScrollBarOrientation == SCROLLBAR_UP .OR. nScrollBarOrientation == SCROLLBAR_DOWN
              IF ValType(nScrollBarFrom) != 'N'
                  nScrollBarFrom := 0
              ENDIF

              IF ValType(nScrollBarTo) != 'N'
                  nScrollBarTo := MaxCol()
              ENDIF
      ENDCASE

      DO CASE
          CASE nScrollBarOrientation == SCROLLBAR_LEFT 
              oScrollBar := ScrollBar(nScrollBarFrom, nScrollBarTo, 0, bScrollBarSBlock, SCROLL_VERTICAL)
          CASE nScrollBarOrientation == SCROLLBAR_RIGHT
              oScrollBar := ScrollBar(nScrollBarFrom, nScrollBarTo, MaxCol(), bScrollBarSBlock, SCROLL_VERTICAL)
          CASE nScrollBarOrientation == SCROLLBAR_UP 
              oScrollBar := ScrollBar(nScrollBarFrom, nScrollBarTo, 0, bScrollBarSBlock, SCROLL_HORIZONTAL)
          CASE nScrollBarOrientation == SCROLLBAR_DOWN
              oScrollBar := ScrollBar(nScrollBarFrom, nScrollBarTo, MaxRow(), bScrollBarSBlock, SCROLL_HORIZONTAL)
      ENDCASE

      oScrollBar:total := Len(acMenuItems)
      oScrollBar:cargo := xScrollBarCargo

      IF ValType(cScrollBarColor) == 'C'
          oScrollBar:colorSpec := cScrollBarColor
      ENDIF

      IF ValType(cScrollBarStyle) == 'C'
          oScrollBar:style := cScrollBarStyle
      ENDIF

      oScrollBar:display()
    ENDIF


    IF ValType(xUserFunctionName) == 'C'
        IF AScan(Menu():__acRegisteredUserFunctionNames, xUserFunctionName) == 0
            nSelectedItem := AChoice(IF(nScrollBarOrientation == SCROLLBAR_UP, 1, 0), IF(nScrollBarOrientation == SCROLLBAR_LEFT, 1, 0), IF(nScrollBarOrientation == SCROLLBAR_DOWN, MaxRow() - 1, MaxRow()), IF(nScrollBarOrientation == SCROLLBAR_RIGHT, MaxCol() - 1, MaxCol()), acMenuItems, xSelectable, xUserFunctionName, nInitialItem)
        ELSE
            nSelectedItem := AChoice(IF(nScrollBarOrientation == SCROLLBAR_UP, 1, 0), IF(nScrollBarOrientation == SCROLLBAR_LEFT, 1, 0), IF(nScrollBarOrientation == SCROLLBAR_DOWN, MaxRow() - 1, MaxRow()), IF(nScrollBarOrientation == SCROLLBAR_RIGHT, MaxCol() - 1, MaxCol()), acMenuItems, xSelectable, {| nMode, nCurrent, nPos | Menu():user_function_handler(nMode, nCurrent, nPos, oScrollBar, xUserFunctionName)}, nInitialItem)
        ENDIF
    ELSE
        nSelectedItem := AChoice(IF(nScrollBarOrientation == SCROLLBAR_UP, 1, 0), IF(nScrollBarOrientation == SCROLLBAR_LEFT, 1, 0), IF(nScrollBarOrientation == SCROLLBAR_DOWN, MaxRow() - 1, MaxRow()), IF(nScrollBarOrientation == SCROLLBAR_RIGHT, MaxCol() - 1, MaxCol()), acMenuItems, xSelectable, xUserFunctionName, nInitialItem)
    ENDIF

    WClose()
    WSelect(0)
    WSetShadow(NToColor(nOldShadow))
    SET COLOR TO (cOldColor)
    WSelect(nOldWindow)

RETURN nSelectedItem

FUNCTION display_menu_autosize(nTop, nLeft, acMenuItems, xSelectable, xUserFunctionName, nInitialItem, cColor, cBorder, cTitle, cTitleColor, cTitleAlign, lScrollBar, nScrollBarFrom, nScrollBarTo, cScrollBarColor, nScrollBarOrientation, xScrollBarCargo, cScrollBarStyle, bScrollBarSBlock)

    LOCAL nBottom
    LOCAL nRight

#ifdef USE_VALIDATORS
    assert_type(nTop, 'N')
    assert_type(nLeft, 'N')
    assert_type(acMenuItems, 'C')

    AEval(acMenuItems, {| cElement | assert_type(cElement, 'C')})
#endif

    nBottom := nTop + Len(acMenuItems) + 1
    nRight := nLeft + max_of_array(length_array(acMenuItems)) + 1
    
    IF ValType(cTitle) == 'C'
        nRight := Max(nRight, nLeft + Len(cTitle))
    ENDIF

RETURN display_menu(nTop, nLeft, nBottom, nRight, acMenuItems, xSelectable, xUserFunctionName, nInitialItem, cColor, cBorder, cTitle, cTitleColor, cTitleAlign, lScrollBar, nScrollBarFrom, nScrollBarTo, cScrollBarColor, nScrollBarOrientation, xScrollBarCargo, cScrollBarStyle, bScrollBarSBlock)


FUNCTION display_menu_center(nCenterRow, nCenterCol, nHeight, nWidth, acMenuItems, xSelectable, xUserFunctionName, nInitialItem, cColor, cBorder, cTitle, cTitleColor, cTitleAlign, lScrollBar, nScrollBarFrom, nScrollBarTo, cScrollBarColor, nScrollBarOrientation, xScrollBarCargo, cScrollBarStyle, bScrollBarSBlock)

    LOCAL nTop
    LOCAL nLeft
    LOCAL nBottom
    LOCAL nRight

#ifdef USE_VALIDATORS
    assert_type(nCenterRow, 'N')
    assert_type(nCenterCol, 'N')
    assert_type(nHeight, 'N')
    assert_type(nWidth, 'N')
#endif

    nTop := nCenterRow - Int(nHeight / 2)
    nLeft := nCenterCol - Int(nWidth / 2)
    nBottom := nCenterRow + Int((nHeight + 1) / 2)
    nRight := nCenterCol + Int((nWidth + 1) / 2)

RETURN display_menu(nTop, nLeft, nBottom, nRight, acMenuItems, xSelectable, xUserFunctionName, nInitialItem, cColor, cBorder, cTitle, cTitleColor, cTitleAlign, lScrollBar, nScrollBarFrom, nScrollBarTo, cScrollBarColor, nScrollBarOrientation, xScrollBarCargo, cScrollBarStyle, bScrollBarSBlock)

FUNCTION display_menu_center_autosize(nCenterRow, nCenterCol, acMenuItems, xSelectable, xUserFunctionName, nInitialItem, cColor, cBorder, cTitle, cTitleColor, cTitleAlign, lScrollBar, nScrollBarFrom, nScrollBarTo, cScrollBarColor, nScrollBarOrientation, xScrollBarCargo, cScrollBarStyle, bScrollBarSBlock)

    LOCAL nHeight
    LOCAL nWidth

#ifdef USE_VALIDATORS
    assert_type(acMenuItems, 'A')
    AEval(acMenuItems, {| cElement | assert_type(cElement, 'C')})
#endif

    nHeight := Len(acMenuItems) + 1
    nWidth := max_of_array(length_array(acMenuItems)) + 1

    IF ValType(cTitle) == 'C'
        nWidth := Max(nWidth, Len(cTitle) + 1)
    ENDIF

RETURN display_menu_center(nCenterRow, nCenterCol, nHeight, nWidth, acMenuItems, xSelectable, xUserFunctionName, nInitialItem, cColor, cBorder, cTitle, cTitleColor, cTitleAlign, lScrollBar, nScrollBarFrom, nScrollBarTo, cScrollBarColor, nScrollBarOrientation, xScrollBarCargo, cScrollBarStyle, bScrollBarSBlock)

#pragma ENABLEWARNINGS = Off //because of unused variables
FUNCTION menu_search_allow_exit(nMode, nCurrentElement, nRowPosition, oScrollBar)

    LOCAL nReturnMode := AC_CONT    
    LOCAL nKey := LastKey()

    DO CASE
        CASE nMode == AC_EXCEPT
            DO CASE
                CASE nKey == Menu():__anKeys[MENU_SELECT]
                    nReturnMode := AC_SELECT
                CASE nKey == Menu():__anKeys[MENU_ABORT]
                    IF YesNo(Config():get_config('DefaultExit'))
                        nReturnMode := AC_ABORT
                    ENDIF
                OTHERWISE
                    nReturnMode := AC_GOTO
            ENDCASE
    ENDCASE

    IF ValType(oScrollBar) == 'O'
        nNewPosition := oScrollBar:current
      
        DO CASE
            CASE nKey == K_CTRL_PGUP
              nNewPosition := 1
            CASE nKey == K_CTRL_PGDN
              nNewPosition := oScrollBar:total
            CASE nKey == K_CTRL_HOME
              nNewPosition := nNewPosition - oScrollBar:barLength - 1
            CASE nKey == K_CTRL_END
              nNewPosition := nNewPosition + oScrollBar:barLength + 1
            CASE nKey == K_PGUP
              nNewPosition := nNewPosition - oScrollBar:barLength - 1
            CASE nKey == K_PGDN
              nNewPosition := nNewPosition + oScrollBar:barLength + 1
            CASE nKey == K_UP
              --nNewPosition
            CASE nKey == K_DOWN
              ++nNewPosition
        ENDCASE

        oScrollBar:current := nNewPosition
        oScrollBar:update()
    ENDIF

RETURN nReturnMode
#pragma ENABLEWARNINGS = On

#pragma ENABLEWARNINGS = Off //because of unused variables
FUNCTION menu_search_disallow_exit(nMode, nCurrentElement, nRowPosition, oScrollBar)

    LOCAL nReturnMode := AC_CONT    
    LOCAL nKey := LastKey()

    DO CASE
        CASE nMode == AC_EXCEPT
            DO CASE
                CASE nKey == Menu():__anKeys[MENU_SELECT]
                    nReturnMode := AC_SELECT
                OTHERWISE
                    nReturnMode := AC_GOTO
            ENDCASE
    ENDCASE

    IF ValType(oScrollBar) == 'O'
        nNewPosition := oScrollBar:current
      
        DO CASE
            CASE nKey == K_CTRL_PGUP
              nNewPosition := 1
            CASE nKey == K_CTRL_PGDN
              nNewPosition := oScrollBar:total
            CASE nKey == K_CTRL_HOME
              nNewPosition := nNewPosition - oScrollBar:barLength - 1
            CASE nKey == K_CTRL_END
              nNewPosition := nNewPosition + oScrollBar:barLength + 1
            CASE nKey == K_PGUP
              nNewPosition := nNewPosition - oScrollBar:barLength - 1
            CASE nKey == K_PGDN
              nNewPosition := nNewPosition + oScrollBar:barLength + 1
            CASE nKey == K_UP
              --nNewPosition
            CASE nKey == K_DOWN
              ++nNewPosition
        ENDCASE

        oScrollBar:current := nNewPosition
        oScrollBar:update()
    ENDIF

RETURN nReturnMode
#pragma ENABLEWARNINGS = On

#pragma ENABLEWARNINGS = Off //because of unused variables
FUNCTION menu_search_allow_exit_move(nMode, nCurrentElement, nRowPosition, oScrollBar)

    LOCAL nReturnMode := AC_CONT    
    LOCAL nKey := LastKey()
    LOCAL nNewPosition

    DO CASE
        CASE nMode == AC_EXCEPT
            DO CASE
                CASE nKey == Menu():__anKeys[MENU_SELECT]
                    nReturnMode := AC_SELECT
                CASE nKey == Menu():__anKeys[MENU_ABORT]
                    IF YesNo(Config():get_config('DefaultExit'))
                        nReturnMode := AC_ABORT
                    ENDIF
                CASE nKey == Menu():__anKeys[MENU_UP]
                    WMove(WRow() - 1, WCol())
                CASE nKey == Menu():__anKeys[MENU_DOWN]
                    WMove(WRow() + 1, WCol())
                CASE nKey == Menu():__anKeys[MENU_LEFT]
                    WMove(WRow(), WCol() - 1)
                CASE nKey == Menu():__anKeys[MENU_RIGHT]
                    WMove(WRow(), WCol() + 1)
                CASE nKey == Menu():__anKeys[MENU_SELECT]
                    WCenter(.T.)
                OTHERWISE
                    nReturnMode := AC_GOTO
            ENDCASE
    ENDCASE

    IF ValType(oScrollBar) == 'O'
        nNewPosition := oScrollBar:current
      
        DO CASE
            CASE nKey == K_CTRL_PGUP
              nNewPosition := 1
            CASE nKey == K_CTRL_PGDN
              nNewPosition := oScrollBar:total
            CASE nKey == K_CTRL_HOME
              nNewPosition := nNewPosition - oScrollBar:barLength - 1
            CASE nKey == K_CTRL_END
              nNewPosition := nNewPosition + oScrollBar:barLength + 1
            CASE nKey == K_PGUP
              nNewPosition := nNewPosition - oScrollBar:barLength - 1
            CASE nKey == K_PGDN
              nNewPosition := nNewPosition + oScrollBar:barLength + 1
            CASE nKey == K_UP
              --nNewPosition
            CASE nKey == K_DOWN
              ++nNewPosition
        ENDCASE

        oScrollBar:current := nNewPosition
        oScrollBar:update()
    ENDIF

RETURN nReturnMode
#pragma ENABLEWARNINGS = On

#pragma ENABLEWARNINGS = Off //because of unused variables
FUNCTION menu_search_disallow_exit_move(nMode, nCurrentElement, nRowPosition)

    LOCAL nReturnMode := AC_CONT    
    LOCAL nKey := LastKey()

    DO CASE
        CASE nMode == AC_EXCEPT
            DO CASE
                CASE nKey == Menu():__anKeys[MENU_SELECT]
                    nReturnMode := AC_SELECT
                CASE nKey == Menu():__anKeys[MENU_UP]
                    WMove(WRow() - 1, WCol())
                CASE nKey == Menu():__anKeys[MENU_DOWN]
                    WMove(WRow() + 1, WCol())
                CASE nKey == Menu():__anKeys[MENU_LEFT]
                    WMove(WRow(), WCol() - 1)
                CASE nKey == Menu():__anKeys[MENU_RIGHT]
                    WMove(WRow(), WCol() + 1)
                CASE nKey == Menu():__anKeys[MENU_CENTER]
                    WCenter(.T.)
                OTHERWISE
                    nReturnMode := AC_GOTO
            ENDCASE
    ENDCASE

    IF ValType(oScrollBar) == 'O'
        nNewPosition := oScrollBar:current
      
        DO CASE
            CASE nKey == K_CTRL_PGUP
              nNewPosition := 1
            CASE nKey == K_CTRL_PGDN
              nNewPosition := oScrollBar:total
            CASE nKey == K_CTRL_HOME
              nNewPosition := nNewPosition - oScrollBar:barLength - 1
            CASE nKey == K_CTRL_END
              nNewPosition := nNewPosition + oScrollBar:barLength + 1
            CASE nKey == K_PGUP
              nNewPosition := nNewPosition - oScrollBar:barLength - 1
            CASE nKey == K_PGDN
              nNewPosition := nNewPosition + oScrollBar:barLength + 1
            CASE nKey == K_UP
              --nNewPosition
            CASE nKey == K_DOWN
              ++nNewPosition
        ENDCASE

        oScrollBar:current := nNewPosition
        oScrollBar:update()
    ENDIF

RETURN nReturnMode
