#include "achoice.ch"
#include "inkey.ch"
#include "button.ch"

#include "align.ch"
#include "functions.ch"
#include "menu.ch"

#include "setup.ch"

STATIC FUNCTION user_function_handler(nMode, nCurrent, nPos, oScrollBar, acMenuItems, xSelectable;
                                      , lIsBorder, axKeys, lMousable, nScrollBarOrientation;
                                      , xUserFunctionName)
RETURN Do(xUserFunctionName, nMode, nCurrent, nPos, oScrollBar, acMenuItems, xSelectable, lIsBorder, axKeys, lMousable, nScrollBarOrientation)

FUNCTION display_menu(nTop, nLeft, nBottom, nRight, acMenuItems, xSelectable, xUserFunctionName;
                      , nInitialItem, cColor, cBorder, cTitle, cTitleColor, cTitleAlign, axKeys;
                      , lMousable, lScrollBar, nScrollBarFrom, nScrollBarTo, cScrollBarColor;
                      , nScrollBarOrientation, xScrollBarCargo, cScrollBarStyle, bScrollBarSBlock;
                      , cItemAlign;
                     )

    LOCAL lIsBorder := .F.
    LOCAL nSelectedItem
    LOCAL oScrollBar
    LOCAL nOldWindow := WSelect()
    LOCAL nOldShadow
    LOCAL cOldColor

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
    //Undocumented handling. You can find more about this in the src/rtl/achoice.prg
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

    IF ValType(axKeys) != 'A'
        axKeys := MENU_BASIC_KEYS
#ifdef USE_VALIDATORS
        IF Len(axKeys) != Len(MENU_BASIC_KEYS)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ELSE
            AEval(axKeys, {| xKey | IF(ValType(xKey) == 'A', AEval(xKey, {| nKey | assert_type(nKey, 'N')}), assert_type(xKey, 'N'))})
        ENDIF
#endif
    ENDIF

    IF ValType(lMousable) != 'L'
        lMousable := .T.
    ENDIF
   
    IF ValType(lScrollBar) != 'L'
      lScrollBar := .F.
    ENDIF

    IF lScrollBar
    
        hb_Default(@nScrollBarOrientation, SCROLLBAR_RIGHT)

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

    IF ValType(cItemAlign) != 'C'
        cItemAlign := ALIGN_LEFT
#ifdef USE_VALIDATORS
    ELSEIF AScan({ALIGN_LEFT, ALIGN_CENTER, ALIGN_RIGHT}, cItemAlign) == 0
        throw(ARGUMENT_VALUE_EXCEPTION)
#endif
    ENDIF

    cOldColor := SetColor(cColor)
    WOpen(nTop, nLeft, nBottom, nRight)

    IF ValType(cBorder) == 'C'
        WBox(cBorder)
        lIsBorder := .T.
    ENDIF

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

    IF cItemAlign == ALIGN_CENTER
        IF lScrollBar .AND. (nScrollBarOrientation == SCROLLBAR_RIGHT .OR. nScrollBarOrientation == SCROLLBAR_LEFT)
            AEval(acMenuItems, {| cItem, nKey | acMenuItems[nKey] := PadC(cItem, MaxCol())})
        ELSE
            AEval(acMenuItems, {| cItem, nKey | acMenuItems[nKey] := PadC(cItem, MaxCol() + 1)})
        ENDIF
    ELSEIF cItemAlign == ALIGN_RIGHT
        IF lScrollBar .AND. (nScrollBarOrientation == SCROLLBAR_RIGHT .OR. nScrollBarOrientation == SCROLLBAR_LEFT)
            AEval(acMenuItems, {| cItem, nKey | acMenuItems[nKey] := PadL(cItem, MaxCol())})
        ELSE
            AEval(acMenuItems, {| cItem, nKey | acMenuItems[nKey] := PadL(cItem, MaxCol() + 1)})
        ENDIF
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
        oScrollBar:current := IF(ValType(nInitialItem) == 'N', nInitialItem, 1)

        IF ValType(cScrollBarColor) == 'C'
            oScrollBar:colorSpec := cScrollBarColor
        ENDIF

        IF ValType(cScrollBarStyle) == 'C'
            oScrollBar:style := cScrollBarStyle
        ELSEIF nScrollBarOrientation == SCROLLBAR_UP .OR. nScrollBarOrientation == SCROLLBAR_DOWN
            oScrollBar:style := Right(oScrollBar:style, 1) + SubStr(oScrollBar:style, 2, 2) + Left(oScrollBar:style, 1)
        ENDIF

        oScrollBar:display()
    ENDIF

    IF ValType(xUserFunctionName) == 'C'
        nSelectedItem := AChoice(IF(nScrollBarOrientation == SCROLLBAR_UP, 1, 0);
                                , IF(nScrollBarOrientation == SCROLLBAR_LEFT, 1, 0);
                                , IF(nScrollBarOrientation == SCROLLBAR_DOWN, MaxRow() - 1, MaxRow());
                                , IF(nScrollBarOrientation == SCROLLBAR_RIGHT, MaxCol() - 1, MaxCol());
                                , acMenuItems, xSelectable, {| nMode, nCurrent, nPos |;
                                      user_function_handler(nMode, nCurrent, nPos, oScrollBar, acMenuItems;
                                                                   , xSelectable, lIsBorder, axKeys, lMousable;
                                                                   , nScrollBarOrientation, xUserFunctionName;
                                                                  );
                                                            };
                                , nInitialItem;
                                )
    ELSE
        nSelectedItem := AChoice(IF(nScrollBarOrientation == SCROLLBAR_UP, 1, 0);
                                , IF(nScrollBarOrientation == SCROLLBAR_LEFT, 1, 0);
                                , IF(nScrollBarOrientation == SCROLLBAR_DOWN, MaxRow() - 1, MaxRow());
                                , IF(nScrollBarOrientation == SCROLLBAR_RIGHT, MaxCol() - 1, MaxCol());
                                , acMenuItems, xSelectable, xUserFunctionName, nInitialItem;
                                )
    ENDIF

    WClose()
    WSelect(0)
    WSetShadow(NToColor(nOldShadow))
    SET COLOR TO (cOldColor)
    WSelect(nOldWindow)

RETURN nSelectedItem

FUNCTION display_menu_autosize(nTop, nLeft, acMenuItems, xSelectable, xUserFunctionName, nInitialItem;
                               , cColor, cBorder, cTitle, cTitleColor, cTitleAlign, axKeys, lMousable;
                               , lScrollBar, nScrollBarFrom, nScrollBarTo, cScrollBarColor;
                               , nScrollBarOrientation, xScrollBarCargo, cScrollBarStyle, bScrollBarSBlock;
                               , cItemAlign;
                              )

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

RETURN display_menu(nTop, nLeft, nBottom, nRight, acMenuItems, xSelectable, xUserFunctionName, nInitialItem, cColor, cBorder, cTitle, cTitleColor, cTitleAlign, axKeys, lMousable, lScrollBar, nScrollBarFrom, nScrollBarTo, cScrollBarColor, nScrollBarOrientation, xScrollBarCargo, cScrollBarStyle, bScrollBarSBlock, cItemAlign)

FUNCTION display_menu_center(nCenterRow, nCenterCol, nHeight, nWidth, acMenuItems, xSelectable;
                             , xUserFunctionName, nInitialItem, cColor, cBorder, cTitle, cTitleColor;
                             , cTitleAlign, axKeys, lMousable, lScrollBar, nScrollBarFrom, nScrollBarTo;
                             , cScrollBarColor, nScrollBarOrientation, xScrollBarCargo, cScrollBarStyle;
                             , bScrollBarSBlock, cItemAlign;
                            )

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

RETURN display_menu(nTop, nLeft, nBottom, nRight, acMenuItems, xSelectable, xUserFunctionName, nInitialItem, cColor, cBorder, cTitle, cTitleColor, cTitleAlign, axKeys, lMousable, lScrollBar, nScrollBarFrom, nScrollBarTo, cScrollBarColor, nScrollBarOrientation, xScrollBarCargo, cScrollBarStyle, bScrollBarSBlock, cItemAlign)

FUNCTION display_menu_center_autosize(nCenterRow, nCenterCol, acMenuItems, xSelectable, xUserFunctionName;
                                      , nInitialItem, cColor, cBorder, cTitle, cTitleColor, cTitleAlign;
                                      , axKeys, lMousable, lScrollBar, nScrollBarFrom, nScrollBarTo;
                                      , cScrollBarColor, nScrollBarOrientation, xScrollBarCargo;
                                      , cScrollBarStyle, bScrollBarSBlock, cItemAlign;
                                     )

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

RETURN display_menu_center(nCenterRow, nCenterCol, nHeight, nWidth, acMenuItems, xSelectable, xUserFunctionName, nInitialItem, cColor, cBorder, cTitle, cTitleColor, cTitleAlign, axKeys, lMousable, lScrollBar, nScrollBarFrom, nScrollBarTo, cScrollBarColor, nScrollBarOrientation, xScrollBarCargo, cScrollBarStyle, bScrollBarSBlock, cItemAlign)

STATIC FUNCTION was_menu_key(nKey, xKey)

  LOCAL lWasKey := .F.

  IF ValType(xKey) == 'N' .AND. nKey == xKey
    lWasKey := .T.
  ELSEIF AScan(xKey, nKey) != 0
    lWasKey := .T.
  ENDIF
    
RETURN lWasKey

#pragma ENABLEWARNINGS = Off //because of unused variables
FUNCTION menu_search_allow_exit(nMode, nCurrentElement, nRowPosition, oScrollBar, acMenuItems, xSelectable;
                                , lIsBorder, axKeys, lMousable, nScrollBarOrientation;
                               )

    LOCAL nReturnMode := AC_CONT    
    LOCAL nKey := LastKey()
    LOCAL nMouseRow := MRow()
    LOCAL nMouseCol := MCol()
    LOCAL lScrollBar := (ValType(oScrollBar) == 'O')
    LOCAL nHitStatus := IF(lScrollBar .AND. lMousable, oScrollBar:hitTest(nMouseRow, nMouseCol), HTNOWHERE)
    LOCAL nBorderShift := cast(lIsBorder, 'N')
    LOCAL nNewPosition

    DO CASE
        CASE nMode == AC_EXCEPT
            DO CASE
                CASE was_menu_key(nKey, axKeys[MENU_SELECT])
                    nReturnMode := AC_SELECT
                CASE was_menu_key(nKey, axKeys[MENU_ABORT])
                    IF YesNo(Config():get_config('DefaultExit'))
                        nReturnMode := AC_ABORT
                    ENDIF
                OTHERWISE
                    nReturnMode := AC_GOTO
            ENDCASE
        CASE nMode == AC_IDLE .AND. lScrollBar .AND. lMousable .AND. was_menu_key(nKey, axKeys[MENU_LBUTTONUP])
            DO CASE
                CASE nHitStatus == HTSCROLLUNITDEC
                    KEYBOARD Chr(K_UP)
                CASE nHitStatus == HTSCROLLUNITINC
                    KEYBOARD Chr(K_DOWN)
                CASE nHitStatus == HTSCROLLBLOCKDEC
                    move_thumb_up(oScrollBar, nCurrentElement, xSelectable, acMenuItems)
                CASE nHitStatus == HTSCROLLBLOCKINC
                    move_thumb_down(oScrollBar, nCurrentElement, xSelectable, acMenuItems)
                CASE nHitStatus == HTSCROLLTHUMBDRAG
            ENDCASE
    ENDCASE

    IF lScrollBar
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
                nNewPosition := nCurrentElement
            CASE nKey == K_DOWN
                nNewPosition := nCurrentElement
            CASE nKey == K_LBUTTONUP
                nNewPosition := nCurrentElement
            CASE nKey == K_MWBACKWARD
                nNewPosition := nCurrentElement
            CASE nKey == K_MWFORWARD
                nNewPosition := nCurrentElement
        ENDCASE

        oScrollBar:current := nNewPosition
        oScrollBar:update()
    ENDIF

RETURN nReturnMode
#pragma ENABLEWARNINGS = On

#pragma ENABLEWARNINGS = Off //because of unused variables
FUNCTION menu_search_disallow_exit(nMode, nCurrentElement, nRowPosition, oScrollBar, acMenuItems, xSelectable;
                                   , lIsBorder, axKeys, lMousable, nScrollBarOrientation;
                                  )

    LOCAL nReturnMode := AC_CONT    
    LOCAL nKey := LastKey()
    LOCAL nMouseRow := MRow()
    LOCAL nMouseCol := MCol()
    LOCAL lScrollBar := (ValType(oScrollBar) == 'O')
    LOCAL nHitStatus := IF(lScrollBar .AND. lMousable, oScrollBar:hitTest(nMouseRow, nMouseCol), HTNOWHERE)
    LOCAL nBorderShift := cast(lIsBorder, 'N')
    LOCAL nNewPosition

    DO CASE
        CASE nMode == AC_EXCEPT
            DO CASE
                CASE was_menu_key(nKey, axKeys[MENU_SELECT])
                    nReturnMode := AC_SELECT
                OTHERWISE
                    nReturnMode := AC_GOTO
            ENDCASE
        CASE nMode == AC_IDLE .AND. lScrollBar .AND. lMousable .AND. was_menu_key(nKey, axKeys[MENU_LBUTTONUP])
            DO CASE
                CASE nHitStatus == HTSCROLLUNITDEC
                    KEYBOARD Chr(K_UP)
                CASE nHitStatus == HTSCROLLUNITINC
                    KEYBOARD Chr(K_DOWN)
                CASE nHitStatus == HTSCROLLBLOCKDEC
                    move_thumb_up(oScrollBar, nCurrentElement, xSelectable, acMenuItems)
                CASE nHitStatus == HTSCROLLBLOCKINC
                    move_thumb_down(oScrollBar, nCurrentElement, xSelectable, acMenuItems)
                CASE nHitStatus == HTSCROLLTHUMBDRAG
            ENDCASE
    ENDCASE

    IF lScrollBar
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
              nNewPosition := nCurrentElement
            CASE nKey == K_DOWN
              nNewPosition := nCurrentElement
            CASE nKey == K_LBUTTONUP
              nNewPosition := nCurrentElement
            CASE nKey == K_MWBACKWARD
              nNewPosition := nCurrentElement
            CASE nKey == K_MWFORWARD
              nNewPosition := nCurrentElement
        ENDCASE

        oScrollBar:current := nNewPosition
        oScrollBar:update()
    ENDIF

RETURN nReturnMode
#pragma ENABLEWARNINGS = On

#pragma ENABLEWARNINGS = Off //because of unused variables

STATIC PROCEDURE move_thumb_up(oScrollBar, nCurrentElement, xSelectable, acMenuItems)

    LOCAL alSelectable := IF(ValType(xSelectable) == 'A', xSelectable, AFill(Array(Len(acMenuItems)), xSelectable))
    LOCAL nMousePosition := IF(oScrollBar:orient == SCROLL_VERTICAL, MRow(), MCol())
    LOCAL nMoveUpOrRight := 0
    LOCAL nOldCurrentPosition := oScrollBar:current

    DispBegin()

    DO WHILE oScrollBar:thumbpos > nMousePosition
        --oScrollBar:current

        IF oScrollBar:current > 0 .AND. !alSelectable[oScrollBar:current]
            LOOP
        ENDIF

        oScrollBar:update()
        ++nMoveUpOrRight
    ENDDO

    oScrollBar:current := nOldCurrentPosition
    oScrollBar:update()

    KEYBOARD Replicate(Chr(K_UP), nMoveUpOrRight)

    DispEnd()

RETURN

STATIC PROCEDURE move_thumb_down(oScrollBar, nCurrentElement, xSelectable, acMenuItems)

    LOCAL alSelectable := IF(ValType(xSelectable) == 'A', xSelectable, AFill(Array(Len(acMenuItems)), xSelectable))
    LOCAL nMousePosition := IF(oScrollBar:orient == SCROLL_VERTICAL, MRow(), MCol())
    LOCAL nMoveDownOrLeft := 0
    LOCAL nOldCurrentPosition := oScrollBar:current

    DispBegin()

    DO WHILE oScrollBar:thumbpos < nMousePosition
        ++oScrollBar:current

        IF oScrollBar:current < Len(acMenuItems) .AND. !alSelectable[oScrollBar:current]
            LOOP
        ENDIF

        oScrollBar:update()
        ++nMoveDownOrLeft
    ENDDO

    oScrollBar:current := nOldCurrentPosition
    oScrollBar:update()

    KEYBOARD Replicate(Chr(K_DOWN), nMoveDownOrLeft)

    DispEnd()

RETURN

FUNCTION menu_search_allow_exit_move(nMode, nCurrentElement, nRowPosition, oScrollBar;
                                     , acMenuItems, xSelectable, lIsBorder, axKeys;
                                     , lMousable, nScrollBarOrientation;
                                    )

    LOCAL nReturnMode := AC_CONT    
    LOCAL nKey := LastKey()
    LOCAL nMouseRow := MRow()
    LOCAL nMouseCol := MCol()
    LOCAL lScrollBar := (ValType(oScrollBar) == 'O')
    LOCAL nHitStatus := IF(lScrollBar .AND. lMousable, oScrollBar:hitTest(nMouseRow, nMouseCol), HTNOWHERE)
    LOCAL nBorderShift := cast(lIsBorder, 'N')
    LOCAL nNewPosition

    DO CASE
        CASE nMode == AC_EXCEPT
            DO CASE
                CASE was_menu_key(nKey, axKeys[MENU_SELECT])
                    nReturnMode := AC_SELECT
                CASE was_menu_key(nKey, axKeys[MENU_ABORT])
                    IF YesNo(Config():get_config('DefaultExit'))
                        nReturnMode := AC_ABORT
                    ENDIF
                CASE was_menu_key(nKey, axKeys[MENU_UP])
                    WMove(WRow() - 1, WCol())
                CASE was_menu_key(nKey, axKeys[MENU_DOWN])
                    WMove(WRow() + 1, WCol())
                CASE was_menu_key(nKey, axKeys[MENU_LEFT])
                    WMove(WRow(), WCol() - 1)
                CASE was_menu_key(nKey, axKeys[MENU_RIGHT])
                    WMove(WRow(), WCol() + 1)
                CASE was_menu_key(nKey, axKeys[MENU_CENTER])
                    WCenter(.T.)
                OTHERWISE
                    nReturnMode := AC_GOTO
            ENDCASE
        CASE nMode == AC_IDLE .AND. lScrollBar .AND. lMousable .AND. was_menu_key(nKey, axKeys[MENU_LBUTTONUP])
            DO CASE
                CASE nHitStatus == HTSCROLLUNITDEC
                    KEYBOARD Chr(K_UP)
                CASE nHitStatus == HTSCROLLUNITINC
                    KEYBOARD Chr(K_DOWN)
                CASE nHitStatus == HTSCROLLBLOCKDEC
                    move_thumb_up(oScrollBar, nCurrentElement, xSelectable, acMenuItems)
                CASE nHitStatus == HTSCROLLBLOCKINC
                    move_thumb_down(oScrollBar, nCurrentElement, xSelectable, acMenuItems)
                CASE nHitStatus == HTSCROLLTHUMBDRAG
            ENDCASE
    ENDCASE

    IF nHitStatus == HTNOWHERE .AND. was_menu_key(nKey, axKeys[MENU_LBUTTONDOWN]) .AND. lMousable;
       .AND. nMouseRow >= -1 * nBorderShift .AND. nMouseCol >= -1 * nBorderShift;
       .AND. nMouseRow <= MaxRow() + nBorderShift .AND. nMouseCol <= MaxCol() + nBorderShift

        DO WHILE !was_menu_key(nKey, axKeys[MENU_LBUTTONUP])

            nKey := Inkey()

            DO CASE
                CASE MRow() > nMouseRow .AND. MCol() > nMouseCol
                    WMove(WRow() + 1, WCol() + 1)
                CASE MRow() > nMouseRow .AND. MCol() == nMouseCol
                    WMove(WRow() + 1, WCol())
                CASE MRow() > nMouseRow .AND. MCol() < nMouseCol
                    WMove(WRow() + 1, WCol() - 1)
                CASE MRow() == nMouseRow .AND. MCol() > nMouseCol
                    WMove(WRow(), WCol() + 1)
                CASE MRow() == nMouseRow .AND. MCol() == nMouseCol
                    WMove(WRow(), WCol())
                CASE MRow() == nMouseRow .AND. MCol() < nMouseCol
                    WMove(WRow(), WCol() - 1)
                CASE MRow() < nMouseRow .AND. MCol() > nMouseCol
                    WMove(WRow() - 1, WCol() + 1)
                CASE MRow() < nMouseRow .AND. MCol() == nMouseCol
                    WMove(WRow() - 1, WCol())
                CASE MRow() < nMouseRow .AND. MCol() < nMouseCol
                    WMove(WRow() - 1, WCol() - 1)
                OTHERWISE
                    hb_idleSleep(0.1)
            ENDCASE
        ENDDO
    ENDIF

    IF lScrollBar
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
                nNewPosition := nCurrentElement
            CASE nKey == K_DOWN
                nNewPosition := nCurrentElement
            CASE nKey == K_LBUTTONUP
                nNewPosition := nCurrentElement
            CASE nKey == K_MWBACKWARD
                nNewPosition := nCurrentElement
            CASE nKey == K_MWFORWARD
                nNewPosition := nCurrentElement
        ENDCASE

        oScrollBar:current := nNewPosition
        oScrollBar:update()
    ENDIF

RETURN nReturnMode
#pragma ENABLEWARNINGS = On

#pragma ENABLEWARNINGS = Off //because of unused variables
FUNCTION menu_search_disallow_exit_move(nMode, nCurrentElement, nRowPosition, oScrollBar, acMenuItems;
                                        , xSelectable, lIsBorder, axKeys, lMousable, nScrollBarOrientation;
                                       )

    LOCAL nReturnMode := AC_CONT    
    LOCAL nKey := LastKey()
    LOCAL nMouseRow := MRow()
    LOCAL nMouseCol := MCol()
    LOCAL lScrollBar := (ValType(oScrollBar) == 'O')
    LOCAL nHitStatus := IF(lScrollBar .AND. lMousable, oScrollBar:hitTest(nMouseRow, nMouseCol), HTNOWHERE)
    LOCAL nBorderShift := cast(lIsBorder, 'N')
    LOCAL nNewPosition

    DO CASE
        CASE nMode == AC_EXCEPT
            DO CASE
                CASE was_menu_key(nKey, axKeys[MENU_SELECT])
                    nReturnMode := AC_SELECT
                CASE was_menu_key(nKey, axKeys[MENU_UP])
                    WMove(WRow() - 1, WCol())
                CASE was_menu_key(nKey, axKeys[MENU_DOWN])
                    WMove(WRow() + 1, WCol())
                CASE was_menu_key(nKey, axKeys[MENU_LEFT])
                    WMove(WRow(), WCol() - 1)
                CASE was_menu_key(nKey, axKeys[MENU_RIGHT])
                    WMove(WRow(), WCol() + 1)
                CASE was_menu_key(nKey, axKeys[MENU_CENTER])
                    WCenter(.T.)
                OTHERWISE
                    nReturnMode := AC_GOTO
            ENDCASE
        CASE nMode == AC_IDLE .AND. lScrollBar .AND. lMousable .AND. was_menu_key(nKey, axKeys[MENU_LBUTTONUP])
            DO CASE
                CASE nHitStatus == HTSCROLLUNITDEC
                    KEYBOARD Chr(K_UP)
                CASE nHitStatus == HTSCROLLUNITINC
                    KEYBOARD Chr(K_DOWN)
                CASE nHitStatus == HTSCROLLBLOCKDEC
                    move_thumb_up(oScrollBar, nCurrentElement, xSelectable, acMenuItems)
                CASE nHitStatus == HTSCROLLBLOCKINC
                    move_thumb_down(oScrollBar, nCurrentElement, xSelectable, acMenuItems)
                CASE nHitStatus == HTSCROLLTHUMBDRAG
            ENDCASE
    ENDCASE

    IF nHitStatus == HTNOWHERE .AND. was_menu_key(nKey, axKeys[MENU_LBUTTONDOWN]) .AND. lMousable;
       .AND. nMouseRow >= -1 * nBorderShift .AND. nMouseCol >= -1 * nBorderShift;
       .AND. nMouseRow <= MaxRow() + nBorderShift .AND. nMouseCol <= MaxCol() + nBorderShift

        DO WHILE !was_menu_key(nKey, axKeys[MENU_LBUTTONUP])

            nKey := Inkey()

            DO CASE
                CASE MRow() > nMouseRow .AND. MCol() > nMouseCol
                    WMove(WRow() + 1, WCol() + 1)
                CASE MRow() > nMouseRow .AND. MCol() == nMouseCol
                    WMove(WRow() + 1, WCol())
                CASE MRow() > nMouseRow .AND. MCol() < nMouseCol
                    WMove(WRow() + 1, WCol() - 1)
                CASE MRow() == nMouseRow .AND. MCol() > nMouseCol
                    WMove(WRow(), WCol() + 1)
                CASE MRow() == nMouseRow .AND. MCol() == nMouseCol
                    WMove(WRow(), WCol())
                CASE MRow() == nMouseRow .AND. MCol() < nMouseCol
                    WMove(WRow(), WCol() - 1)
                CASE MRow() < nMouseRow .AND. MCol() > nMouseCol
                    WMove(WRow() - 1, WCol() + 1)
                CASE MRow() < nMouseRow .AND. MCol() == nMouseCol
                    WMove(WRow() - 1, WCol())
                CASE MRow() < nMouseRow .AND. MCol() < nMouseCol
                    WMove(WRow() - 1, WCol() - 1)
                OTHERWISE
                    hb_idleSleep(0.1)
            ENDCASE
        ENDDO
    ENDIF

    IF lScrollBar
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
                nNewPosition := nCurrentElement
            CASE nKey == K_DOWN
                nNewPosition := nCurrentElement
            CASE nKey == K_LBUTTONUP
                nNewPosition := nCurrentElement
            CASE nKey == K_MWBACKWARD
                nNewPosition := nCurrentElement
            CASE nKey == K_MWFORWARD
                nNewPosition := nCurrentElement
        ENDCASE

        oScrollBar:current := nNewPosition
        oScrollBar:update()
    ENDIF

RETURN nReturnMode
