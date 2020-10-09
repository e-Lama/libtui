#include "inkey.ch"
#include "hbclass.ch"

#include "rowbrowse.ch"
#include "align.ch"
#include "functions.ch"

#include "setup.ch"

CREATE CLASS Row_browse

EXPORTED:

    METHOD new(cTableID, nTop, nLeft, nBottom, nRight, cColor, cBorder, cTitle, cTitleAlign;
               , cTitleColor, nStartRow, bUserAction, bColorBlock, acHeadersColors, hKeysMap, xCargo;
               , bSkip, bGoBottom, bGoTop, lMousable) CONSTRUCTOR

    //Row_browse extensions
    METHOD display(lEnd)
    METHOD finish() INLINE ::__lActive := .F.
    METHOD reprepare() INLINE ::__lPrepared := .F.

    METHOD title(cTitle) SETGET
    METHOD top(nTop) SETGET                                         // It's not TBrowse top
    METHOD left(nLeft) SETGET                                       // It's not TBrowse left
    METHOD right(nRight) SETGET                                     // It's not TBrowse right
    METHOD bottom(nBottom) SETGET                                   // It's not TBrowse bottom
    METHOD keys_map(hKeysMap) SETGET
    METHOD header_color(nIndex, cColor) SETGET
    METHOD get_separator(nIndex)
    METHOD get_header(nIndex)

    METHOD home() INLINE IF(::__lPrepared, ::__go_first_visible(::__oTBrowse:rowPos), throw(RUNTIME_EXCEPTION))
    METHOD end() INLINE IF(::__lPrepared, ::__go_last_visible(::__oTBrowse:rowPos, ::__oTBrowse:rowCount()), throw(RUNTIME_EXCEPTION))

    METHOD search(cTarget, lExactly, cPattern)
    METHOD search_keys(cSearchKeys)

    METHOD print_title()
    METHOD draw_border() INLINE DispBox(::__nTop, ::__nLeft, ::__nBottom, ::__nRight, ::__cBorder, ::__cColor)

    METHOD refresh(nNewRefresh)

    METHOD prepare()

    METHOD how_to(axNewHowTo) SETGET

    //Basic TBrowse 
    //Variables wrappers
    METHOD automatic_highlight(lAutomaticHighlight) SETGET          // autoLite wrapper
    METHOD border(cBorder) SETGET                                   // border wrapper
    METHOD cargo(xCargo) SETGET                                     // cargo wrapper
    METHOD color(cColor) SETGET                                     // colorSpec wrapper
    METHOD hit_bottom(lHitBottom) SETGET                            // hitBottom wrapper
    METHOD hit_top(lHitTop) SETGET                                  // hitTop wrapper
    METHOD mouse_column_position(nMouseColumnPosition) SETGET       // mColPos wrapper
    METHOD mouse_row_position(nMouseRowPosition) SETGET             // mRowPos wrapper
    METHOD row_position(nRowPosition) SETGET                        // nRowPos wrapper
    METHOD skip_block(bSkipBlock) SETGET                            // skipBlock wrapper
    METHOD stable(lStable) SETGET                                   // stable wrapper
    METHOD go_bottom_block(bGoBottom) SETGET                        // goBottom wrapper
    METHOD go_top_block(bGoTop) SETGET                              // goTop wrapper

    METHOD color_block(bColorBlock) SETGET                          // TBColumn colorBlock

    //Exported methods wrappers
    METHOD down() INLINE IF(::__lPrepared, (::__oTBrowse:down(), NIL), throw(RUNTIME_EXCEPTION))                       // down() wrapper
    METHOD up() INLINE IF(::__lPrepared, (::__oTBrowse:up(), NIL), throw(RUNTIME_EXCEPTION))                           // up() wrapper
    METHOD go_top() INLINE IF(::__lPrepared, (::__oTBrowse:goTop(), NIL), throw(RUNTIME_EXCEPTION))                    // goTop() wrapper
    METHOD go_bottom() INLINE IF(::__lPrepared, (::__oTBrowse:goBottom(), NIL), throw(RUNTIME_EXCEPTION))              // goBottom() wrapper
    METHOD page_down() INLINE IF(::__lPrepared, (::__oTBrowse:pageDown(), NIL), throw(RUNTIME_EXCEPTION))              // pageDown() wrapper
    METHOD page_up() INLINE IF(::__lPrepared, (::__oTBrowse:pageUp(), NIL), throw(RUNTIME_EXCEPTION))                  // pageUp() wrapper
    METHOD configure() INLINE IF(::__lPrepared, (::__oTBrowse:configure(), NIL), throw(RUNTIME_EXCEPTION))             // configure() wrapper
    METHOD dehighlight() INLINE IF(::__lPrepared, (::__oTBrowse:deHilite(), NIL), throw(RUNTIME_EXCEPTION))            // deHilite() wrapper
    METHOD force_stable() INLINE IF(::__lPrepared, (::__oTBrowse:forceStable(), NIL), throw(RUNTIME_EXCEPTION))        // forceStable() wrapper
    METHOD highlight() INLINE IF(::__lPrepared, (::__oTBrowse:hilite(), NIL), throw(RUNTIME_EXCEPTION))                // Hilite() wrapper
    METHOD invalidate() INLINE IF(::__lPrepared, (::__oTBrowse:invalidate(), NIL), throw(RUNTIME_EXCEPTION))           // invalidate() wrapper
    METHOD refresh_all() INLINE IF(::__lPrepared, (::__oTBrowse:refreshAll(), NIL), throw(RUNTIME_EXCEPTION))          // refreshAll() wrapper
    METHOD refresh_current() INLINE IF(::__lPrepared, (::__oTBrowse:refreshCurrent(), NIL), throw(RUNTIME_EXCEPTION))  // refreshCurrent() wrapper
    METHOD stabilize() INLINE IF(::__lPrepared, (::__oTBrowse:stabilize(), NIL), throw(RUNTIME_EXCEPTION))             // stabilize() wrapper
    METHOD row_count() INLINE IF(::__lPrepared, ::__oTBrowse:rowCount(), throw(RUNTIME_EXCEPTION))                     // rowCount() wrapper
    METHOD get_row_pos() INLINE IF(::__lPrepared, ::__oTBrowse:rowPos, throw(RUNTIME_EXCEPTION))                       // getRowPos wrapper
    METHOD color_rectangle(anRows, anColors)                                                                           // colorRect() wrapper
    METHOD hit_test(nRow)                                                                                              // HitTest() wrapper

HIDDEN:

    VAR __lPrepared AS LOGICAL INIT .F.
    VAR __lActive AS LOGICAL INIT .F.
    VAR __lMousable AS LOGICAL INIT .T.
    VAR __cTitle AS CHARACTER INIT ''
    VAR __axHowTo AS ARRAY INIT Array(0)
    VAR __cTitleAlign AS CHARACTER INIT ALIGN_CENTER
    VAR __cTitleColor AS CHARACTER INIT ''
    VAR __nStartRow AS NUMERIC INIT 0
    VAR __bUserAction AS CODEBLOCK INIT {|| ROWBROWSE_NOTHING}
    VAR __bColorBlock AS CODEBLOCK INIT {|| nothing()}
    VAR __acHeadersColors AS ARRAY INIT Array(0)
    VAR __bSkipBlock AS CODEBLOCK INIT {| nRecs | __dbSkipper(nRecs)} //src/rtl/browdb.prg
    VAR __bGoBottom AS CODEBLOCK INIT {|| dbGoBottom()}
    VAR __bGoTop AS CODEBLOCK INIT {|| dbGoTop()}
    VAR __nRefresh AS NUMERIC INIT ALWAYS_REFRESH_CURRENT
    VAR __acHeaders AS ARRAY INIT Array(0)
    VAR __acSeparators AS ARRAY INIT Array(0)
    VAR __cSearchKeys AS CHARACTER INIT ''
    VAR __hKeysMap AS HASH INIT {K_ESC => K_ESC, K_DOWN => K_DOWN, K_UP => K_UP, K_PGUP => K_PGUP;
                                 , K_PGDN => K_PGDN, K_HOME => K_HOME, K_END => K_END, K_CTRL_HOME => K_CTRL_HOME;
                                 , K_CTRL_END => K_CTRL_END;
                                }
    VAR __cColor AS CHARACTER INIT Config():get_config('DefaultColor')
    VAR __cBorder AS CHARACTER INIT Config():get_config('RowBrowseDefaultBox')
    VAR __oTBrowse AS OBJECT 
    VAR __cTableID AS CHARACTER
    VAR __nTop AS NUMERIC
    VAR __nLeft AS NUMERIC
    VAR __nBottom AS NUMERIC
    VAR __nRight AS NUMERIC 
    VAR __lSpecialOperationRefresh AS LOGICAL
    VAR __nOldRecNo AS NUMERIC INIT 0
    VAR __nOldRow AS NUMERIC
    VAR __xCargo

    METHOD __is_align(cAlign) INLINE ValType(cAlign) == 'C' .AND. AScan({ALIGN_LEFT, ALIGN_CENTER, ALIGN_RIGHT}, cAlign) > 0
    METHOD __extract_separator(cSeparator) INLINE IF(Empty(Left(cSeparator, 1)), '', Right(cSeparator, 1))
    METHOD __linear_search(cTarget, lExactly, cPattern)
    METHOD __binary_search(cTarget, lExactly)
    METHOD __handle_action(nAction)
    METHOD __handle_move(nKey, nRow)
    METHOD __how_to_browse()
    METHOD __get_print_function()
    METHOD __go_first_visible()
    METHOD __go_last_visible()
    METHOD __print_header()
    METHOD __do_refresh()

#ifdef USE_VALIDATORS
    METHOD __keys_map_asserts(hKeysMap)
    METHOD __validate(axRow)
#endif

ENDCLASS LOCK

METHOD how_to(axNewHowTo) CLASS Row_browse

    LOCAL axOldHowTo := AClone(::__axHowTo)

    IF ValType(axNewHowTo) != 'U'
#ifdef USE_VALIDATORS
        assert_type(axNewHowTo, 'A')

        AEval(axNewHowTo, {| axRow | assert_type(axRow, 'A'), IF(!::__validate(axRow), throw(RUNTIME_EXCEPTION), NIL)})
#endif

        ::__axHowTo := AClone(axNewHowTo)
        ::__lPrepared := .F.
    ENDIF

RETURN axOldHowTo

METHOD refresh(nNewRefresh) CLASS Row_browse

    LOCAL nOldRefresh := ::__nRefresh

    IF ValType(nNewRefresh) != 'U'
#ifdef USE_VALIDATORS
        assert_type(nNewRefresh, 'N')
#endif
        ::__nRefresh := nNewRefresh
    ENDIF

RETURN nOldRefresh

METHOD get_separator(nIndex) CLASS Row_browse

#ifdef USE_VALIDATORS
    assert_type(nIndex, 'N')

    IF nIndex < 1 .OR. nIndex > Len(::__acSeparators) .OR. Int(nIndex) != nIndex
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF
#endif

RETURN ::__acSeparators[nIndex]

METHOD get_header(nIndex) CLASS Row_browse

#ifdef USE_VALIDATORS
    assert_type(nIndex, 'N')

    IF nIndex < 1 .OR. nIndex > Len(::__acHeaders) .OR. Int(nIndex) != nIndex
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF
#endif

RETURN ::__acHeaders[nIndex]

METHOD color_rectangle(anRows, anColors) CLASS Row_browse

#ifdef USE_VALIDATORS
    LOCAL nNumber

    IF !::__lPrepared
        throw(RUNTIME_EXCEPTION)
    ENDIF

    assert_type(anRows, 'A')
    assert_type(anColors, 'A')

    IF Len(anRows) != 2 .OR. anRows[1] < 0 .OR. anRows[2] < 0 .OR. anRows[1] > anRows[2]
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF

    IF Empty(anColors)
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF

    FOR EACH nNumber IN anColors
        IF nNumber < 1 .OR. nNumber > Len(::__cColor)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
    NEXT
#endif
    
    ::__oTBrowse:colorRect({anRows[1], 1, anRows[2], 1}, anColors)

RETURN NIL

METHOD hit_test(nRow) CLASS Row_browse

#ifdef USE_VALIDATORS
    assert_type(nRow, 'N')

    IF nRow < 1
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF
#endif

RETURN ::__oTBrowse:hitTest(nRow, 1)

METHOD header_color(nIndex, cColor) CLASS Row_browse

    LOCAL cOldColor

#ifdef USE_VALIDATORS
    assert_type(nIndex, 'N')

    IF nIndex < 1 .OR. nIndex > Len(::__acHeadersColors) .OR. Int(nIndex) != nIndex
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF
#endif

    cOldColor := ::__acHeadersColors[nIndex]

    IF cColor != NIL
#ifdef USE_VALIDATORS
        assert_type(cColor, 'C')
        IF !Empty(cColor) .AND. !is_color(cColor)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif
        
        ::__acHeadersColors[nIndex] := cColor
    ENDIF

RETURN cOldColor

METHOD color_block(bColorBlock) CLASS Row_browse
    
    LOCAL bOldColorBlock := ::__bColorBlock

    IF bColorBlock != NIL
#ifdef USE_VALIDATORS
        assert_type(bColorBlock, 'B')
#endif
        ::__bColorBlock := bColorBlock
    ENDIF

RETURN bOldColorBlock

METHOD row_position(nRowPosition) CLASS Row_browse

    LOCAL nOldRowPosition := ::__oTBrowse:rowPos()

    IF nRowPosition != NIL
#ifdef USE_VALIDATORS
        assert_type(nRowPosition, 'N')

        IF nRowPosition < 0
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif

        ::__oTBrowse:rowPos(nRowPosition)
    ENDIF

RETURN nOldRowPosition

#ifdef USE_VALIDATORS
METHOD __keys_map_asserts(hKeysMap) CLASS Row_browse

    LOCAL nKey

    assert_type(hKeysMap, 'H')

    FOR EACH nKey IN hb_hKeys(hKeysMap)
        assert_type(nKey, 'N')
        assert_type(hKeysMap[nKey], 'N')

        IF nKey < 0 .OR. hKeysMap[nKey] < 0
            throw(ARGUMENT_VALUE_EXCEPTION)
        ELSEIF AScan({K_ESC, K_DOWN, K_UP, K_PGUP, K_PGDN, K_HOME, K_END, K_CTRL_HOME, K_CTRL_END}, hKeysMap[nKey]) == 0
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
    NEXT

RETURN NIL
#endif

METHOD keys_map(hKeysMap) CLASS Row_browse

    LOCAL hOldKeysMap := hb_hClone(::__hKeysMap)

    IF hKeysMap != NIL
#ifdef USE_VALIDATORS
        ::__keys_map_asserts(hKeysMap)
#endif
        ::__hKeysMap := hKeysMap
    ENDIF

RETURN hOldKeysMap

METHOD top(nTop) CLASS Row_browse

    LOCAL nOldTop := ::__nTop

    IF nTop != NIL
#ifdef USE_VALIDATORS
        assert_type(nTop, 'N')

        IF nTop > ::__nBottom
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif

        ::__nTop := nTop
    ENDIF

RETURN nOldTop

METHOD left(nLeft) CLASS Row_browse

    LOCAL nOldLeft := ::__nLeft

    IF nLeft != NIL
#ifdef USE_VALIDATORS
        assert_type(nLeft, 'N')

        IF nLeft > ::__nRight
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif

        ::__nLeft := nLeft
    ENDIF

RETURN nOldLeft

METHOD bottom(nBottom) CLASS Row_browse

    LOCAL nOldBottom := ::__nBottom

    IF nBottom != NIL
#ifdef USE_VALIDATORS
        assert_type(nBottom, 'N')

        IF ::__nTop > nBottom
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif

        ::__nBottom := nBottom
    ENDIF

RETURN nOldBottom

METHOD right(nRight) CLASS Row_browse

    LOCAL nOldRight := ::__nRight

    IF nRight != NIL
#ifdef USE_VALIDATORS
        assert_type(nRight, 'N')

        IF ::__nLeft > nRight
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif

        ::__nRight := nRight
    ENDIF

RETURN nOldRight

METHOD title(cTitle) CLASS Row_browse

    LOCAL cOldTitle := ::__cTitle

    IF cTitle != NIL
#ifdef USE_VALIDATORS
        assert_type(cTitle, 'C')
#endif
        ::__cTitle := cTitle
    ENDIF

RETURN cOldTitle

METHOD automatic_highlight(lAutomaticHighlight) CLASS Row_browse

    LOCAL lOldAutomaticHighlight := ::__oTBrowse:autoLite

    IF lAutomaticHighlight != NIL
#ifdef USE_VALIDATORS
        assert_type(lAutomaticHighlight, 'L')
#endif
        ::__oTBrowse:autoLite := lAutomaticHighlight
    ENDIF

RETURN lOldAutomaticHighlight

METHOD border(cBorder) CLASS Row_browse

    LOCAL cOldBorder := ::__cBorder

    IF cBorder != NIL
#ifdef USE_VALIDATORS
        assert_type(cBorder, 'C')
        IF is_box(hb_Translate(cBorder), 'EN', hb_cdpSelect())
#endif
            ::__cBorder := cBorder
#ifdef USE_VALIDATORS
        ELSE
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif
    ENDIF

RETURN cOldBorder

METHOD cargo(xCargo) CLASS Row_browse

    LOCAL xOldCargo := ::__xCargo

    IF xCargo != NIL
        ::__xCargo := xCargo
    ENDIF

RETURN xOldCargo

METHOD color(cColor) CLASS Row_browse

    LOCAL cOldColor := ::__cColor

    IF cColor != NIL
#ifdef USE_VALIDATORS
        assert_type(cColor, 'C')
        IF is_color(cColor)
#endif
            ::__cColor := cColor
#ifdef USE_VALIDATORS
        ELSE
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif
    ENDIF

RETURN cOldColor

METHOD hit_bottom(lHitBottom) CLASS Row_browse

    LOCAL xOldHitBottom := IF(::__lPrepared, ::__oTBrowse:getBottomFlag(), NIL)

    IF lHitBottom != NIL .AND. ::__lPrepared
#ifdef USE_VALIDATORS
        assert_type(lHitBottom, 'L')
#endif
        ::__oTBrowse:setBottomFlag(lHitBottom)
    ENDIF

RETURN xOldHitBottom

METHOD hit_top(lHitTop) CLASS Row_browse

    LOCAL xOldHitTop := IF(::__lPrepared, ::__oTBrowse:getTopFlag(), NIL)

    IF lHitTop != NIL .AND. ::__lPrepared
#ifdef USE_VALIDATORS
        assert_type(lHitTop, 'L')
#endif
        ::__oTBrowse:setTopFlag(lHitTop)
    ENDIF

RETURN xOldHitTop

METHOD mouse_column_position(nMouseColumnPosition) CLASS Row_browse

    LOCAL nOldMouseColumnPosition := ::nMouseColumnPosition

    IF nMouseColumnPosition != NIL
#ifdef USE_VALIDATORS
        assert_type(nMouseColumnPosition, 'N')
#endif
        ::nMouseColumnPosition := nMouseColumnPosition
    ENDIF

RETURN nOldMouseColumnPosition

METHOD mouse_row_position(nMouseRowPosition) CLASS Row_browse

    LOCAL nOldMouseRowPosition := ::nMouseRowPosition

    IF nMouseRowPosition != NIL
#ifdef USE_VALIDATORS
        assert_type(nMouseRowPosition, 'N')
#endif
        ::nMouseRowPosition := nMouseRowPosition
    ENDIF

RETURN nOldMouseRowPosition

METHOD skip_block(bSkipBlock) CLASS Row_browse

    LOCAL bOldSkipBlock := ::__bSkipBlock

    IF bOldSkipBlock != NIL
#ifdef USE_VALIDATORS
        assert_type(bSkipBlock, 'B')
#endif
        ::__bSkipBlock := bSkipBlock
    ENDIF

RETURN bOldSkipBlock

METHOD go_bottom_block(bGoBottom) CLASS Row_browse

    LOCAL bOldGoBottom:= ::__bGoBottom

    IF bGoBottom != NIL
#ifdef USE_VALIDATORS
        assert_type(bGoBottom, 'B')
#endif
        ::__bGoBottom := bGoBottom
    ENDIF

RETURN bOldGoBottom

METHOD go_top_block(bGoTop) CLASS Row_browse

    LOCAL bOldGoTop:= ::__bGoTop

    IF bGoTop != NIL
#ifdef USE_VALIDATORS
        assert_type(bGoTop, 'B')
#endif
        ::__bGoTop := bGoTop
    ENDIF

RETURN bOldGoTop

METHOD stable(lStable) CLASS Row_browse

    LOCAL xOldStable := IF(::__lPrepared, ::__oTBrowse:getStableFlag(), NIL)

    IF lStable != NIL .AND. ::__lPrepared
#ifdef USE_VALIDATORS
        assert_type(lStable, 'L')
#endif
        ::__oTBrowse:setStableFlag(lStable)
    ENDIF

RETURN xOldStable

METHOD new(cTableID, nTop, nLeft, nBottom, nRight, cColor, cBorder, cTitle, cTitleAlign;
           , cTitleColor, nStartRow, bUserAction, bColorBlock, acHeadersColors, hKeysMap, xCargo;
           , bSkip, bGoBottom, bGoTop, lMousable) CLASS Row_browse

#ifdef USE_VALIDATORS
    assert_type(cTableID, 'C')
    assert_type(nTop, 'N')
    assert_type(nLeft, 'N')
    assert_type(nBottom, 'N')
    assert_type(nRight, 'N')
#endif

    ::__cTableID := cTableID

#ifdef USE_VALIDATORS
    IF nTop >= nBottom .OR. nRight <= nLeft
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF
#endif

    ::__nTop := nTop
    ::__nLeft := nLeft
    ::__nBottom := nBottom
    ::__nRight := nRight

    IF cColor != NIL
#ifdef USE_VALIDATORS
        assert_type(cColor, 'C')

        IF !is_color(cColor)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif

        ::__cColor := cColor
    ENDIF

    IF cBorder != NIL
#ifdef USE_VALIDATORS
        assert_type(cBorder, 'C')

        IF !is_box(hb_Translate(cBorder, 'EN', hb_cdpSelect()))
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif

        ::__cBorder := cBorder
    ENDIF

    IF cTitle != NIL
#ifdef USE_VALIDATORS
        assert_type(cTitle, 'C')
#endif
        ::__cTitle := cTitle
    ENDIF

    IF cTitleAlign != NIL
#ifdef USE_VALIDATORS
        assert_type(cTitleAlign, 'C')

        IF !::__is_align(cTitleAlign)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif

        ::__cTitleAlign := cTitleAlign
    ENDIF

    IF cTitleColor != NIL
#ifdef USE_VALIDATORS
        assert_type(cTitleColor, 'C')

        IF !is_color(cTitleColor)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif

        ::__cTitleColor := cTitleColor
    ENDIF

    IF nStartRow != NIL
#ifdef USE_VALIDATORS
        assert_type(nStartRow, 'N')

        IF nStartRow < 0
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif

        ::__nStartRow := nStartRow
    ENDIF

    IF bUserAction != NIL
#ifdef USE_VALIDATORS
        assert_type(bUserAction, 'B')
#endif
        ::__bUserAction := bUserAction
    ENDIF

    IF bColorBlock != NIL
#ifdef USE_VALIDATORS
        assert_type(bColorBlock, 'B')
#endif
        ::__bColorBlock := bColorBlock
    ENDIF

    IF ValType(acHeadersColors) == 'A'
#ifdef USE_VALIDATORS
        AEval(acHeadersColors, {| cColor | assert_type(cColor, 'C'), IF(!Empty(cColor) .AND. !is_color(cColor), throw(ARGUMENT_VALUE_EXCEPTION), )})
#endif
        ::__acHeadersColors := acHeadersColors
#ifdef USE_VALIDATORS
    ELSEIF ValType(acHeadersColors) != 'U'
        throw(ARGUMENT_TYPE_EXCEPTION)
#endif
    ENDIF

    IF hKeysMap != NIL
#ifdef USE_VALIDATORS
        ::__keys_map_asserts(hKeysMap)
#endif
        ::__hKeysMap := hKeysMap
    ENDIF

    ::__xCargo := xCargo

    IF bSkip != NIL
#ifdef USE_VALIDATORS
        assert_type(bSkip, 'B')
#endif
        ::bSkip := bSkip
    ENDIF

    IF bGoBottom != NIL
#ifdef USE_VALIDATORS
        assert_type(bGoBottom, 'B')
#endif
        ::__bGoBottom := bGoBottom
    ENDIF

    IF bGoTop != NIL
#ifdef USE_VALIDATORS
        assert_type(bGoTop, 'B')
#endif
        ::__bGoTop := bGoTop
    ENDIF

    IF lMousable != NIL
#ifdef USE_VALIDATORS
        assert_type(lMousable, 'L')
#endif
        ::__lMousable := lMousable
    ENDIF

RETURN Self

METHOD prepare() CLASS Row_browse

    IF !::__lPrepared
        ::__oTBrowse := TBrowseDB(::__nTop + 2, ::__nLeft + 1, ::__nBottom - 1, ::__nRight - 1)
        ::__oTBrowse:addColumn(TBColumnNew(NIL, ::__get_print_function()))
        ::__oTBrowse:colorSpec := ::__cColor
        ::__oTBrowse:GoTopBlock := ::__bGoTop
        ::__oTBrowse:GoBottomBlock := ::__bGoBottom
        ::__oTBrowse:SkipBlock := ::__bSkipBlock
        ::__oTBrowse:GetColumn(1):ColorBlock := ::__bColorBlock
        ::__lPrepared := .T.
    ENDIF

RETURN Self

METHOD display(lEnd) CLASS Row_browse

    LOCAL lRestart := .F.
    LOCAL nRowFromHeader
    LOCAL nAction
    LOCAL nKey

    IF lEnd == NIL
        lEnd := .F.
#ifdef USE_VALIDATORS
    ELSE
        assert_type(lEnd, 'L')
#endif
    ENDIF

    ::prepare()

    ::__lActive := .T.

    ::draw_border()
    ::print_title()

    IF ::__nOldRecNo == 0
        SKIP ::__nStartRow
    ELSE
        GO ::__nOldRecNo
        ::__nOldRecNo := 0

        DO WHILE ::__nOldRow != 0
            ::__oTBrowse:up()
            --::__nOldRow
        ENDDO
    ENDIF

    IF lEnd
        ::__oTBrowse:ForceStable()
    ELSE
        DO WHILE !lEnd .AND. ::__lActive

            ::__oTBrowse:ForceStable()

            ::__print_header()

            nKey := Inkey(0)
            nRowFromHeader := ::__oTBrowse:mRowPos

            nAction := ::__handle_move(IF(hb_hHasKey(::__hKeysMap, nKey), ::__hKeysMap[nKey], nKey), nRowFromHeader)

            IF nAction != ROWBROWSE_NO_ACTION .AND. nAction != ROWBROWSE_END
                nAction := ::__handle_action(Eval(::__bUserAction, Self, nKey, nRowFromHeader))
            ENDIF

            IF nAction == ROWBROWSE_END
                lEnd := .T.
            ELSEIF nAction == ROWBROWSE_RESTART
                lEnd := .T.
                lRestart := .T.
            ELSEIF nAction == ROWBROWSE_RESTART_PRESERVE
                lEnd := .T.
                lRestart := .T.
                ::__nOldRecNo := RecNo()
                ::__nOldRow := ::__oTBrowse:nRow
            ENDIF

            IF !lEnd
                ::__do_refresh()
            ENDIF
        ENDDO
    ENDIF

    ::__lActive := .F.

    IF lRestart
        ::display()
    ENDIF

RETURN NIL

METHOD __do_refresh() CLASS Row_browse

    DO CASE
        CASE ::__lSpecialOperationRefresh
            ::__oTBrowse:refreshAll()
            ::__lSpecialOperationRefresh := .F.
        CASE ::__nRefresh == ALWAYS_REFRESH_ALL
            ::__oTBrowse:refreshAll()
        CASE ::__nRefresh == ALWAYS_REFRESH_CURRENT
            ::__oTBrowse:refreshCurrent()
        CASE ::__nRefresh == REFRESH_ALL_ONCE
            ::__oTBrowse:refreshAll()
            ::__nRefresh := DO_NOT_REFRESH
        CASE ::__nRefresh == REFRESH_CURRENT_ONCE
            ::__oTBrowse:refreshCurrent()
            ::__nRefresh := DO_NOT_REFRESH
        OTHERWISE
            throw(ARGUMENT_VALUE_EXCEPTION)
    ENDCASE

RETURN NIL

METHOD __print_header() CLASS Row_browse

    LOCAL nCol := ::__nLeft + 1
    LOCAL nColorsLen := Len(::__acHeadersColors)
    LOCAL i

    FOR i := 1 TO Len(::__acHeaders)
        IF i <= nColorsLen .AND. !Empty(::__acHeadersColors[i])
            @ ::__nTop + 1, nCol SAY ::__acHeaders[i] COLOR ::__acHeadersColors[i]
        ELSE
            @ ::__nTop + 1, nCol SAY ::__acHeaders[i] 
        ENDIF
        
        nCol += Len(::__acHeaders[i])

        IF i < Len(::__acHeaders)
            @ ::__nTop + 1, nCol SAY ::__acSeparators[i]
            ++nCol
        ENDIF
    NEXT

RETURN NIL

METHOD __handle_move(nKey, nRow) CLASS Row_browse

    DO CASE
        CASE SetKey(nKey) != NIL
            Eval(SetKey(nKey))
        CASE nKey == K_ESC
            RETURN ROWBROWSE_END
        CASE nKey == K_DOWN
            ::__oTBrowse:Down()
        CASE nKey == K_UP
            ::__oTBrowse:Up()
        CASE nKey == K_PGUP
            ::__oTBrowse:PageUp()
        CASE nKey == K_PGDN
            ::__oTBrowse:PageDown()
        CASE nKey == K_HOME
            ::__go_first_visible()
        CASE nKey == K_END
            ::__go_last_visible()
        CASE nKey == K_CTRL_HOME
            ::__oTBrowse:GoTop()
        CASE nKey == K_CTRL_END
            ::__oTBrowse:GoBottom()
        CASE ::__lMousable
            DO CASE
                CASE nKey == K_LBUTTONDOWN .AND. nRow != 0
                    DO WHILE nRow > ::__oTBrowse:rowPos
                      ::__oTBrowse:Down()
                      --nRow
                    ENDDO

                    DO WHILE nRow < ::__oTBrowse:rowPos
                      ::__oTBrowse:Up()
                      ++nRow
                    ENDDO
                CASE nKey == K_MWFORWARD
                    ::__oTBrowse:Down()
                CASE nKey == K_MWBACKWARD
                    ::__oTBrowse:Up()
            ENDCASE
    ENDCASE 

RETURN ROWBROWSE_NOTHING

METHOD __handle_action(nAction) CLASS Row_browse

    DO CASE
        CASE nAction == ROWBROWSE_NOTHING
        CASE nAction == ROWBROWSE_END
        CASE nAction == ROWBROWSE_CLEAR_SEARCH_STRING
            ::draw_border()
            ::print_title()
        CASE nAction == ROWBROWSE_REBORDER
            ::draw_border()
            ::print_title()
        CASE nAction == ROWBROWSE_SEARCH
            ::__lSpecialOperationRefresh := .T.
        CASE nAction == ROWBROWSE_RESTART
        CASE nAction == ROWBROWSE_RESTART_PRESERVE
        OTHERWISE
            throw(ARGUMENT_VALUE_EXCEPTION)
    ENDCASE

RETURN nAction

METHOD __how_to_browse() CLASS Row_browse

    LOCAL nOldSelect := Select()

#ifdef USE_VALIDATORS
    IF Select('DBROWBROWSE') == 0
        throw(RUNTIME_EXCEPTION)
    ENDIF
#endif

    SELECT dbRowBrowse

#ifdef USE_VALIDATORS
    IF IndexOrd() == 0
        throw(RUNTIME_EXCEPTION)
    ENDIF
#endif

    SEEK ::__cTableID

#ifdef USE_VALIDATORS
    IF !Found()
        throw(RUNTIME_EXCEPTION)
    ENDIF
#endif

    DO WHILE RTrim(field->id) == ::__cTableID .AND. !Eof()
        AAdd(::__axHowTo, {;
                            field->col_nr, field->width, field->relative, field->fld_sep, field->fld_align;
                            , RTrim(field->head), field->head_sep, field->head_align, RTrim(field->print);
                          };
            )
        SKIP
    ENDDO

    SELECT (nOldSelect)

RETURN Self

#ifdef USE_VALIDATORS
METHOD __validate(axRow) CLASS Row_browse

    IF Len(axRow) != C_PRINT_FNC
        RETURN .F.
    ENDIF

    IF axRow[N_COLUMN_NUMBER] <= 0
        RETURN .F.
    ENDIF

    IF axRow[N_FIELD_WIDTH] <= 0
        RETURN .F.
    ENDIF

    IF axRow[L_RELATIVE]
        IF axRow[N_FIELD_WIDTH] > 100
            RETURN .F.
        ENDIF
    ENDIF

    IF !::__is_align(axRow[C_FIELD_ALIGN]) .OR. !::__is_align(axRow[C_HEADER_ALIGN])
        RETURN .F.
    ENDIF

    IF Empty(axRow[C_PRINT_FNC])
        RETURN .F.
    ENDIF

RETURN .T.
#endif

METHOD __get_print_function() CLASS Row_browse

    LOCAL nSpaceLeft := ::__nRight - ::__nLeft - 1
    LOCAL cMethod := ''
    LOCAL nIteration := 1
    LOCAL cFieldSeparator
    LOCAL cHeaderSeparator
    LOCAL nStringLength
    LOCAL nComma
    LOCAL axRow

    IF Empty(::__axHowTo)
        ::__how_to_browse()
    ENDIF

    ASize(::__acHeaders, Len(::__axHowTo))
    ASize(::__acSeparators, Len(::__axHowTo) - 1)

    FOR EACH axRow IN ::__axHowTo

#ifdef USE_VALIDATORS
        IF !::__validate(axRow)
            throw(RUNTIME_EXCEPTION)
        ENDIF
#endif

        cFieldSeparator := ::__extract_separator(axRow[C_FIELD_SEPARATOR])
        cHeaderSeparator := ::__extract_separator(axRow[C_HEADER_SEPARATOR])

        IF !Empty(cFieldSeparator) .AND. Empty(cHeaderSeparator)
            cHeaderSeparator := ' '
        ELSEIF Empty(cFieldSeparator) .AND. !Empty(cHeaderSeparator)
            cFieldSeparator := ' '
        ENDIF

        IF axRow[L_RELATIVE]
            nStringLength := Min(Round(axRow[N_FIELD_WIDTH] * (::__nRight - ::__nLeft - 1) / 100.0, 0), nSpaceLeft)
        ELSE
            nStringLength := Min(axRow[N_FIELD_WIDTH], nSpaceLeft)
        ENDIF

        nSpaceLeft -= nStringLength

        DO CASE
            CASE axRow[C_FIELD_ALIGN] == ALIGN_LEFT
                cMethod += 'PadR(' + axRow[C_PRINT_FNC] + ',' + LTrim(Str(nStringLength)) + ')+'
            CASE axRow[C_FIELD_ALIGN] == ALIGN_CENTER
                cMethod += 'PadC(' + axRow[C_PRINT_FNC] + ',' + LTrim(Str(nStringLength)) + ')+'
            CASE axRow[C_FIELD_ALIGN] == ALIGN_RIGHT
                cMethod += 'PadL(' + axRow[C_PRINT_FNC] + ',' + LTrim(Str(nStringLength)) + ')+'
        ENDCASE

        DO CASE
            CASE axRow[C_HEADER_ALIGN] == ALIGN_LEFT
                ::__acHeaders[nIteration] := 'PadR("' + axRow[C_HEADER] + '",' + LTrim(Str(nStringLength)) + ')'
            CASE axRow[C_HEADER_ALIGN] == ALIGN_CENTER
                ::__acHeaders[nIteration] := 'PadC("' + axRow[C_HEADER] + '",' + LTrim(Str(nStringLength)) + ')'
            CASE axRow[C_HEADER_ALIGN] == ALIGN_RIGHT
                ::__acHeaders[nIteration] := 'PadL("' + axRow[C_HEADER] + '",' + LTrim(Str(nStringLength)) + ')'
        ENDCASE

        IF nIteration != Len(::__axHowTo)
            IF nSpaceLeft > 0 .AND. Len(cFieldSeparator) > 0 .AND. Len(cHeaderSeparator) > 0
                cMethod += '"' + cFieldSeparator + '"+'
                ::__acSeparators[nIteration] := cHeaderSeparator
                --nSpaceLeft
            ELSE
                ::__acSeparators[nIteration] := ''
            ENDIF
        ENDIF

        ++nIteration
    NEXT

    IF nSpaceLeft > 0
        --nIteration
        nComma := RAt(',', cMethod)
        cMethod := Left(cMethod, nComma) + LTrim(Str(nSpaceLeft + Val(SubStr(cMethod, nComma + 1, Len(cMethod) - 2)))) + ')+'
        nComma := RAt(',', ::__acHeaders[nIteration])
        ::__acHeaders[nIteration] := Left(::__acHeaders[nIteration], nComma) + LTrim(Str(nSpaceLeft + Val(SubStr(::__acHeaders[nIteration];
                       , nComma + 1, Len(::__acHeaders[nIteration]) - 1)))) + ')'
    ENDIF

    AEval(::__acHeaders, {| cHeader, nIndex | ::__acHeaders[nIndex] := &(cHeader)})

RETURN {|| &(cMethod + '""')}

METHOD print_title() CLASS Row_browse

    LOCAL nColFrom

    DO CASE
        CASE ::__cTitleAlign == ALIGN_LEFT
            nColFrom := ::__nLeft
        CASE ::__cTitleAlign == ALIGN_CENTER
            nColFrom := Int((::__nRight + ::__nLeft - Len(::__cTitle)) / 2)
        CASE ::__cTitleAlign == ALIGN_RIGHT
            nColFrom := ::__nRight - Len(::__cTitle)
    ENDCASE

    @ ::__nTop, Max(nColFrom, ::__nLeft) SAY Left(::__cTitle, ::__nRight - ::__nLeft) COLOR ::__cTitleColor  

RETURN NIL

METHOD __linear_search(cTarget, lExactly, cPattern) CLASS Row_browse

    LOCAL nTargetLength := Len(cTarget)
    LOCAL lFound := .F.

    ::__oTBrowse:GoTop()

    IF cPattern == NIL
        cPattern := IndexKey()
    ENDIF

    IF lExactly
        DO WHILE !lFound
            IF EoF()
                EXIT
            ELSEIF cTarget == cast(&(cPattern), 'C')
                lFound := .T.
            ELSE
                SKIP
            ENDIF
        ENDDO
    ELSE
        DO WHILE !lFound
            IF EoF()
                EXIT
            ELSEIF cTarget == Left(cast(&(cPattern), 'C'), nTargetLength)
                lFound := .T.
            ELSE
                SKIP
            ENDIF
        ENDDO
    ENDIF

    IF !lFound
        SKIP -1
    ENDIF

RETURN lFound

METHOD __binary_search(cTarget, lExactly) CLASS Row_browse

    ::__oTBrowse:goTop()

    IF lExactly
        SEEK cTarget
    ELSE
        SEEK cTarget SOFTSEEK
    ENDIF

RETURN Found()

METHOD __go_first_visible() CLASS Row_browse

    LOCAL nCount := ::__oTBrowse:rowPos - 1

    DO WHILE nCount > 0
        ::__oTBrowse:Up()
        --nCount
    ENDDO

RETURN NIL

METHOD __go_last_visible() CLASS Row_browse

    LOCAL nCount := ::__oTBrowse:rowCount() - ::__oTBrowse:rowPos

    DO WHILE nCount > 0
        ::__oTBrowse:Down()
        --nCount
    ENDDO
    
RETURN NIL

METHOD search_keys(cSearchKeys) CLASS Row_browse

    LOCAL cOldSearchKeys := ::__cSearchKeys

    IF cSearchKeys != NIL
#ifdef USE_VALIDATORS
        assert_type(cSearchKeys, 'C')
#endif
        ::__cSearchKeys := cSearchKeys
    ENDIF

RETURN cOldSearchKeys

METHOD search(cTarget, lExactly, cPattern) CLASS Row_browse

#ifdef USE_VALIDATORS
    IF cTarget != NIL
        assert_type(cTarget, 'C')
    ENDIF
#endif

    IF lExactly == NIL
        lExactly := .F.
#ifdef USE_VALIDATORS
    ELSE
        assert_type(lExactly, 'L')
#endif
    ENDIF

#ifdef USE_VALIDATORS
    IF IndexOrd() == 0
        assert_type(cPattern, 'C')
    ENDIF
#endif

    IF IndexOrd() == 0 .OR. (cPattern != NIL .AND. IndexKey() != cPattern)
        RETURN ::__linear_search(cTarget, lExactly, cPattern)
    ENDIF

RETURN ::__binary_search(cTarget, lExactly)
