#include "inkey.ch"
#include "hbclass.ch"

#include "rowbrowse.ch"
#include "align.ch"
#include "functions.ch"

CREATE CLASS Row_browse

EXPORTED:

    METHOD new(cTableID, nTop, nLeft, nBottom, nRight, cColor, cBorder, cTitle, cTitleAlign;
               , cTitleColor, nStartRow, bUserAction, bColorBlock, acHeadersColors, hKeysMap, xCargo;
               , lAutoHighlight, bSkip, bGoBottom, bGoTop) CONSTRUCTOR

    //Row_browse extensions
    METHOD display(lEnd)
    METHOD finish() INLINE ::lActive := .F.

    METHOD title(cTitle) SETGET
    METHOD top(nTop) SETGET                                         // It's not TBrowse top
    METHOD left(nLeft) SETGET                                       // It's not TBrowse left
    METHOD right(nRight) SETGET                                     // It's not TBrowse right
    METHOD bottom(nBottom) SETGET                                   // It's not TBrowse bottom
    METHOD keys_map(hKeysMap) SETGET
    METHOD header_color(nIndex, cColor) SETGET
    METHOD get_separator(nIndex) 
    METHOD get_header(nIndex)

    METHOD home() INLINE IF(::lActive, ::go_first_visible(::oTBrowse:rowPos), throw(RUNTIME_EXCEPTION))
    METHOD end() INLINE IF(::lActive, ::go_last_visible(::oTBrowse:rowPos, ::oTBrowse:rowCount()), throw(RUNTIME_EXCEPTION))

    METHOD search(cTarget, lExactly, cPattern)
    METHOD search_keys(cSearchKeys)

    METHOD print_title()
    METHOD draw_border()

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
    METHOD down() INLINE IF(::lActive, (::oTBrowse:down(), NIL), throw(RUNTIME_EXCEPTION))                       // down() wrapper
    METHOD up() INLINE IF(::lActive, (::oTBrowse:up(), NIL), throw(RUNTIME_EXCEPTION))                           // up() wrapper
    METHOD go_top() INLINE IF(::lActive, (::oTBrowse:goTop(), NIL), throw(RUNTIME_EXCEPTION))                    // goTop() wrapper
    METHOD go_bottom() INLINE IF(::lActive, (::oTBrowse:goBottom(), NIL), throw(RUNTIME_EXCEPTION))              // goBottom() wrapper
    METHOD page_down() INLINE IF(::lActive, (::oTBrowse:pageDown(), NIL), throw(RUNTIME_EXCEPTION))              // pageDown() wrapper
    METHOD page_up() INLINE IF(::lActive, (::oTBrowse:pageUp(), NIL), throw(RUNTIME_EXCEPTION))                  // pageUp() wrapper
    METHOD configure() INLINE IF(::lActive, (::oTBrowse:configure(), NIL), throw(RUNTIME_EXCEPTION))             // configure() wrapper
    METHOD dehighlight() INLINE IF(::lActive, (::oTBrowse:deHilite(), NIL), throw(RUNTIME_EXCEPTION))            // deHilite() wrapper
    METHOD force_stable() INLINE IF(::lActive, (::oTBrowse:forceStable(), NIL), throw(RUNTIME_EXCEPTION))        // forceStable() wrapper
    METHOD highlight() INLINE IF(::lActive, (::oTBrowse:hilite(), NIL), throw(RUNTIME_EXCEPTION))                // Hilite() wrapper
    METHOD invalidate() INLINE IF(::lActive, (::oTBrowse:invalidate(), NIL), throw(RUNTIME_EXCEPTION))           // invalidate() wrapper
    METHOD refresh_all() INLINE IF(::lActive, (::oTBrowse:refreshAll(), NIL), throw(RUNTIME_EXCEPTION))          // refreshAll() wrapper
    METHOD refresh_current() INLINE IF(::lActive, (::oTBrowse:refreshCurrent(), NIL), throw(RUNTIME_EXCEPTION))  // refreshCurrent() wrapper
    METHOD stabilize() INLINE IF(::lActive, (::oTBrowse:stabilize(), NIL), throw(RUNTIME_EXCEPTION))             // stabilize() wrapper
    METHOD color_rectangle(anRows, anColors)                                                                     // colorRect() wrapper
    METHOD hit_test(nRow)                                                                                        // HitTest() wrapper

HIDDEN:

    VAR lActive AS LOGICAL INIT .F.
    VAR cTitle AS CHARACTER INIT ''
    VAR cTitleAlign AS CHARACTER INIT ALIGN_CENTER
    VAR cTitleColor AS CHARACTER INIT ''
    VAR nStartRow AS NUMERIC INIT 0
    VAR bUserAction AS CODEBLOCK INIT {|| ROWBROWSE_NOTHING}
    VAR bColorBlock AS CODEBLOCK INIT {|| nothing()}
    VAR acHeadersColors AS ARRAY INIT Array(0)
    VAR cKeys AS CHARACTER INIT ''
    VAR bSkipBlock AS CODEBLOCK INIT {| nRecs | __dbSkipper(nRecs)} //src/rtl/browdb.prg
    VAR bGoBottom AS CODEBLOCK INIT {|| dbGoBottom()}
    VAR bGoTop AS CODEBLOCK INIT {|| dbGoTop()}
    VAR nRefresh AS NUMERIC INIT ALWAYS_REFRESH_CURRENT
    VAR nReprint AS CHARACTER INIT REPRINT_ONCE
    VAR acHeaders AS ARRAY INIT Array(0)
    VAR acSeparators AS ARRAY INIT Array(0)
    VAR cSearchKeys AS CHARACTER INIT ''
    VAR hKeysMap AS HASH INIT {;
                                    K_ESC => K_ESC, K_DOWN => K_DOWN, K_UP => K_UP, K_PGUP => K_PGUP;
                                    , K_PGDN => K_PGDN, K_LEFT => K_LEFT, K_RIGHT => K_RIGHT;
                                    , K_HOME => K_HOME, K_END => K_END, K_CTRL_HOME => K_CTRL_HOME;
                                    , K_CTRL_END => K_CTRL_END;
                               }
    VAR cColor AS CHARACTER INIT Config():get_config('DefaultColor')
    VAR cBorder AS CHARACTER INIT Config():get_config('RowBrowseDefaultBox')
    VAR oTBrowse AS OBJECT 
    VAR cTableID AS CHARACTER
    VAR nTop AS NUMERIC
    VAR nLeft AS NUMERIC
    VAR nBottom AS NUMERIC
    VAR nRight AS NUMERIC 
    VAR lOldHitBottom AS LOGICAL
    VAR lOldHitTop AS LOGICAL
    VAR lStable AS LOGICAL
    VAR lSpecialOperationRefresh AS LOGICAL
    VAR xCargo

    METHOD is_align(cAlign) INLINE ValType(cAlign) == 'C' .AND. AScan({ALIGN_LEFT, ALIGN_CENTER, ALIGN_RIGHT}, cAlign) > 0
    METHOD extract_separator(cSeparator) INLINE IF(Empty(Left(cSeparator, 1)), '', Right(cSeparator, 1))
    METHOD linear_search(cTarget, lExactly, cPattern)
    METHOD binary_search(cTarget, lExactly)
    METHOD handle_action(nAction)
    METHOD keys_map_asserts(hKeysMap)
    METHOD validate(axRow)
    METHOD handle_move(nKey)
    METHOD how_to_browse()
    METHOD get_print_function()
    METHOD go_first_visible()
    METHOD go_last_visible()
    METHOD print_header()
    METHOD refresh()

ENDCLASS LOCK

METHOD get_separator(nIndex) CLASS Row_browse

    assert_type(nIndex, 'N')

    IF nIndex < 1 .OR. nIndex > Len(::acSeparators) .OR. Int(nIndex) != nIndex
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF

RETURN ::acSeparators[nIndex]

METHOD get_header(nIndex) CLASS Row_browse

    assert_type(nIndex, 'N')

    IF nIndex < 1 .OR. nIndex > Len(::acHeaders) .OR. Int(nIndex) != nIndex
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF

RETURN ::acHeaders[nIndex]

METHOD color_rectangle(anRows, anColors) CLASS Row_browse

    LOCAL nNumber

    IF !::lActive
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
        //TODO nNumber < 1 or nNumber < 0? 
        IF nNumber < 0 .OR. nNumber > Len(::cColor)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
    NEXT
    
    ::oTBrowse:colorRect({anRows[1], 1, anRows[2], 1}, anColors)

RETURN NIL

METHOD hit_test(nRow) CLASS Row_browse

    assert_type(nRow, 'N')

    IF nRow < 1
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF

RETURN ::oTBrowse:hitTest(nRow, 1)

METHOD header_color(nIndex, cColor) CLASS Row_browse

    LOCAL cOldColor

    assert_type(nIndex, 'N')

    IF nIndex < 1 .OR. nIndex > Len(::acHeadersColors) .OR. Int(nIndex) != nIndex
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF

    cOldColor := ::acHeadersColors[nIndex]

    IF cColor != NIL
        assert_type(cColor, 'C')
        IF !Empty(cColor) .AND. !is_color(cColor)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
        
        ::acHeadersColors[nIndex] := cColor
    ENDIF

RETURN cOldColor

METHOD color_block(bColorBlock) CLASS Row_browse
    
    LOCAL bOldColorBlock := ::bColorBlock

    IF bColorBlock != NIL
        assert_type(bColorBlock, 'B')
        ::bColorBlock := bColorBlock
    ENDIF

RETURN bOldColorBlock

METHOD row_position(nRowPosition) CLASS Row_browse

    LOCAL nOldRowPosition := ::oTBrowse:rowPos()

    IF nRowPosition != NIL
        assert_type(nRowPosition, 'N')

        IF nRowPosition < 0
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF

        ::oTBrowse:rowPos(nRowPosition)
    ENDIF

RETURN nOldRowPosition

METHOD keys_map_asserts(hKeysMap) CLASS Row_browse

    LOCAL nKey

    assert_type(hKeysMap, 'H')
    assert_length(hKeysMap, Len(::hKeysMap))

    FOR EACH nKey IN hb_hKeys(hKeysMap)
        assert_type(nKey, 'N')
        assert_type(hKeysMap[nKey], 'N')

        IF nKey < 0 .OR. hKeysMap[nKey] < 0
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
    NEXT

RETURN NIL

METHOD keys_map(hKeysMap) CLASS Row_browse

    LOCAL hOldKeysMap := hb_hClone(::hKeysMap)

    IF hKeysMap != NIL
        ::keys_map_asserts(hKeysMap)
        ::hKeysMap := hKeysMap
    ENDIF

RETURN hOldKeysMap

METHOD top(nTop) CLASS Row_browse

    LOCAL nOldTop := ::nTop

    IF nTop != NIL
        assert_type(nTop, 'N')

        IF nTop > ::nBottom
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF

        ::nTop := nTop
    ENDIF

RETURN nOldTop

METHOD left(nLeft) CLASS Row_browse

    LOCAL nOldLeft := ::nLeft

    IF nLeft != NIL
        assert_type(nLeft, 'N')

        IF nLeft > ::nRight
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF

        ::nLeft := nLeft
    ENDIF

RETURN nOldLeft

METHOD bottom(nBottom) CLASS Row_browse

    LOCAL nOldBottom := ::nBottom

    IF nBottom != NIL
        assert_type(nBottom, 'N')

        IF ::nTop > nBottom
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF

        ::nBottom := nBottom
    ENDIF

RETURN nOldBottom

METHOD right(nRight) CLASS Row_browse

    LOCAL nOldRight := ::nRight

    IF nRight != NIL
        assert_type(nRight, 'N')

        IF ::nLeft > nRight
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF

        ::nRight := nRight
    ENDIF

RETURN nOldRight

METHOD title(cTitle) CLASS Row_browse

    LOCAL cOldTitle := ::cTitle

    IF cTitle != NIL
        assert_type(cTitle, 'C')
        ::cTitle := cTitle
    ENDIF

RETURN cOldTitle

METHOD automatic_highlight(lAutomaticHighlight) CLASS Row_browse

    LOCAL lOldAutomaticHighlight := ::oTBrowse:autoLite

    IF lAutomaticHighlight != NIL
        assert_type(lAutomaticHighlight, 'L')
        ::oTBrowse:autoLite := lAutomaticHighlight
    ENDIF

RETURN lOldAutomaticHighlight

METHOD border(cBorder) CLASS Row_browse

    LOCAL cOldBorder := ::cBorder

    IF cBorder != NIL
        assert_type(cBorder, 'C')
        IF is_box(cBorder)
            ::cBorder := cBorder
        ELSE
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
    ENDIF

RETURN cOldBorder

METHOD cargo(xCargo) CLASS Row_browse

    LOCAL xOldCargo := ::xCargo

    IF xCargo != NIL
        ::xCargo := xCargo
    ENDIF

RETURN xOldCargo

METHOD color(cColor) CLASS Row_browse

    LOCAL cOldColor := ::cColor

    IF cColor != NIL
        assert_type(cColor, 'C')
        IF is_color(cColor)
            ::cColor := cColor
        ELSE
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
    ENDIF

RETURN cOldColor

METHOD hit_bottom(lHitBottom) CLASS Row_browse

    LOCAL lOldHitBottom := ::lHitBottom

    IF lHitBottom != NIL
        assert_type(lHitBottom, 'L')
        ::lHitBottom := lHitBottom
    ENDIF

RETURN lOldHitBottom

METHOD hit_top(lHitTop) CLASS Row_browse

    LOCAL lOldHitTop := ::lHitTop

    IF lHitTop != NIL
        assert_type(lHitTop, 'L')
        ::lHitTop := lHitTop
    ENDIF

RETURN lOldHitTop

METHOD mouse_column_position(nMouseColumnPosition) CLASS Row_browse

    LOCAL nOldMouseColumnPosition := ::nMouseColumnPosition

    IF nMouseColumnPosition != NIL
        assert_type(nMouseColumnPosition, 'N')
        ::nMouseColumnPosition := nMouseColumnPosition
    ENDIF

RETURN nOldMouseColumnPosition

METHOD mouse_row_position(nMouseRowPosition) CLASS Row_browse

    LOCAL nOldMouseRowPosition := ::nMouseRowPosition

    IF nMouseRowPosition != NIL
        assert_type(nMouseRowPosition, 'N')
        ::nMouseRowPosition := nMouseRowPosition
    ENDIF

RETURN nOldMouseRowPosition

METHOD skip_block(bSkipBlock) CLASS Row_browse

    LOCAL bOldSkipBlock := ::bSkipBlock

    IF bOldSkipBlock != NIL
        assert_type(bSkipBlock, 'B')
        ::bSkipBlock := bSkipBlock
    ENDIF

RETURN bOldSkipBlock

METHOD go_bottom_block(bGoBottom) CLASS Row_browse

    LOCAL bOldGoBottom:= ::bGoBottom

    IF bGoBottom != NIL
        assert_type(bGoBottom, 'B')
        ::bGoBottom := bGoBottom
    ENDIF

RETURN bOldGoBottom

METHOD go_top_block(bGoTop) CLASS Row_browse

    LOCAL bOldGoTop:= ::bGoTop

    IF bGoTop != NIL
        assert_type(bGoTop, 'B')
        ::bGoTop := bGoTop
    ENDIF

RETURN bOldGoTop

METHOD stable(lStable) CLASS Row_browse

    LOCAL lOldStable := ::lStable

    IF lStable != NIL
        assert_type(lStable, 'L')
        ::lStable := lStable
    ENDIF

RETURN lOldStable

 METHOD new(cTableID, nTop, nLeft, nBottom, nRight, cColor, cBorder, cTitle, cTitleAlign;
               , cTitleColor, nStartRow, bUserAction, bColorBlock, acHeadersColors, hKeysMap, xCargo;
               , lAutoHighlight, bSkip, bGoBottom, bGoTop) CLASS Row_browse

    assert_type(cTableID, 'C')
    assert_type(nTop, 'N')
    assert_type(nLeft, 'N')
    assert_type(nBottom, 'N')
    assert_type(nRight, 'N')

    ::cTableID := cTableID

    IF nTop >= nBottom .OR. nRight <= nLeft
        throw(ARGUMENT_VALUE_EXCEPTION)
    ENDIF

    ::nTop := nTop
    ::nLeft := nLeft
    ::nBottom := nBottom
    ::nRight := nRight

    IF cColor != NIL
        assert_type(cColor, 'C')

        IF !is_color(cColor)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF

        ::cColor := cColor
    ENDIF

    IF cBorder != NIL
        assert_type(cBorder, 'C')

        IF !is_box(cBorder)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF

        ::cBorder := cBorder
    ENDIF

    IF cTitle != NIL
        assert_type(cTitle, 'C')
        ::cTitle := cTitle
    ENDIF

    IF cTitleAlign != NIL
        assert_type(cTitleAlign, 'C')

        IF !::is_align(cTitleAlign)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF

        ::cTitleAlign := cTitleAlign
    ENDIF

    IF cTitleColor != NIL
        assert_type(cTitleColor, 'C')

        IF !is_color(cTitleColor)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF

        ::cTitleColor := cTitleColor
    ENDIF

    IF nStartRow != NIL
        assert_type(nStartRow, 'N')

        IF nStartRow < 0
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF

        ::nStartRow := nStartRow
    ENDIF

    IF bUserAction != NIL
        assert_type(bUserAction, 'B')
        ::bUserAction := bUserAction
    ENDIF

    IF bColorBlock != NIL
        assert_type(bColorBlock, 'B')
        ::bColorBlock := bColorBlock
    ENDIF

    IF ValType(acHeadersColors) == 'A'
        AEval(acHeadersColors, {| cColor | assert_type(cColor, 'C'), IF(!Empty(cColor) .AND. !is_color(cColor), throw(ARGUMENT_VALUE_EXCEPTION), )})
        ::acHeadersColors := acHeadersColors
    ELSEIF ValType(acHeadersColors) != 'U'
        throw(ARGUMENT_TYPE_EXCEPTION)
    ENDIF

    IF hKeysMap != NIL
        ::keys_map_asserts(hKeysMap)
        ::hKeysMap := hKeysMap
    ENDIF

    ::xCargo := xCargo

    IF lAutoHighlight != NIL
        assert_type(lAutoHighlight, 'L')
        ::lAutoHighlight := lAutoHighlight
    ENDIF

    IF bSkip != NIL
        assert_type(bSkip, 'B')
        ::bSkip := bSkip
    ENDIF

    IF bGoBottom != NIL
        assert_type(bGoBottom, 'B')
        ::bGoBottom := bGoBottom
    ENDIF

    IF bGoTop != NIL
        assert_type(bGoTop, 'B')
        ::bGoTop := bGoTop
    ENDIF

RETURN Self

METHOD display(lEnd)

    LOCAL bRowPrint := ::get_print_function()
    LOCAL lRestart := .F.
    LOCAL nAction
    LOCAL nKey

    IF lEnd != NIL
        assert_type(lEnd, 'L')
    ELSE
        lEnd := .F.
    ENDIF

    ::oTBrowse := TBrowseDB(::nTop + IF(::nReprint == DO_NOT_REPRINT, 1, 2), ::nLeft + 1, ::nBottom - 1, ::nRight - 1)
    ::oTBrowse:addColumn(TBColumnNew(NIL, bRowPrint))
    ::oTBrowse:colorSpec := ::cColor
    ::oTBrowse:GoTopBlock := ::bGoTop
    ::oTBrowse:GoBottomBlock := ::bGoBottom
    ::oTBrowse:SkipBlock := ::bSkipBlock
    ::oTBrowse:GetColumn(1):ColorBlock := ::bColorBlock

    ::lActive := .T.

    ::draw_border()
    ::print_title()

    SKIP ::nStartRow

    IF lEnd
        ::oTBrowse:ForceStable()
    ELSE
        DO WHILE !lEnd .AND. ::lActive

            ::oTBrowse:ForceStable()

            ::print_header()

            nKey := Inkey(0)

            IF hb_HHasKey(::hKeysMap, nKey)
                nKey := ::hKeysMap[nKey]
            ENDIF

            nAction := ::handle_move(nKey)

            IF nAction != ROWBROWSE_NO_ACTION .AND. nAction != ROWBROWSE_END
                nAction := ::handle_action(Eval(::bUserAction, Self, nKey))
            ENDIF

            IF nAction == ROWBROWSE_END
                lEnd := .T.
            ELSEIF nAction == ROWBROWSE_RESTART
                lEnd := .T.
                lRestart := .T.
            ENDIF

            IF !lEnd
                ::refresh()
            ENDIF
        ENDDO
    ENDIF

    ::lActive := .F.

    IF lRestart
        ::display()
    ENDIF

RETURN NIL

METHOD refresh() CLASS Row_browse

    DO CASE
        CASE ::lSpecialOperationRefresh
            ::oTBrowse:refreshAll()
            ::lSpecialOperationRefresh := .F.
        CASE ::nRefresh == ALWAYS_REFRESH_ALL
            ::oTBrowse:refreshAll()
        CASE ::nRefresh == ALWAYS_REFRESH_CURRENT
            ::oTBrowse:refreshCurrent()
        CASE ::nRefresh == REFRESH_ALL_ONCE
            ::oTBrowse:refreshAll()
            ::nRefresh := DO_NOT_REFRESH
        CASE ::nRefresh == REFRESH_CURRENT_ONCE
            ::oTBrowse:refreshCurrent()
            ::nRefresh := DO_NOT_REFRESH
        OTHERWISE
            throw(ARGUMENT_VALUE_EXCEPTION)
    ENDCASE

RETURN NIL

METHOD print_header() CLASS Row_browse

    LOCAL nCol := ::nLeft + 1
    LOCAL nColorsLen := Len(::acHeadersColors)
    LOCAL i

    FOR i := 1 TO Len(::acHeaders)
        IF i <= nColorsLen .AND. !Empty(::acHeadersColors[i])
            @ ::nTop + 1, nCol SAY ::acHeaders[i] COLOR ::acHeadersColors[i]
        ELSE
            @ ::nTop + 1, nCol SAY ::acHeaders[i] 
        ENDIF
        
        nCol += Len(::acHeaders[i])

        IF i < Len(::acHeaders)
            @ ::nTop + 1, nCol SAY ::acSeparators[i]
            ++nCol
        ENDIF
    NEXT

RETURN NIL

METHOD handle_move(nKey) CLASS Row_browse

    DO CASE
        CASE SetKey(nKey) != NIL
            Eval(SetKey(nKey))
        CASE nKey == K_ESC
            RETURN ROWBROWSE_END
        CASE nKey == K_DOWN
            ::oTBrowse:Down()
        CASE nKey == K_UP
            ::oTBrowse:Up()
        CASE nKey == K_PGUP
            ::oTBrowse:PageUp()
        CASE nKey == K_PGDN
            ::oTBrowse:PageDown()
        CASE nKey == K_LEFT
            ::oTBrowse:Left()
        CASE nKey == K_RIGHT
            ::oTBrowse:Right()
        CASE nKey == K_HOME
            ::go_first_visible()
        CASE nKey == K_END
            ::go_last_visible()
        CASE nKey == K_CTRL_HOME
            ::oTBrowse:GoTop()
        CASE nKey == K_CTRL_END
            ::oTBrowse:GoBottom()
    ENDCASE 

RETURN ROWBROWSE_NOTHING

METHOD handle_action(nAction) CLASS Row_browse

    MEMVAR cKeys

    DO CASE
        CASE nAction == ROWBROWSE_NOTHING
        CASE nAction == ROWBROWSE_END
        CASE nAction == ROWBROWSE_CLEAR_SEARCH_STRING
            cKeys := ''
            ::draw_border()
            ::print_title()
        CASE nAction == ROWBROWSE_REBORDER
            ::draw_border()
            ::print_title()
        CASE nAction == ROWBROWSE_SEARCH
            ::lSpecialOperationRefresh := .T.
        OTHERWISE
            throw(ARGUMENT_VALUE_EXCEPTION)
    ENDCASE

RETURN nAction

METHOD how_to_browse() CLASS Row_browse

    LOCAL nOldSelect := Select()
    LOCAL axHowTo := Array(0)

    IF Select('DBROWBROWSE') == 0
        throw(RUNTIME_EXCEPTION)
    ENDIF

    SELECT dbRowBrowse

    IF IndexOrd() == 0
        throw(RUNTIME_EXCEPTION)
    ENDIF

    SEEK ::cTableID

    IF !Found()
        throw(RUNTIME_EXCEPTION)
    ENDIF

    DO WHILE RTrim(field->id) == ::cTableID .AND. !Eof()
        AAdd(axHowTo, {;
                            field->col_nr, field->width, field->relative, field->fld_sep, field->fld_align;
                            , RTrim(field->head), field->head_sep, field->head_align, RTrim(field->print);
                      };
            )
        SKIP
    ENDDO

    SELECT (nOldSelect)

RETURN axHowTo

METHOD validate(axRow) CLASS Row_browse

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

    IF !::is_align(axRow[C_FIELD_ALIGN]) .OR. !::is_align(axRow[C_HEADER_ALIGN])
        RETURN .F.
    ENDIF

    IF Empty(axRow[C_PRINT_FNC])
        RETURN .F.
    ENDIF

RETURN .T.

METHOD get_print_function() CLASS Row_browse

    LOCAL axHowTo := ::how_to_browse()
    LOCAL nSpaceLeft := ::nRight - ::nLeft - 1
    LOCAL cMethod := ''
    LOCAL nIteration := 1
    LOCAL cFieldSeparator
    LOCAL cHeaderSeparator
    LOCAL nStringLength
    LOCAL nComma
    LOCAL axRow

    ASize(::acHeaders, Len(axHowTo))
    ASize(::acSeparators, Len(axHowTo) - 1)

    FOR EACH axRow IN axHowTo

        IF !::validate(axRow)
            throw(RUNTIME_EXCEPTION)
        ENDIF

        cFieldSeparator := ::extract_separator(axRow[C_FIELD_SEPARATOR])
        cHeaderSeparator := ::extract_separator(axRow[C_HEADER_SEPARATOR])

        IF !Empty(cFieldSeparator) .AND. Empty(cHeaderSeparator)
            cHeaderSeparator := ' '
        ELSEIF Empty(cFieldSeparator) .AND. !Empty(cHeaderSeparator)
            cFieldSeparator := ' '
        ENDIF

        IF axRow[L_RELATIVE]
            nStringLength := Min(Round(axRow[N_FIELD_WIDTH] * (::nRight - ::nLeft - 1) / 100.0, 0), nSpaceLeft)
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
                ::acHeaders[nIteration] := 'PadR("' + axRow[C_HEADER] + '",' + LTrim(Str(nStringLength)) + ')'
            CASE axRow[C_HEADER_ALIGN] == ALIGN_CENTER
                ::acHeaders[nIteration] := 'PadC("' + axRow[C_HEADER] + '",' + LTrim(Str(nStringLength)) + ')'
            CASE axRow[C_HEADER_ALIGN] == ALIGN_RIGHT
                ::acHeaders[nIteration] := 'PadL("' + axRow[C_HEADER] + '",' + LTrim(Str(nStringLength)) + ')'
        ENDCASE

        IF nSpaceLeft > 0 .AND. nIteration != Len(axHowTo) .AND. Len(cFieldSeparator) > 0 .AND. Len(cHeaderSeparator) > 0
            cMethod += '"' + cFieldSeparator + '"+'
            ::acSeparators[nIteration] := cHeaderSeparator
            --nSpaceLeft
        ELSEIF nIteration != Len(axHowTo)
            ::acSeparators[nIteration] := ''
        ENDIF

        ++nIteration
    NEXT

    IF nSpaceLeft > 0
        --nIteration
        nComma := RAt(',', cMethod)
        cMethod := Left(cMethod, nComma) + LTrim(Str(nSpaceLeft + Val(SubStr(cMethod, nComma + 1, Len(cMethod) - 2)))) + ')+'
        nComma := RAt(',', ::acHeaders[nIteration])
        ::acHeaders[nIteration] := Left(::acHeaders[nIteration], nComma) + LTrim(Str(nSpaceLeft + Val(SubStr(::acHeaders[nIteration];
                       , nComma + 1, Len(::acHeaders[nIteration]) - 1)))) + ')'
    ENDIF

    AEval(::acHeaders, {| cHeader, nIndex | ::acHeaders[nIndex] := &(cHeader)})

RETURN {|| &(cMethod + '""')}

METHOD print_title() CLASS Row_browse

    LOCAL nColFrom

    DO CASE
        CASE ::cTitleAlign == ALIGN_LEFT
            nColFrom := ::nLeft
        CASE ::cTitleAlign == ALIGN_CENTER
            nColFrom := Int((::nRight + ::nLeft - Len(::cTitle)) / 2)
        CASE ::cTitleAlign == ALIGN_RIGHT
            nColFrom := ::nRight - Len(::cTitle)
    ENDCASE

    @ ::nTop, Max(nColFrom, ::nLeft) SAY Left(::cTitle, ::nRight - ::nLeft) COLOR ::cTitleColor  

RETURN NIL

METHOD draw_border() CLASS Row_browse
    @ ::nTop, ::nLeft, ::nBottom, ::nRight BOX ::cBorder COLOR ::cColor
RETURN NIL

METHOD linear_search(cTarget, lExactly, cPattern) CLASS Row_browse

    LOCAL nTargetLength := Len(cTarget)
    LOCAL lFound := .F.

    ::oTBrowse:GoTop()

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

METHOD binary_search(cTarget, lExactly) CLASS Row_browse

    IF lExactly
        SEEK cTarget
    ELSE
        SEEK cTarget SOFTSEEK
    ENDIF

    ::lWasBinarySearch := .T.

RETURN Found()

METHOD go_first_visible() CLASS Row_browse

    ::oTBrowse:dehilite()
    SKIP (-1) * (::oTBrowse:rowPos - 1)

RETURN NIL

METHOD go_last_visible() CLASS Row_browse

    LOCAL nCount := ::oTBrowse:rowCount() - ::oTBrowse:rowPos

    DO WHILE nCount > 0
        ::oTBrowse:Down()
        --nCount
    ENDDO
    
RETURN NIL

METHOD search_keys(cSearchKeys) CLASS Row_browse

    LOCAL cOldSearchKeys := ::cSearchKeys

    IF cSearchKeys != NIL
        assert_type(cSearchKeys, 'C')
        ::cSearchKeys := cSearchKeys
    ENDIF

RETURN cOldSearchKeys

METHOD search(cTarget, lExactly, cPattern) CLASS Row_browse

    IF cTarget != NIL
        assert_type(cTarget, 'C')
    ENDIF

    IF lExactly != NIL
        assert_type(lExactly, 'L')
    ELSE
        lExactly := .F.
    ENDIF

    IF IndexOrd() == 0
        assert_type(cPattern, 'C')
    ENDIF

    IF IndexOrd() == 0 .OR. (cPattern != NIL .AND. IndexKey() != cPattern)
        RETURN ::linear_search(cTarget, lExactly, cPattern)
    ENDIF

RETURN ::linear_search(cTarget, lExactly, cPattern)//::binary_search(cTarget, lExactly) TODO: binary search doesn't work with :ForceStable method
