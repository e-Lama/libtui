#include "box.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "hbclass.ch"

#include "functions.ch"
#include "align.ch"

#include "setup.ch"

CREATE CLASS AlertLG

EXPORTED:

    METHOD AlertLG(xMessage, acOptions, cColorMessage, cColorButtons, nDelay, nRow;
                   , nLeft, nRight, nCurrentOption, lAllowEscape, lAllowMove, lCyclic;
                   , lAcceptFirstFounded, cBorder, cAlign, lAllowMouse;
                  )

    METHOD keys_map(hKeysMap) SETGET
    METHOD create_centered(lCreateCentered)

HIDDEN:

    CLASSVAR __lCreateCentered AS LOGICAL INIT .T.
    CLASSVAR __hKeysMap AS HASH INIT {;
                                    K_ALT_UP => K_ALT_UP, K_ALT_DOWN => K_ALT_DOWN;
                                    , K_ALT_RIGHT => K_ALT_RIGHT, K_ALT_LEFT => K_ALT_LEFT;
                                    , K_ALT_ENTER => K_ALT_ENTER, K_LEFT => K_LEFT;
                                    , K_RIGHT => K_RIGHT, K_ENTER => K_ENTER;
                              }

    METHOD __merged_options_length(acOptions)
    METHOD __word_length_from(cTxt, nFrom)
    METHOD __create_message(cTxt, nMaxWidth, cAlign, nAlertWidth, nOptionsLength)
    METHOD __find_letter(nKey, nCurrentOption, acOptions, lAcceptFirstFounded, lFound)
    METHOD __adjust(cTxt, nWidth)

#ifdef USE_VALIDATORS
    METHOD __keys_map_asserts(hKeysMap)
#endif

ENDCLASS LOCK

METHOD AlertLG(xMessage, acOptions, cColorMessage, cColorButtons, nDelay, nRow, nLeft;
               , nRight, nCurrentOption, lAllowEscape, lAllowMove, lCyclic, lAcceptFirstFounded;
               , cBorder, cAlign, lAllowMouse;
              ) CLASS AlertLG

    LOCAL nOldWindow := WSelect()
    LOCAL acOptionsTrimmed := Array(Len(acOptions))
    LOCAL anButtonsColFrom
    LOCAL anButtonsColTo
    LOCAL cOldColor
    LOCAL nOldCursor
    LOCAL cMessage
    LOCAL acMessage
    LOCAL nMessageHeight
    LOCAL nOptionsLength
    LOCAL nWidth
    LOCAL nKey
    LOCAL nShift
    LOCAL lFound
    LOCAL xResult
    LOCAL nMouseCol
    LOCAL nMouseRow
    LOCAL i

    WSelect(0)

#ifdef USE_VALIDATORS
    IF PCount() < 2 .OR. PCount() > 16
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ENDIF
#endif

    IF ValType(xMessage) == 'A'
        cMessage := ''
        FOR i := 1 TO Len(xMessage)
            cMessage += IF(i == 1, '', hb_OsNewLine()) + hb_CStr(xMessage[i])
        NEXT
    ELSEIF ValType(xMessage) == 'C'
        cMessage := StrTran(xMessage, ';', hb_OsNewLine())
    ELSE
        cMessage := hb_CStr(xMessage)
    ENDIF

    IF acOptions != NIL
#ifdef USE_VALIDATORS
        assert_type(acOptions, 'A')
        AEval(acOptions, {| cOption | assert_type(cOption, 'C')})
#endif
        
        FOR i := 1 TO Len(acOptions)
            acOptionsTrimmed[i] := AllTrim(acOptions[i])
        NEXT
    ENDIF

    IF cColorMessage == NIL
        cColorMessage := 'W+/R'
#ifdef USE_VALIDATORS
    ELSE
        assert_type(cColorMessage, 'C')

        IF !is_color(cColorMessage)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif
    ENDIF

    IF cColorButtons == NIL
        cColorMessage := 'W+/B'
#ifdef USE_VALIDATORS
    ELSE
        assert_type(cColorButtons, 'C')

        IF !is_color(cColorButtons)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif
    ENDIF

    IF nDelay == NIL
        nDelay := 0
#ifdef USE_VALIDATORS
    ELSE
        assert_type(nDelay, 'N')
        IF nDelay < 0
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif
    ENDIF

#ifdef USE_VALIDATORS
    IF nRow != NIL
        assert_type(nRow, 'N')
        IF nRow < 0 .OR. nRow > MaxRow()
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
    ENDIF
#endif

    IF nLeft == NIL
        nLeft := Int(MaxCol() / 3)
#ifdef USE_VALIDATORS
    ELSE
        assert_type(nLeft, 'N')
        IF nLeft < 0 .OR. nLeft > MaxCol()
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif
    ENDIF

    IF nRight == NIL
        nRight := Int(2 * MaxCol() / 3)
#ifdef USE_VALIDATORS
    ELSE
        assert_type(nLeft, 'N')
        IF nRight < 0 .OR. nRight > MaxCol()
            throw(ARGUMENT_VALUE_EXCEPTION)
        ELSEIF nRight <= nLeft + 5
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif
    ENDIF

    IF nCurrentOption == NIL
        nCurrentOption := 1
#ifdef USE_VALIDATORS
    ELSE
        assert_type(nCurrentOption, 'N')
        IF nCurrentOption < 0 .OR. nCurrentOption > Len(acOptionsTrimmed) + 1
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif
    ENDIF

    IF lAllowEscape == NIL
        lAllowEscape := .T.
#ifdef USE_VALIDATORS
    ELSE
        assert_type(lAllowEscape, 'L')
#endif
    ENDIF

    IF lAllowMove == NIL
        lAllowMove := .T.
#ifdef USE_VALIDATORS
    ELSE
        assert_type(lAllowMove, 'L')
#endif
    ENDIF

    IF lCyclic == NIL
        lCyclic := .T.
#ifdef USE_VALIDATORS
    ELSE
        assert_type(lCyclic, 'L')
#endif
    ENDIF

    IF lAcceptFirstFounded == NIL
        lAcceptFirstFounded := .F.
#ifdef USE_VALIDATORS
    ELSE
        assert_type(lAcceptFirstFounded, 'L')
#endif
    ENDIF

    IF cBorder == NIL
        cBorder := B_SINGLE
#ifdef USE_VALIDATORS
    ELSEIF ValType(cBorder) != 'C' .OR. !is_box(hb_Translate(cBorder, 'EN', hb_cdpSelect()))
        throw(ARGUMENT_VALUE_EXCEPTION)
#endif
    ENDIF

    IF cAlign == NIL
        cAlign := ALIGN_CENTER
#ifdef USE_VALIDATORS
    ELSEIF ValType(cAlign) != 'C' .OR. AScan({ALIGN_LEFT, ALIGN_CENTER, ALIGN_RIGHT, ALIGN_JUSTIFIED}, cAlign) == 0
        throw(ARGUMENT_VALUE_EXCEPTION)
#endif
    ENDIF

    IF lAllowMouse == NIL
        lAllowMouse := .T.
#ifdef USE_VALIDATORS
    ELSE
        assert_type(lAllowMouse, 'L')
#endif
    ENDIF

    nOldCursor := SetCursor(SC_NONE)
    cOldColor := SetColor(cColorMessage)

    nOptionsLength := ::__merged_options_length(acOptionsTrimmed)
    acMessage := ::__create_message(cMessage, Max(Min(nRight - nLeft - 4, Len(cMessage)), nOptionsLength), cAlign, nRight - nLeft - 4, nOptionsLength)
    nMessageHeight := Len(acMessage)

    IF nRow == NIL
        nRow := Int((MaxRow() - nMessageHeight - 2) / 2)
    ENDIF

    nWidth := Max(Max(max_of_array(length_array(acMessage)), nOptionsLength), Min(nRight - nLeft - 4, Len(cMessage)))

    WOpen(nRow, nLeft, nRow + nMessageHeight + 3, nLeft + nWidth + 4, .T.)
    WBox(cBorder)

    IF ::__lCreateCentered
        WCenter(.T.)
    ENDIF

    FOR i := 1 TO Len(acMessage)
        SetPos(i - 1, 2)
        QQOut(acMessage[i])
    NEXT

    DO WHILE .T.

        nShift := 0
        anButtonsColFrom := {}
        anButtonsColTo := {}

        DispBegin()
        FOR i := 1 TO Len(acOptionsTrimmed)
            SetPos(nMessageHeight + 1, Int((MaxCol() - nOptionsLength) / 2) + 4 * (i - 1) + nShift + 1)
            AAdd(anButtonsColFrom, Col())

            IF i == nCurrentOption
                SetColor(cColorButtons)
                DevOut(' ' + acOptionsTrimmed[i] + ' ')
                SetColor(cColorMessage)
            ELSE
                DevOut(' ' + acOptionsTrimmed[i] + ' ')
            ENDIF

            AAdd(anButtonsColTo, Col())

            QQOut('  ')
            nShift += Len(acOptionsTrimmed[i])
        NEXT
        DispEnd()

        nKey := Inkey(nDelay)
        nMouseCol := MCol()
        nMouseRow := MRow()

        IF nKey > 0 .AND. hb_HHasKey(::__hKeysMap, nKey)
            nKey := ::__hKeysMap[nKey]
        ENDIF

        IF nDelay > 0
            nDelay := 0
        ENDIF

        IF SetKey(nKey) != NIL
            xResult := EVal(SetKey(nKey))

            IF ValType(xResult) == 'N'
                nKey := xResult
            ENDIF
        ENDIF

        IF lAllowMouse
            DO CASE
                CASE nKey == K_MWBACKWARD
                    nKey := K_LEFT
                CASE nKey == K_MWFORWARD
                    nKey := K_RIGHT
                CASE nKey == K_LBUTTONDOWN
                    IF nMouseRow == nMessageHeight + 1
                        FOR i := 1 TO Len(anButtonsColFrom)
                            IF nMouseCol >= anButtonsColFrom[i] .AND. nMouseCol <= anButtonsColTo[i]
                                nCurrentOption := i
                                EXIT
                            ENDIF
                        NEXT
                    ELSEIF nMouseRow >= -1 .AND. nMouseCol >= -1 .AND. nMouseRow <= MaxRow() + 1 .AND. nMouseCol <= MaxCol() + 1

                        DO WHILE nKey != K_LBUTTONUP

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
            ENDCASE
        ENDIF
            
        DO CASE
            CASE nKey == K_LEFT
                IF nCurrentOption > 1
                    --nCurrentOption
                ELSEIF lCyclic
                    nCurrentOption := Len(acOptionsTrimmed)
                ENDIF
            CASE nKey == K_RIGHT
                IF nCurrentOption < Len(acOptionsTrimmed)
                    ++nCurrentOption
                ELSEIF lCyclic
                    nCurrentOption := 1
                ENDIF
            CASE nKey == K_ENTER
                EXIT
            CASE lAllowEscape .AND. nKey == K_ESC
                nCurrentOption := -1
                EXIT
            CASE lAllowMove .AND. nKey == K_ALT_UP
                WMove(WRow() - 1, WCol())
            CASE lAllowMove .AND. nKey == K_ALT_DOWN
                WMove(WRow() + 1, WCol())
            CASE lAllowMove .AND. nKey == K_ALT_RIGHT
                WMove(WRow(), WCol() + 1)
            CASE lAllowMove .AND. nKey == K_ALT_LEFT
                WMove(WRow(), WCol() - 1)
            CASE lAllowMove .AND. nKey == K_ALT_ENTER
                WCenter(.T.)
            CASE nKey == K_LBUTTONUP
                //Do nothing
            CASE nKey == K_LDBLCLK
                EXIT
            OTHERWISE
                lFound := .F.

                nCurrentOption := ::__find_letter(nKey, nCurrentOption, acOptionsTrimmed, lAcceptFirstFounded, @lFound)

                IF lFound .AND. lAcceptFirstFounded
                    EXIT
                ENDIF
        ENDCASE
    ENDDO

    WClose()
    WSelect(0)
    SetCursor(nOldCursor)
    SET COLOR TO (cOldColor)
    WSelect(nOldWindow)

RETURN nCurrentOption

METHOD __find_letter(nKey, nCurrentOption, acOptions, lAcceptFirstFounded, lFound) CLASS AlertLG

    LOCAL cKey := Upper(Chr(nKey))
    LOCAL i

    lFound := .F.

    FOR i := nCurrentOption + IF(lAcceptFirstFounded, 0, 1) TO Len(acOptions)
        IF cKey $ Upper(Left(acOptions[i], 1))
            lFound := .T.
            RETURN i
        ENDIF   
    NEXT

    FOR i := 1 TO nCurrentOption - IF(lAcceptFirstFounded, 1, 0)
        IF cKey $ Upper(Left(acOptions[i], 1))
            lFound := .T.
            RETURN i
        ENDIF
    NEXT

RETURN nCurrentOption

METHOD keys_map(hKeysMap) CLASS AlertLG

    LOCAL hOldKeysMap := hb_hClone(::__hKeysMap)

    IF hKeysMap != NIL
#ifdef USE_VALIDATORS
        ::__keys_map_asserts(hKeysMap)
#endif
        ::__hKeysMap := hKeysMap
    ENDIF

RETURN hOldKeysMap

METHOD create_centered(lCreateCentered) CLASS AlertLG

    LOCAL lOldCreateCentered := ::__lCreateCentered

    IF lCreateCentered != NIL
#ifdef USE_VALIDATORS
        assert_type(lCreateCentered, 'L')
#endif
        ::__lCreateCentered := lCreateCentered
    ENDIF

RETURN lOldCreateCentered

#ifdef USE_VALIDATORS
METHOD __keys_map_asserts(hKeysMap) CLASS AlertLG

    LOCAL nKey

    assert_type(hKeysMap, 'H')
    assert_length(hKeysMap, Len(::__hKeysMap))

    FOR EACH nKey IN hb_hKeys(hKeysMap)
        assert_type(nKey, 'N')
        assert_type(hKeysMap[nKey], 'N')

        IF nKey < 0 .OR. hKeysMap[nKey] < 0
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
    NEXT

RETURN NIL
#endif

METHOD __word_length_from(cTxt, nFrom) CLASS AlertLG

    LOCAL nLength := 0

    DO WHILE nFrom < Len(cTxt)
        IF SubStr(cTxt, nFrom++, 1) > ' '
            ++nLength
        ELSE
            EXIT
        ENDIF
    ENDDO

RETURN nLength

METHOD __create_message(cTxt, nMaxWidth, cAlign, nAlertWidth, nOptionsLength) CLASS AlertLG

    LOCAL acRows := {''}
    LOCAL nCurrentRow := 1
    LOCAL nCurrentWidth := 0
    LOCAL cCharacter

    IF cAlign != ALIGN_JUSTIFIED
        FOR EACH cCharacter IN cTxt
            IF cCharacter == Chr(10)
                nMaxWidth := Max(nMaxWidth, nCurrentWidth)
                AAdd(acRows, '') 
                ++nCurrentRow
                nCurrentWidth := 0
                LOOP
            ELSEIF nCurrentWidth >= nMaxWidth .AND. SubStr(acRows[nCurrentRow], Len(acRows[nCurrentRow]), 1) == ' '
                nMaxWidth := Max(nMaxWidth, nCurrentWidth)
                AAdd(acRows, cCharacter)
                ++nCurrentRow
                nCurrentWidth := 1
                LOOP
            ELSEIF SubStr(acRows[nCurrentRow], Len(acRows[nCurrentRow]), 1) == ' ';
                   .AND. ::__word_length_from(cTxt, cCharacter:__enumindex) + nCurrentWidth > nMaxWidth

                nMaxWidth := Max(nMaxWidth, nCurrentWidth)
                AAdd(acRows, cCharacter)
                ++nCurrentRow
                nCurrentWidth := 1
                LOOP
            ELSE
                acRows[nCurrentRow] += cCharacter
                ++nCurrentWidth
            ENDIF
        NEXT
    ENDIF

    DO CASE
        CASE cAlign == ALIGN_LEFT
            AEval(acRows, {| cLine, nIndex | acRows[nIndex] := PadL(RTrim(cLine), nMaxWidth)})
        CASE cAlign == ALIGN_CENTER
            AEval(acRows, {| cLine, nIndex | acRows[nIndex] := PadC(AllTrim(cLine), nMaxWidth)})
        CASE cAlign == ALIGN_RIGHT
            AEval(acRows, {| cLine, nIndex | acRows[nIndex] := PadR(LTrim(cLine), nMaxWidth)})
        CASE cAlign == ALIGN_JUSTIFIED
            acRows[1] := ::__adjust(AllTrim(cTxt), Max(nOptionsLength, Min(nAlertWidth, Len(cTxt))))
            acRows := hb_ATokens(acRows[1], hb_OsNewLine())
    ENDCASE

RETURN acRows

METHOD __merged_options_length(acOptions) CLASS AlertLG

    LOCAL nLength := 0
    LOCAL cOption

    FOR EACH cOption IN acOptions
        nLength += Len(cOption) + 4
    NEXT

RETURN nLength

METHOD __adjust(cTxt, nWidth) CLASS AlertLG

    LOCAL acWords := hb_ATokens(cTxt, ' ')
    LOCAL cAlignedTxt := ''
    LOCAL nLineLength := 0
    LOCAL nFirstWordInLine := 1
    LOCAL nLastWordInLine := 1
    LOCAL lSeparated := .F.
    LOCAL nCurrentWordLength
    LOCAL nDifference
    LOCAL cWord
    LOCAL i

    FOR EACH cWord IN acWords

        nCurrentWordLength := Len(cWord)

        IF nLineLength + nCurrentWordLength + 1 <= nWidth
            nLineLength += nCurrentWordLength + 1
            lSeparated := .F.
        ELSE
            IF nLastWordInLine - nFirstWordInLine - 1 > 0
                FOR i := nFirstWordInLine TO nLastWordInLine - 1
                    acWords[i] += ' '
                NEXT

                nDifference := nWidth - nLineLength

                DO WHILE nDifference >= nLastWordInLine - nFirstWordInLine - 1
                    FOR i := nFirstWordInLine TO nLastWordInLine - 1
                        acWords[i] += ' '
                    NEXT
                    nDifference -= nLastWordInLine - nFirstWordInLine - 1
                ENDDO

                FOR i := nFirstWordInLine TO nFirstWordInLine + nDifference
                    acWords[i] += ' '
                NEXT
            ENDIF

            acWords[nLastWordInLine] := hb_OsNewLine() + acWords[nLastWordInLine]

            nLineLength := nCurrentWordLength + 1
            nFirstWordInLine := nLastWordInLine
            lSeparated := .T.
        ENDIF

        ++nLastWordInLine
    NEXT

    IF !lSeparated
#pragma ENABLEWARNINGS = Off //unused variable cWord
        AEval(acWords, {| cWord, nIndex | acWords[nIndex] += ' '}, nFirstWordInLine, nLastWordInLine)
#pragma ENABLEWARNINGS = On
    ENDIF

    AEval(acWords, {| cWord | cAlignedTxt += cWord})

    IF lSeparated
        cAlignedTxt := Left(cAlignedTxt, Len(cAlignedTxt))
    ELSE
        cAlignedTxt := Left(cAlignedTxt, Len(cAlignedTxt) - 1)
    ENDIF

RETURN cAlignedTxt
