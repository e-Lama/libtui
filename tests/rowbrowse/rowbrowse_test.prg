#include "inkey.ch"

#include "rowbrowse.ch"

PROCEDURE main()

    LOCAL oRowBrowse
    LOCAL hKeysMap := {;
                                    K_ESC => K_ESC, K_DOWN => K_DOWN, K_UP => K_UP, K_PGUP => K_PGUP;
                                    , K_PGDN => K_PGDN, K_HOME => K_HOME, K_END => K_END, K_CTRL_HOME => K_CTRL_HOME;
                                    , K_ENTER => K_CTRL_END;
                               }
    USE dbRowBrowse NEW
    INDEX ON field->id + Str(field->col_nr) TO indRowBrowse

    SET KEY K_F4 TO alice_and_bob()
    SET CURSOR OFF
    CLS

    @ 0, 0 SAY '0         1         2         3         4         5         6         7         8'
    @ 1, 0 SAY '012345678901234567890123456789012345678901234567890123456789012345678901234567890'
    @ 2, 1, 17, 35 ROWBROWSE oRowBrowse ID 'test2' COLOR 'R/W,G/B,G/R,R/G' TITLE 'test' ALIGN 'L' TITLECOLOR 'G/R' ROW 4 CARGO 'Semper fidelis' HEADERCOLOR {'', 'G/R'} KEYMAP hKeysMap ACTION {| oRowBrowse, nKey | search(oRowBrowse, nKey)} COLORBLOCK {|| color()}

    USE example NEW
    INDEX ON field->first+field->second TO ind
    oRowBrowse:display()

    CLS

RETURN

PROCEDURE alice_and_bob()
    YesNo('Alice and Bob')
RETURN 

FUNCTION search(oRowBrowse, nKey)

    LOCAL cCurrentString := oRowBrowse:search_keys()
    LOCAL nReturn := ROWBROWSE_NOTHING
    LOCAL nOldRecNo := RecNo()

    @ 20, 1 CLEAR TO 20, MaxCol()

    IF AScan({K_DOWN, K_UP, K_END}, nKey) != 0
        oRowBrowse:search_keys('')
        oRowBrowse:draw_border()
        oRowBrowse:print_title()
    ELSEIF nKey == K_BS
        oRowBrowse:search_keys(Left(cCurrentString, Len(cCurrentString) - 1))
        IF !Empty(oRowBrowse:search_keys())
            oRowBrowse:search(oRowBrowse:search_keys())
            @ 20, 1 SAY 'Found: ' + oRowBrowse:search_keys()
        ENDIF
        oRowBrowse:draw_border()
        oRowBrowse:print_title()
    ELSEIF !hb_hHasKey(oRowBrowse:keys_map(), nKey) .AND. !Empty(IndexKey())
        IF oRowBrowse:search(cCurrentString + Chr(nKey))
            nReturn := ROWBROWSE_SEARCH
            oRowBrowse:search_keys(cCurrentString + Chr(nKey))
            oRowBrowse:draw_border()
            oRowBrowse:print_title()
            @ 20, 1 SAY 'Found: ' + cCurrentString + Chr(nKey)
        ELSE
            GO nOldRecNo
            @ 20, 1 SAY 'Found: ' + cCurrentString
        ENDIF
    ENDIF

RETURN nReturn

FUNCTION color()

    IF Val(field->first) > 10
        RETURN {3, 4}
    ENDIF

RETURN {1, 2}
