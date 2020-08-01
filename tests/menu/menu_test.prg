#include "inkey.ch"
#include "box.ch"

#include "menu.ch"
#include "align.ch"

PROCEDURE main()

    LOCAL acMenuItems := {'first', 'second', 'third', 'fourth', 'fifth', 'sixth', 'seventh', 'eight', 'ninth', 'tenth', 'eleventh', 'twelfth', 'thirteenth', 'fourteenth', 'fiveteenth', 'sixteenth', 'seventeenth', 'eighteenth', 'nineteenth', 'twentieth'}
    LOCAL alAllowed := Array(Len(acMenuItems))
    LOCAL nResult
    LOCAL oError

    AFill(alAllowed, .T.)
    alAllowed[2] := .F.

    BEGIN SEQUENCE

        ErrorBlock({| oError | standard_error_handler(oError)})
        Menu():keys({K_ENTER, K_ESC, K_ALT_UP, K_ALT_LEFT, K_ALT_DOWN, K_RIGHT, K_ALT_ENTER})

        CLS

        @ 3, 4, 15, 25 MENU TO nResult ITEMS acMenuItems SELECTABLE alAllowed FUNCTION 'menu_search_allow_exit_move' STARTFROM 4 COLOR 'N/W*,W+/B,,,W/N' BORDER B_SINGLE TITLE ' Title ' TITLECOLOR 'W/B' ALIGN ALIGN_CENTER SCROLLABLE

        ? nResult

    RECOVER USING oError
        standard_error_handler(oError)
    END SEQUENCE

RETURN
