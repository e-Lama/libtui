#include "inkey.ch"
#include "box.ch"

#include "menu.ch"
#include "align.ch"

PROCEDURE main()

    LOCAL acMenuItems := {'first', 'second', 'third', 'fourth', 'fifth', 'sixth', 'seventh', 'eight', 'ninth', 'tenth', 'eleventh', 'twelfth', 'thirteenth', 'fourteenth', 'fiveteenth', 'sixteenth', 'seventeenth', 'eighteenth', 'nineteenth', 'twentieth'}
    LOCAL alAllowed := Array(Len(acMenuItems))
    LOCAL nResult
    LOCAL oError

    Set(_SET_EVENTMASK, INKEY_ALL)

    AFill(alAllowed, .T.)
    alAllowed[2] := .F.
    alAllowed[3] := .F.
    alAllowed[4] := .F.

    BEGIN SEQUENCE

        ErrorBlock({| oError | standard_error_handler(oError)})

        CLS

        @ 0, 4, 7, 25 MENU TO nResult ITEMS acMenuItems SELECTABLE alAllowed FUNCTION 'menu_search_allow_exit_move' STARTFROM 3 COLOR 'N/W*,W+/B,,,W/N' BORDER B_SINGLE TITLE ' Title ' TITLECOLOR 'W/B' ALIGN ALIGN_CENTER MOUSABLE SCROLLABLE

        ? nResult

    RECOVER USING oError
        standard_error_handler(oError)
    END SEQUENCE

RETURN
