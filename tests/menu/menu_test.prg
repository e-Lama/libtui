#include "inkey.ch"

#include "menu.ch"

PROCEDURE main()

    LOCAL acMenuItems := {'first', 'second', 'third', 'fourth'}

    Menu():keys({K_ENTER, K_ESC, K_ALT_UP, K_ALT_LEFT, K_ALT_DOWN, K_RIGHT, K_ALT_ENTER})
    ? Menu():key(1)
    ? Menu():key(3)
    Menu():key(3, K_F5)
    ? Menu():key(3)
    Inkey(0)
    display_menu(2, 2, 5, 20, acMenuItems, {.T., .F., .T., .T.}, 'menu_search_allow_exit_move', 3, 'R/G,N/W,W/N', '12345678', 'Title', 'G/B', 'R')

RETURN
