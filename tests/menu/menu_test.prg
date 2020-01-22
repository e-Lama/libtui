PROCEDURE main()

    LOCAL acMenuItems := {'first', 'second', 'third', 'fourth'}
    display_menu(2, 2, 5, 20, acMenuItems, {.T., .F., .T., .T.}, 'menu_search_allow_exit_move', 3, 'R/G,N/W,W/N', '12345678', 'Title', 'G/B', 'R')

RETURN
