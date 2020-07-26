#define MENU_SELECT 1
#define MENU_ABORT 2
#define MENU_UP 3
#define MENU_LEFT 4
#define MENU_DOWN 5
#define MENU_RIGHT 6
#define MENU_CENTER 7

#define SCROLLBAR_UP 1
#define SCROLLBAR_LEFT 2
#define SCROLLBAR_DOWN 3
#define SCROLLBAR_RIGHT 4

#command @ <top>, <left>, <bottom>, <right> MENU TO <ret> ITEMS <items> SELECTABLE <selectable> ;
           [FUNCTION <function>] [STARTFROM <initialitem>] [COLOR <color>] [BORDER <border>] ;
           [ ;
               TITLE <title> [TITLECOLOR <titlecolor>] [ALIGN <align>] ; 
           ] ;
           [ ;
               [<scrollable: SCROLLABLE>] [SCROLLFROM <scrollfrom>] [SCROLLTO <scrollto>]  ;
               [ORIENTATION <orientation>] [CARGO <cargo>] [STYLE <style>] [BLOCK <block>] ;
           ] => ;
           <ret> := display_menu(<top>, <left>, <bottom>, <right>, <items>, <selectable>, <function> ;
                                , <initialitem>, <color>, <border>, <title>, <titlecolor>, <align> ;
                                , <.scrollable.>, <scrollfrom>, <scrollto>, <orientation>, <cargo> ;
                                , <style>, <block> ;
                                )
