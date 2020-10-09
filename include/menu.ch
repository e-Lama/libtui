#define MENU_SELECT 1
#define MENU_ABORT 2
#define MENU_UP 3
#define MENU_LEFT 4
#define MENU_DOWN 5
#define MENU_RIGHT 6
#define MENU_CENTER 7
#define MENU_LBUTTONUP 8
#define MENU_LBUTTONDOWN 9

#define SCROLLBAR_UP 1
#define SCROLLBAR_LEFT 2
#define SCROLLBAR_DOWN 3
#define SCROLLBAR_RIGHT 4

#define MENU_BASIC_KEYS {K_ENTER, K_ESC, K_ALT_UP, K_ALT_LEFT, K_ALT_DOWN, K_ALT_RIGHT, K_ALT_ENTER, K_LBUTTONUP, K_LBUTTONDOWN}

#command @ <top>, <left>, <bottom>, <right> MENU TO <ret> ITEMS <items> SELECTABLE <selectable> ;
           [FUNCTION <function>] [STARTFROM <initialitem>] [COLOR <color>] [BORDER <border>] ;
           [ ;
               TITLE <title> [TITLECOLOR <titlecolor>] [ALIGN <align>] ; 
           ] ;
           [ ;
               [<scrollable: SCROLLABLE>] [SCROLLFROM <scrollfrom>] [SCROLLTO <scrollto>]  ;
               [SCROLLCOLOR <scrollcolor>] [ORIENTATION <orientation>] [CARGO <cargo>] ;
               [STYLE <style>] [BLOCK <block>] ;
           ] ;
           [KEYS <keys>] [<mousable: MOUSABLE>] [ITEMALIGN <itemalign>] ;
           => ;
           <ret> := display_menu(<top>, <left>, <bottom>, <right>, <items>, <selectable>, <function> ;
                                , <initialitem>, <color>, <border>, <title>, <titlecolor>, <align> ;
                                , <keys>, <.mousable.>, <.scrollable.>, <scrollfrom>, <scrollto> ;
                                , <scrollcolor>, <orientation>, <cargo>, <style>, <block>, <itemalign> ;
                                )

#command @ <top>, <left> MENU AUTOSIZE TO <ret> ITEMS <items> SELECTABLE <selectable> ;
           [FUNCTION <function>] [STARTFROM <initialitem>] [COLOR <color>] [BORDER <border>] ;
           [ ;
               TITLE <title> [TITLECOLOR <titlecolor>] [ALIGN <align>] ; 
           ] ;
           [ ;
               [<scrollable: SCROLLABLE>] [SCROLLFROM <scrollfrom>] [SCROLLTO <scrollto>]  ;
               [SCROLLCOLOR <scrollcolor>] [ORIENTATION <orientation>] [CARGO <cargo>] ;
               [STYLE <style>] [BLOCK <block>] ;
           ] ;
           [KEYS <keys>] [<mousable: MOUSABLE>] ;
           => ;
           <ret> := display_menu_autosize(<top>, <left>, <items>, <selectable>, <function> ;
                                , <initialitem>, <color>, <border>, <title>, <titlecolor>, <align> ;
                                , <keys>, <.mousable.>, <.scrollable.>, <scrollfrom>, <scrollto> ;
                                , <scrollcolor>, <orientation>, <cargo>, <style>, <block> ;
                                )

#command @ <centerrow>, <centercol>, <height>, <width> MENU CENTER TO <ret> ITEMS <items> SELECTABLE <selectable> ;
           [FUNCTION <function>] [STARTFROM <initialitem>] [COLOR <color>] [BORDER <border>] ;
           [ ;
               TITLE <title> [TITLECOLOR <titlecolor>] [ALIGN <align>] ; 
           ] ;
           [ ;
               [<scrollable: SCROLLABLE>] [SCROLLFROM <scrollfrom>] [SCROLLTO <scrollto>]  ;
               [SCROLLCOLOR <scrollcolor>] [ORIENTATION <orientation>] [CARGO <cargo>] ;
               [STYLE <style>] [BLOCK <block>] ;
           ] ;
           [KEYS <keys>] [<mousable: MOUSABLE>] ;
           => ;
           <ret> := display_menu_center(<centerrow>, <centercol>, <height>, <width>, <items> ;
                                , <selectable>, <function>, <initialitem>, <color>, <border> ;
                                , <title>, <titlecolor>, <align>, <keys>, <.mousable.> ;
                                , <.scrollable.>, <scrollfrom>, <scrollto>, <scrollcolor> ;
                                , <orientation>, <cargo>, <style>, <block> ;
                                )

#command @ <centerrow>, <centercol> MENU CENTER AUTOSIZE TO <ret> ITEMS <items> SELECTABLE <selectable> ;
           [FUNCTION <function>] [STARTFROM <initialitem>] [COLOR <color>] [BORDER <border>] ;
           [ ;
               TITLE <title> [TITLECOLOR <titlecolor>] [ALIGN <align>] ; 
           ] ;
           [ ;
               [<scrollable: SCROLLABLE>] [SCROLLFROM <scrollfrom>] [SCROLLTO <scrollto>]  ;
               [SCROLLCOLOR <scrollcolor>] [ORIENTATION <orientation>] [CARGO <cargo>] ;
               [STYLE <style>] [BLOCK <block>] ;
           ] ;
           [KEYS <keys>] [<mousable: MOUSABLE>] ;
           => ;
           <ret> := display_menu_center_autosize(<centerrow>, <centercol>, <items>, <selectable>, <function> ;
                                , <initialitem>, <color>, <border>, <title>, <titlecolor>, <align>, <keys>, <.mousable.> ;
                                , <.scrollable.>, <scrollfrom>, <scrollto>, <scrollcolor>, <orientation> ;
                                , <cargo>, <style>, <block> ;
                                )
