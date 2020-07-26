#define SELECT 1
#define ABORT 2
#define UP 3
#define LEFT 4
#define DOWN 5
#define RIGHT 6
#define CENTER 7

#define SCROLLBAR_UP 1
#define SCROLLBAR_LEFT 2
#define SCROLLBAR_DOWN 3
#define SCROLLBAR_RIGHT 4

#command @ <top>, <left>, <bottom>, <right> MENUMTO <ret> ITEMS <items> [SELCTABLE <selectable>] ;
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
