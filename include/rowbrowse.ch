#define ROWBROWSE_NOTHING 0
#define ROWBROWSE_REBORDER 1
#define ROWBROWSE_CLEAR_SEARCH_STRING 2
#define ROWBROWSE_SEARCH 3
#define ROWBROWSE_END 4
#define ROWBROWSE_RESTART 5
#define ROWBROWSE_NO_ACTION 6
#define ROWBROWSE_RESTART_PRESERVE 7

#define DO_NOT_REFRESH 0
#define REFRESH_CURRENT_ONCE 1
#define REFRESH_ALL_ONCE 2
#define ALWAYS_REFRESH_ALL 3
#define ALWAYS_REFRESH_CURRENT 4

#define N_COLUMN_NUMBER 1
#define N_FIELD_WIDTH 2
#define L_RELATIVE 3
#define C_FIELD_SEPARATOR 4
#define C_FIELD_ALIGN 5
#define C_HEADER 6
#define C_HEADER_SEPARATOR 7
#define C_HEADER_ALIGN 8
#define C_PRINT_FNC 9


#define DO_NOT_REPRINT 0
#define REPRINT_ONCE 1
#define ALWAYS_REPRINT 2

#command @ <top>, <left>, <bottom>, <right> ROWBROWSE <obj> ID <id> [COLOR <color>] ;
           [BORDER <border>] [TITLE <title>] [ALIGN <align>] [TITLECOLOR <titlecolor>] ;
           [ROW <row>] [ACTION <action>] [COLORBLOCK <colorblock>] ;
           [HEADERCOLOR <headercolor>] [KEYMAP <keymap>] [CARGO <cargo>] ;
           [AUTOHIGHLIGHT <autohighlight>] [SKIP <skip>] [GOBOTTOM <gobottom>] ;
           [GOTOP <gotop>] [<mousable: MOUSABLE>] => ;
           <obj> := Row_browse():new(<id>, <top>, <left>, <bottom>, <right>, <color> ;
                                    , <border>, <title>, <align>, <titlecolor>, <row>, <action> ;
                                    , <colorblock>, <headercolor>, <keymap>, <cargo> ;
                                    , <autohighlight>, <skip>, <gobottom>, <gotop>, <.mousable.> ;
                                    )
