#define SAFE_YES 'Yes'
#define SAFE_NO 'No'
#define SAFE_OK 'OK'
#define YESNO_SAFE_COLOR 'W+/R,W/B'
#define DIALOG_SAFE_COLOR 'W+/R,W/B'
#define INFORM_SAFE_COLOR 'W+/R,W/B'

#define YES Config():get_config('DefaultYes')
#define NO Config():get_config('DefaultNo')
#define OK Config():get_config('DefaultPrintMessageOption')

#define YESNO_COLOR Config():get_config('DefaultYesNoColor')
#define YESNO_MOVE Config():get_config('DefaultYesNoAllowMove')
#define YESNO_CYCLIC Config():get_config('DefaultYesNoCyclic')
#define YESNO_ACCEPT_FIRST Config():get_config('DefaultYesNoAcceptFirst')

#define DIALOG_COLOR Config():get_config('DefaultDialogColor')
#define DIALOG_MOVE Config():get_config('DefaultDialogAllowMove')
#define DIALOG_CYCLIC Config():get_config('DefaultDialogCyclic')
#define DIALOG_ACCEPT_FIRST Config():get_config('DefaultDialogAcceptFirst')

#define INFORM_COLOR Config():get_config('DefaultInformColor')
#define INFORM_MOVE Config():get_config('DefaultInformAllowMove')
#define INFORM_CYCLIC Config():get_config('DefaultInformCyclic')
#define INFORM_ACCEPT_FIRST Config():get_config('DefaultInformAcceptFirst')

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

#command @ YESNO <message> TO <ret> [ANWSERS <anwsers>] [COLOR <color>] [DEFAULT <currentoption>] ;
           [<cyclic: CYCLIC>] [<safe: SAFE>] [<allowmove: MOVABLE>] [<acceptfirstfounded: FAST ACCEPT>] ;
           [BORDER <border>] [ALIGN <align>] => ;
           <ret> := YesNo(<message>, <anwsers>, <color>, <.safe.>, <.allowmove.>, <currentoption>, <.cyclic.> ;
                         , <.acceptfirstfounded.>, <border>, <align> ;
                         )

#command @ NOYES <message> TO <ret> [ANWSERS <anwsers>] [COLOR <color>] [DEFAULT <currentoption>] ;
           [<cyclic: CYCLIC>] [<safe: SAFE>] [<allowmove: MOVABLE>] [<acceptfirstfounded: FAST ACCEPT>] ;
           [BORDER <border>] [ALIGN <align>] => ;
           <ret> := NoYes(<message>, <anwsers>, <color>, <.safe.>, <.allowmove.>, <currentoption>, <.cyclic.> ;
                         , <.acceptfirstfounded.>, <border>, <align> ;
                         )

#command @ DIALOG <message> TO <ret> [OPTIONS <options>] [COLOR <color>] [DEFAULT <currentoption>] ;
           [<cyclic: CYCLIC>] [<safe: SAFE>] [<allowmove: MOVABLE>] [<acceptfirstfounded: FAST ACCEPT>] ;
           [BORDER <border>] [ALIGN <align>] [<allowescape: ESCAPABLE>] [DELAY <delay>] => ;
           <ret> := Dialog(<message>, <options>, <color>, <.allowescape.>, <delay>, <.safe.>, <.allowmove.> ;
                         , <currentoption>, <.cyclic.>, <.acceptfirstfounded.>, <border>, <align> ;
                         )

#command @ INFORM <message> TO <ret> [COLOR <color>] [DEFAULT <currentoption>] ;
           [<cyclic: CYCLIC>] [<safe: SAFE>] [<allowmove: MOVABLE>] [<acceptfirstfounded: FAST ACCEPT>] ;
           [BORDER <border>] [ALIGN <align>] [<allowescape: ESCAPABLE>] [DELAY <delay>] => ;
           <ret> := Inform(<message>, <color>, <.allowescape.>, <delay>, <.safe.>, <.allowmove.> ;
                          , <.cyclic.>, <.acceptfirstfounded.>, <border>, <align> ;
                          )
