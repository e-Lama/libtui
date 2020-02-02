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
