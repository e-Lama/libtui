#define N_POSITION_NONE 0
#define N_POSITION_RIGHT 1
#define N_POSITION_CENTER 2
#define N_POSITION_LEFT 3
#define N_POSITION_BOTTOM 4
#define N_POSITION_TOP 5
#define N_POSITION_CHASE 6

#define N_FORMAT_SLASH 0
#define N_FORMAT_PERCENT 1

#define N_START 1
#define N_FILLED 2
#define N_EMPTY 3
#define N_END 4
#define N_FINISHED 5

#command @ <row>, <left>, <right> PROGRESSBAR <obj> [BEGIN <actual>] [END <target>] [POSITION <display>] ;
           [FORMAT <format>] [ACCURACY <accuracy>] [<rev:REVERSE>] [DESCRIPTION <color>] [SLASH <slash>] ;
           [PATTERNS <patterns>] [COLORS <colors>] => ;
           <obj> := Progress_bar():new(<row>, <left>, <right>, <actual>, <target>, <display>, <format> ;
                                      , <accuracy>, <.rev.>, <color>, <slash>, <patterns>, <colors> ;
                                      )

