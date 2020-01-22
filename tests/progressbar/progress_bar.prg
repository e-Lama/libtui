#include "progressbar.ch"

PROCEDURE main()

    LOCAL aoBars := Array(0)
    LOCAL nKey
    LOCAL i

    SET CURSOR OFF

    CLS

    FOR i := 10 TO 110 STEP 10
        @ 0, i SAY LTrim(Str(Int(Int((i - 10) / 10) % 10)))
    NEXT
    FOR i := 10 TO 110
        @ 1, i SAY LTrim(Str(Int(i % 10)))
    NEXT

    AAdd(aoBars, Progress_bar():new(2, 10, 110, , , N_POSITION_RIGHT))
    AAdd(aoBars, Progress_bar():new(3, 10, 110, , , N_POSITION_LEFT))
    AAdd(aoBars, Progress_bar():new(5, 10, 110, , , N_POSITION_TOP))
    AAdd(aoBars, Progress_bar():new(6, 10, 110, , , N_POSITION_BOTTOM))
    AAdd(aoBars, Progress_bar():new(8, 10, 110, , , N_POSITION_CENTER))
    AAdd(aoBars, Progress_bar():new(9, 10, 110, , , N_POSITION_NONE))
    AAdd(aoBars, Progress_bar():new(10, 10, 115, , , N_POSITION_CHASE))

    AAdd(aoBars, Progress_bar():new(13, 10, 115, , , N_POSITION_RIGHT, , 1))
    AAdd(aoBars, Progress_bar():new(14, 10, 110, , , N_POSITION_LEFT, , 1))
    AAdd(aoBars, Progress_bar():new(16, 10, 110, , , N_POSITION_TOP, , 1))
    AAdd(aoBars, Progress_bar():new(17, 10, 110, , , N_POSITION_BOTTOM, , 1))
    AAdd(aoBars, Progress_bar():new(19, 10, 110, , , N_POSITION_CENTER, , 1))
    AAdd(aoBars, Progress_bar():new(20, 10, 110, , , N_POSITION_NONE, , 1))
    AAdd(aoBars, Progress_bar():new(21, 10, 115, , , N_POSITION_CHASE, , 1))

    AAdd(aoBars, Progress_bar():new(25, 10, 115, , , N_POSITION_RIGHT, , 2))
    AAdd(aoBars, Progress_bar():new(26, 10, 110, , , N_POSITION_LEFT, , 2))
    AAdd(aoBars, Progress_bar():new(28, 10, 110, , , N_POSITION_TOP, , 2))
    AAdd(aoBars, Progress_bar():new(29, 10, 110, , , N_POSITION_BOTTOM, , 2))
    AAdd(aoBars, Progress_bar():new(31, 10, 110, , , N_POSITION_CENTER, , 2))
    AAdd(aoBars, Progress_bar():new(31, 10, 110, , , N_POSITION_NONE, , 2))
    AAdd(aoBars, Progress_bar():new(33, 10, 115, , , N_POSITION_CHASE, , 2))

    AAdd(aoBars, Progress_bar():new(36, 30, 50, 3, 701, N_POSITION_CHASE, N_FORMAT_SLASH, , .T.))

    DO WHILE .T.
        nKey := Inkey(0)        
        IF Chr(nKey) $ 'Ll'
            AEval(aoBars, {| oElement | oElement:increment()})
        ELSEIF Chr(nKey) $ 'Hh'
            AEval(aoBars, {| oElement | oElement:decrement()})
        ELSEIF Chr(nKey) $ 'Qq'
            EXIT
        ENDIF
        AEval(aoBars, {| oElement | oElement:display()})
    ENDDO

    ? 'Continue? (y/other character)'

    nKey = Inkey(0)

    IF !(Chr(nKey) $ 'Yy')
        RETURN
    ENDIF

    aoBars := {}
    CLS

    AAdd(aoBars, Progress_bar():new(2, 10, 54, , , N_POSITION_RIGHT))
    AAdd(aoBars, Progress_bar():new(3, 10, 54, , , N_POSITION_LEFT))
    AAdd(aoBars, Progress_bar():new(5, 10, 54, , , N_POSITION_TOP))
    AAdd(aoBars, Progress_bar():new(6, 10, 54, , , N_POSITION_BOTTOM))
    AAdd(aoBars, Progress_bar():new(8, 10, 54, , , N_POSITION_CENTER))
    AAdd(aoBars, Progress_bar():new(9, 10, 54, , , N_POSITION_NONE))
    AAdd(aoBars, Progress_bar():new(10, 10, 54, , , N_POSITION_CHASE))

    AAdd(aoBars, Progress_bar():new(13, 10, 54, , , N_POSITION_RIGHT, , 1))
    AAdd(aoBars, Progress_bar():new(14, 10, 54, , , N_POSITION_LEFT, , 1))
    AAdd(aoBars, Progress_bar():new(16, 10, 54, , , N_POSITION_TOP, , 1))
    AAdd(aoBars, Progress_bar():new(17, 10, 54, , , N_POSITION_BOTTOM, , 1))
    AAdd(aoBars, Progress_bar():new(19, 10, 54, , , N_POSITION_CENTER, , 1))
    AAdd(aoBars, Progress_bar():new(20, 10, 54, , , N_POSITION_NONE, , 1))
    AAdd(aoBars, Progress_bar():new(21, 10, 54, , , N_POSITION_CHASE, , 1))

    AAdd(aoBars, Progress_bar():new(25, 10, 54, , , N_POSITION_RIGHT, , 2))
    AAdd(aoBars, Progress_bar():new(26, 10, 54, , , N_POSITION_LEFT, , 2))
    AAdd(aoBars, Progress_bar():new(28, 10, 54, , , N_POSITION_TOP, , 2))
    AAdd(aoBars, Progress_bar():new(29, 10, 54, , , N_POSITION_BOTTOM, , 2))
    AAdd(aoBars, Progress_bar():new(31, 10, 54, , , N_POSITION_CENTER, , 2))
    AAdd(aoBars, Progress_bar():new(31, 10, 54, , , N_POSITION_NONE, , 2))
    AAdd(aoBars, Progress_bar():new(33, 10, 54, , , N_POSITION_CHASE, , 2))

    AAdd(aoBars, Progress_bar():new(36, 10, 110, 3, 701, N_POSITION_CHASE, N_FORMAT_SLASH, , .T.))

    DO WHILE .T.
        nKey := Inkey(0)        
        IF Chr(nKey) $ 'Ll'
            AEval(aoBars, {| oElement | oElement:increment()})
        ELSEIF Chr(nKey) $ 'Hh'
            AEval(aoBars, {| oElement | oElement:decrement()})
        ELSEIF Chr(nKey) $ 'Qq'
            EXIT
        ENDIF
        AEval(aoBars, {| oElement | oElement:display()})
    ENDDO

    ? 'Continue? (y/other character)'

    nKey = Inkey(0)

    IF !(Chr(nKey) $ 'Yy')
        RETURN
    ENDIF

    CLS

    @ 10, 20, 100 PROGRESSBAR aoBars BEGIN 10 END 200 POSITION N_POSITION_CHASE FORMAT N_FORMAT_PERCENT ACCURACY 1 DESCRIPTION 'R/G,W/N' REVERSE COLORS {'N/W,W/N', 'G/R,R/G', 'W/N,N/W', 'B/G,G/B', 'R/B,B/R'} PATTERNS {'S', '.', '_', 'E', '*'}

    aoBars := {aoBars}

    DO WHILE .T.
        nKey := Inkey(0)        
        IF Chr(nKey) $ 'Ll'
            AEval(aoBars, {| oElement | oElement:increment()})
        ELSEIF Chr(nKey) $ 'Hh'
            AEval(aoBars, {| oElement | oElement:decrement()})
        ELSEIF Chr(nKey) $ 'Qq'
            EXIT
        ENDIF
        AEval(aoBars, {| oElement | oElement:display()})
    ENDDO

    ? 'Press any key to quit the program'

    Inkey(0)

RETURN
