//#define VALIDATE_DIMENSIONS

#include "hbclass.ch"
#include "color.ch"

#include "parser.ch"

CREATE CLASS Parser

EXPORTED:

    METHOD prepare_form_from_database(cLanguage, cId, hVariables, cDatabase)
    METHOD prepare_form_from_record(acRows, hVariables)
    METHOD check_correctness(acRows, hVariables)
    METHOD get_answers()

    METHOD log(cLog) SETGET

    METHOD get_window_handler() INLINE ::nWindow

    METHOD getlist(aoGetList) SETGET
    METHOD use_memvar(lUseMemvar) SETGET

HIDDEN:

    CLASSVAR cLog AS CHARACTER INIT ''
    CLASSVAR axKeys AS ARRAY INIT Array(0)
    CLASSVAR axValues AS ARRAY INIT Array(0)
    CLASSVAR axUsedKeys AS ARRAY INIT Array(0)
    CLASSVAR nWindow AS NUMERIC INIT -1
    CLASSVAR aoGetList AS ARRAY INIT Array(0)
    CLASSVAR lUseMemvar AS LOGICAL INIT .T.

    METHOD validate_window(axRow, hVariables)
    METHOD validate_box(axRow, hVariables)
    METHOD validate_say(axRow, hVariables)
    METHOD validate_get(axRow, hVariables)
    METHOD validate_checkbox(axRow, hVariables)
    METHOD validate_listbox(axRow, hVariables)
    METHOD validate_radiogroup(axRow, hVariables)
    METHOD validate_pushbutton(axRow, hVariables)
    METHOD add_to_debug(cTxt) INLINE ::cLog += cTxt
    METHOD validate(axRow, hVariables)

    METHOD make_window(nTop, nLeft, nBottom, nRight, cBox, cColor, xShadow)
    METHOD make_say(nRow, nCol, cExp, cSayPicture, cColorString)
    METHOD make_radiogroup(nTop, nLeft, nBottom, nRight, xIdVar, aGroup, cCaption, cMessage, cColor, cFocus, cWhen, cValid)
    METHOD make_box(nTop, nLeft, nBottom, nRight, cBoxString, cColorString)
    METHOD make_get(nRow, nCol, cExp, cSayPicture, cColorStringSay, xIdVar, cGetPicture, cColorStringGet, cCaption, cMessage, cWhen, cValid)
    METHOD make_listbox(nTop, nLeft, nBottom, nRight, xIdVar, axList, cCaption, cMessage, cWhen, cValid, cColor, cFocus, cState, lDropDown, lScrollBar)
    METHOD make_checkbox(nRow, nCol, xIdVar, cCaption, cMessage, cWhen, cValid, cColor, cFocus, cState, cStyle)
    METHOD make_pushbutton(nRow, nCol, xIdVar, cCaption, cMessage, cWhen, cValid, cColor, cFocus, cState, cStyle)

    METHOD make_buttons(acPar)

    METHOD corrupted_row(cRow)
    METHOD basic_parse(xRow, cType, hVariables)
    METHOD handle_hash(hVariables)
    METHOD handle_object(axRow)

ENDCLASS LOCK 

METHOD getlist(aoGetList) CLASS Parser

    LOCAL aoWasGetList := clone_objects_array(::aoGetList)

    IF aoGetList != NIL
        assert_type(aoGetList, 'A')
        AEval(aoGetList, {| oElement | assert_type(oElement, 'O')})
        ::aoGetList := aoGetList
    ENDIF

RETURN aoWasGetList

METHOD use_memvar(lUseMemvar) CLASS Parser

    LOCAL lWasUseMemvar := ::lUseMemvar

    IF lUseMemvar != NIL
        assert_type(lUseMemvar, 'L')
        ::lUseMemvar := lUseMemvar
    ENDIF

RETURN lWasUseMemvar

METHOD log(cLog) CLASS Parser

    LOCAL cWasLog := ::cLog

    IF cLog != NIL
        assert_type(cLog, 'C')
        ::cLog := cLog
    ENDIF

RETURN cWasLog

METHOD get_answers() 

    LOCAL hAnswers := hb_Hash()
    LOCAL i

    FOR i := 1 TO Len(::axKeys)
        hAnswers[::axKeys[i]] := ::axValues[i]
    NEXT

RETURN hAnswers

METHOD make_buttons(acPar) CLASS Parser

    LOCAL aoButtons := Array(0)
    LOCAL cRow
    LOCAL acParams
    
    FOR EACH cRow IN acPar
        acParams := hb_ATokens(cRow, ',')
        AAdd(aoButtons, RadioButto(Val(acParams[N_ROW_RBT]), Val(acParams[N_COL_RBT]), acParams[C_CAPTION_RBT], acParams[C_VALUE_RBT]))
    NEXT

RETURN aoButtons
    
METHOD handle_hash(hVariables) CLASS Parser
    
    ::axKeys := hb_HKeys(hVariables)
    ::axValues := hb_HValues(hVariables)
    ::axUsedKeys := Array(0)

RETURN NIL

METHOD handle_object(axRow) CLASS Parser

    DO CASE 
        CASE axRow[OBJECT] == OBJECT_WINDOW
            RETURN ::make_window(axRow[N_TOP_WN];
                                     , axRow[N_LEFT_WN];
                                     , axRow[N_BOTTOM_WN];
                                     , axRow[N_RIGHT_WN];
                                     , axRow[C_BOX_WN];
                                     , axRow[C_COLOR_WN];
                                     , axRow[NC_SHADOW_WN];
                                     )
        CASE axRow[OBJECT] == OBJECT_BOX
            RETURN ::make_box(axRow[N_TOP_BOX];
                                     , axRow[N_LEFT_BOX];
                                     , axRow[N_BOTTOM_BOX];
                                     , axRow[N_RIGHT_BOX];
                                     , axRow[C_BOX_BOX];
                                     , axRow[C_COLOR_BOX];
                                     )
        CASE axRow[OBJECT] == OBJECT_SAY 
            RETURN ::make_say(axRow[N_ROW_SAY];
                                     , axRow[N_COL_SAY];
                                     , axRow[C_EXPRESSION_SAY];
                                     , axRow[C_PICTURE_SAY];
                                     , axRow[C_COLOR_SAY];
                                     )
        CASE axRow[OBJECT] == OBJECT_GET
            RETURN ::make_get(axRow[N_ROW_GET];
                                     , axRow[N_COL_GET];
                                     , axRow[C_EXPRESSION_GET];
                                     , axRow[C_SAY_PICTURE_GET];
                                     , axRow[C_SAY_COLOR_GET];
                                     , axRow[X_ID_VAR_GET];
                                     , axRow[C_GET_PICTURE_GET];
                                     , axRow[C_GET_COLOR_GET];
                                     , axRow[C_CAPTION_GET];
                                     , axRow[C_MESSAGE_GET];
                                     , axRow[C_WHEN_FNC_GET];
                                     , axRow[C_VALID_FNC_GET];
                                     )
        CASE axRow[OBJECT] == OBJECT_CHECKBOX
            RETURN ::make_checkbox(axRow[N_ROW_CHB];
                                     , axRow[N_COL_CHB];
                                     , axRow[L_ID_VAR_CHB];
                                     , axRow[C_CAPTION_CHB];
                                     , axRow[C_MESSAGE_CHB];
                                     , axRow[C_WHEN_FNC_CHB];
                                     , axRow[C_VALID_FNC_CHB];
                                     , axRow[C_COLOR_CHB];
                                     , axRow[C_FOCUS_FNC_CHB];
                                     , axRow[C_STATE_CHB];
                                     , axRow[C_STYLE_CHB];
                                     )
        CASE axRow[OBJECT] == OBJECT_LISTBOX
            RETURN ::make_listbox(axRow[N_TOP_LSB];
                                      , axRow[N_LEFT_LSB];
                                      , axRow[N_BOTTOM_LSB];
                                      , axRow[N_RIGHT_LSB];
                                      , axRow[NC_ID_VAR_LSB];
                                      , axRow[A_LIST_LSB];
                                      , axRow[C_CAPTION_LSB];
                                      , axRow[C_MESSAGE_LSB];
                                      , axRow[C_WHEN_FNC_LSB];
                                      , axRow[C_VALID_FNC_LSB];
                                      , axRow[C_COLOR_LSB];
                                      , axRow[C_FOCUS_FNC_LSB];
                                      , axRow[C_STATE_FNC_LSB];
                                      , axRow[L_DROPDOWN_LSB];
                                      , axRow[L_SCROLLBAR_LSB];
                                      )
        CASE axRow[OBJECT] == OBJECT_RADIOGROUP    
            RETURN ::make_radiogroup(axRow[N_TOP_RGB];
                                      , axRow[N_LEFT_RGB];
                                      , axRow[N_BOTTOM_RGB];
                                      , axRow[N_RIGHT_RGB];
                                      , axRow[NC_ID_VAR_RGB];
                                      , axRow[A_GROUP_RGB];
                                      , axRow[C_CAPTION_RGB];
                                      , axRow[C_MESSAGE_RGB];
                                      , axRow[C_COLOR_RGB];
                                      , axRow[C_FOCUS_FNC_RGB];
                                      , axRow[C_WHEN_FNC_RGB];
                                      , axRow[C_VALID_FNC_RGB];
                                      )
        CASE axRow[OBJECT] == OBJECT_PUSHBUTTON
            RETURN ::make_pushbutton(axRow[N_ROW_PSB];
                                     , axRow[N_COL_PSB];
                                     , axRow[C_CAPTION_PSB];
                                     , axRow[C_MESSAGE_PSB];
                                     , axRow[C_WHEN_FNC_PSB];
                                     , axRow[C_VALID_FNC_PSB];
                                     , axRow[C_COLOR_PSB];
                                     , axRow[C_FOCUS_FNC_PSB];
                                     , axRow[C_STATE_PSB];
                                     , axRow[C_STYLE_PSB];
                                     )
    ENDCASE

RETURN .F.

METHOD check_correctness(acRows, hVariables) CLASS Parser

    LOCAL lWasWindow := .F.
    LOCAL acRowsCopy
    LOCAL acRow

    assert_type(acRows, 'A')
    assert_type(hVariables, 'H')

    AEval(acRows, {| cElement | assert_type(cElement, 'C')})

    ::handle_hash(hVariables)
    ::nWindow := -1

    acRowsCopy := AClone(acRows)

    FOR EACH acRow IN acRowsCopy
        acRow := hb_ATokens(acRow, LINE_SEPARATOR)

        IF acRow[OBJECT] == OBJECT_WINDOW
            IF lWasWindow
                RETURN .F.
            ENDIF
            lWasWindow := .T.
            IF acRow:__enumIndex() != 1
                ::add_to_debug(Config():get_config('WindowMustBeFirst'))
                RETURN .F.
            ENDIF
        ENDIF

        IF !::validate(acRow, hVariables)
            RETURN .F.
        ENDIF
    NEXT

RETURN .T.

METHOD prepare_form_from_record(acRows, hVariables) CLASS Parser

    LOCAL lWasWindow := .F.
    LOCAL acRowsCopy
    LOCAL acRow

    assert_type(acRows, 'A')
    assert_type(hVariables, 'H')

    AEval(acRows, {| cElement | assert_type(cElement, 'C')})

    ::handle_hash(hVariables)
    ::nWindow := -1

    acRowsCopy := AClone(acRows)

    FOR EACH acRow IN acRowsCopy
        acRow := hb_ATokens(acRow, LINE_SEPARATOR)

        IF acRow[OBJECT] == OBJECT_WINDOW
            IF lWasWindow
                RETURN .F.
            ENDIF

            lWasWindow := .T.

            IF acRow:__enumIndex() != 1
                ::add_to_debug(Config():get_config('WindowMustBeFirst'))
                RETURN .F.
            ENDIF
        ENDIF

        IF !::validate(@acRow, hVariables)
            RETURN .F.
        ENDIF

        IF !::handle_object(acRow)
            RETURN .F.
        ENDIF

    NEXT

RETURN .T.
 
METHOD prepare_form_from_database(cLanguage, cId, hVariables, cDatabase) CLASS Parser

    LOCAL nOldSelect := Select()
    LOCAL nOldRecNo
    LOCAL lResult

    assert_type(cLanguage, 'C')
    assert_type(cId, 'C')
    assert_type(hVariables, 'H')

    IF ValType(cDatabase) == 'C'
        SELECT (cDatabase)
        nOldRecNo := RecNo()
    ELSE
        SELECT dbForms
        nOldRecNo := RecNo()
    ENDIF

    SEEK PadR(cLanguage, Len(field->language)) + PadR(cId, Len(field->id))

    IF !Found()
        RETURN .F.
    ENDIF

    assert_type(field->code, 'M')

    lResult := ::prepare_form_from_record(hb_ATokens(field->code, OBJECT_SEPARATOR), hVariables)

    GO nOldRecNo
    SELECT (nOldSelect)

RETURN lResult

METHOD corrupted_row(cRow) CLASS Parser

    IF Len(cRow) < 2
        RETURN .T.
    ELSEIF Left(cRow, 1) != VARIABLE .AND. Left(cRow, 1) != CONSTANT
        RETURN .T.
    ELSEIF !is_data_type(SubStr(cRow, 2, 1))
        RETURN .T.
    ENDIF

RETURN .F.

METHOD basic_parse(xRow, cType, hVariables) CLASS Parser

    LOCAL lInHash := .F.

    IF ::corrupted_row(xRow)
        ::add_to_debug(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF
   
    IF Left(xRow, 1) == VARIABLE
        lInHash := .T.
    ENDIF

    cType := SubStr(xRow, 2, 1)
    xRow := Right(xRow, Len(xRow) - 2)

    IF lInHash
        xRow := AllTrim(xRow)
        IF hb_hHasKey(hVariables, xRow)
            IF AScan(::axUsedKeys, xRow) != 0
                ::add_to_debug(Config():get_config('VariableRepeating'))
                RETURN .F.
            ENDIF
            AAdd(::axUsedKeys, xRow)
            xRow := hVariables[xRow] 
        ELSE
            ::add_to_debug(Config():get_config('CorruptionDetected'))
            RETURN .F.
        ENDIF
    ENDIF

RETURN .T.

METHOD validate(axRow, hVariables) CLASS Parser

    DO CASE
        CASE axRow[OBJECT] == OBJECT_WINDOW
            RETURN ::validate_window(@axRow, hVariables)
        CASE axRow[OBJECT] == OBJECT_BOX
            RETURN ::validate_box(@axRow, hVariables)
        CASE axRow[OBJECT] == OBJECT_SAY
            RETURN ::validate_say(@axRow, hVariables)
        CASE axRow[OBJECT] == OBJECT_GET
            RETURN ::validate_get(@axRow, hVariables)
        CASE axRow[OBJECT] == OBJECT_CHECKBOX
            RETURN ::validate_checkbox(@axRow, hVariables)
        CASE axRow[OBJECT] == OBJECT_LISTBOX
            RETURN ::validate_listbox(@axRow, hVariables)
        CASE axRow[OBJECT] == OBJECT_RADIOGROUP
            RETURN ::validate_radiogroup(@axRow, hVariables)
        CASE axRow[OBJECT] == OBJECT_PUSHBUTTON
            RETURN ::validate_pushbutton(@axRow, hVariables)
        CASE Empty(axRow[OBJECT])
            ::add_to_debug(Config():get_config('EmptyObject'))
        OTHERWISE
            ::add_to_debug(Config():get_config('UnknownObject'))
    ENDCASE

RETURN .F.

METHOD validate_window(axRow, hVariables) CLASS Parser

    LOCAL cType
    LOCAL i

    IF Len(axRow) != NC_SHADOW_WN
        ::add_to_debug(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF

    FOR i := N_TOP_WN TO N_RIGHT_WN

        IF !::basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

        IF cType != 'N'
            ::add_to_debug(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF

        axRow[i] := cast(axRow[i], cType)

        IF ValType(axRow[i]) != cType
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
#ifdef VALIDATE_DIMENSIONS
        ELSEIF axRow[i] < 0
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
#endif
        ENDIF
    NEXT

    IF axRow[N_BOTTOM_WN] < axRow[N_TOP_WN] .OR. axRow[N_RIGHT_WN] < axRow[N_LEFT_WN]
        ::add_to_debug(Config():get_config('IncorrectDimensions'))
        RETURN .F.
#ifdef VALIDATE_DIMENSIONS
    ELSEIF axRow[N_BOTTOM_WN] > MaxRow() .OR. axRow[N_TOP_WN] > MaxRow() .OR. axRow[N_RIGHT_WN] > MaxCol() .OR. axRow[N_LEFT_WN] > MaxCol()
        ::add_to_debug(Config():get_config('IncorrectValue'))
        RETURN .F.
#endif
    ENDIF

    IF !::basic_parse(@axRow[C_BOX_WN], @cType, hVariables)
        RETURN .F.
    ENDIF

    IF !is_box(axRow[C_BOX_WN])
        ::add_to_debug(Config():get_config('IncorrectValue'))
        RETURN .F.
    ENDIF

    IF !::basic_parse(@axRow[C_COLOR_WN], @cType, hVariables)
        RETURN .F.
    ENDIF

    IF !is_color(axRow[C_COLOR_WN])
        ::add_to_debug(Config():get_config('IncorrectValue'))
        RETURN .F.
    ENDIF

    IF !::basic_parse(@axRow[NC_SHADOW_WN], @cType, hVariables)
        RETURN .F.
    ENDIF

    axRow[NC_SHADOW_WN] := cast(axRow[NC_SHADOW_WN], cType)

    IF ValType(axRow[NC_SHADOW_WN]) != cType
        ::add_to_debug(Config():get_config('IncorrectValue'))
        RETURN .F.
    ENDIF

    IF cType == 'C'
        IF AScan({'N', 'B', 'G', 'BG', 'R', 'RB', 'GR', 'W', 'N+', 'B+', 'G+', 'BG+', 'R+', 'RB+', 'GR+', 'W+'}, axRow[NC_SHADOW_WN]) == 0
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
    ELSEIF cType == 'N'
        IF axRow[NC_SHADOW_WN] != -1
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
    ELSE
        ::add_to_debug(Config():get_config('IncorrectDataType'))
        RETURN .F.
    ENDIF

RETURN .T.

METHOD validate_box(axRow, hVariables) CLASS Parser

    LOCAL cType
    LOCAL i

    IF Len(axRow) != C_COLOR_BOX
        ::add_to_debug(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF

    FOR i := N_TOP_BOX TO N_RIGHT_BOX

        IF !::basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

        IF cType != 'N'
            ::add_to_debug(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF

        axRow[i] := cast(axRow[i], cType)

        IF ValType(axRow[i]) != cType
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
#ifdef VALIDATE_DIMENSIONS
        ELSEIF axRow[i] < 0
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
#endif
        ENDIF
    NEXT

    IF axRow[N_BOTTOM_BOX] < axRow[N_TOP_BOX] .OR. axRow[N_RIGHT_BOX] < axRow[N_LEFT_BOX]
        ::add_to_debug(Config():get_config('IncorrectDimensions'))
        RETURN .F.
#ifdef VALIDATE_DIMENSIONS
    ELSEIF axRow[N_BOTTOM_BOX] > MaxRow() .OR. axRow[N_TOP_BOX] > MaxRow() .OR. axRow[N_RIGHT_BOX] > MaxCol() .OR. axRow[N_LEFT_BOX] > MaxCol()
        ::add_to_debug(Config():get_config('IncorrectValue'))
        RETURN .F.
#endif
    ENDIF

    IF !::basic_parse(@axRow[C_BOX_BOX], @cType, hVariables)
        RETURN .F.
    ENDIF

    IF !is_box(axRow[C_BOX_BOX])
        ::add_to_debug(Config():get_config('IncorrectValue'))
        RETURN .F.
    ENDIF

    IF !::basic_parse(@axRow[C_COLOR_BOX], @cType, hVariables)
        RETURN .F.
    ENDIF

    IF !is_color(axRow[C_COLOR_BOX])
        ::add_to_debug(Config():get_config('IncorrectValue'))
        RETURN .F.
    ENDIF
 
RETURN .T.

METHOD validate_say(axRow, hVariables) CLASS Parser

    LOCAL cType
    LOCAL i

    IF Len(axRow) != C_COLOR_SAY
        ::add_to_debug(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF

    FOR i := N_ROW_SAY TO N_COL_SAY

        IF !::basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

        IF cType != 'N'
            ::add_to_debug(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF

        axRow[i] := cast(axRow[i], cType)

        IF ValType(axRow[i]) != cType
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
#ifdef VALIDATE_DIMENSIONS
        ELSEIF axRow[i] < 0
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
#endif
        ENDIF
    NEXT

#ifdef VALIDATE_DIMENSIONS
    IF axRow[N_ROW_SAY] > MaxRow() .OR. axRow[N_COL_SAY] > MaxCol()
        ::add_to_debug(Config():get_config('IncorrectValue'))
        RETURN .F.
    ENDIF
#endif

    IF !::basic_parse(@axRow[C_EXPRESSION_SAY], @cType, hVariables)
        RETURN .F.
    ENDIF

    IF !(cType $ 'N;L;C;D')
        ::add_to_debug(Config():get_config('IncorrectDataType'))
        RETURN .F.
    ENDIF

    axRow[C_EXPRESSION_SAY] := cast(axRow[C_EXPRESSION_SAY], cType)

    IF ValType(axRow[C_EXPRESSION_SAY]) != cType
        ::add_to_debug(Config():get_config('IncorrectValue'))
        RETURN .F.
    ENDIF

    FOR i := C_PICTURE_SAY TO C_COLOR_SAY

        IF !::basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

        IF cType != 'C'
            ::add_to_debug(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF

        IF i == C_PICTURE_SAY .AND. !is_picture(axRow[i])
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
        ELSEIF i == C_COLOR_SAY .AND. !is_color(axRow[i])
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
    NEXT

RETURN .T.

METHOD validate_get(axRow, hVariables) CLASS Parser

    LOCAL axSayPart := Array(C_SAY_COLOR_GET)
    LOCAL nIndex
    LOCAL cType
    LOCAL i

    IF Len(axRow) != C_VALID_FNC_GET
        ::add_to_debug(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF
    
    ACopy(axRow, axSayPart, N_ROW_GET, C_SAY_COLOR_GET, N_ROW_SAY)

    IF !::validate_say(@axSayPart, hVariables)
        RETURN .F.
    ENDIF

    FOR i := N_ROW_GET TO C_SAY_COLOR_GET
        axRow[i] := axSayPart[i]
    NEXT

    IF Left(axRow[X_ID_VAR_GET], 1) != VARIABLE
        ::add_to_debug(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF

    cType := SubStr(axRow[X_ID_VAR_GET], 2, 1)

    IF AScan(::axUsedKeys, axRow[X_ID_VAR_GET]) != 0
        ::add_to_debug(Config():get_config('VariableRepeating'))
        RETURN .F.
    ENDIF
    AAdd(::axUsedKeys, axRow[X_ID_VAR_GET])

    axRow[X_ID_VAR_GET] := AllTrim(Right(axRow[X_ID_VAR_GET], Len(axRow[X_ID_VAR_GET]) - 2))

    nIndex := AScan(::axKeys, axRow[X_ID_VAR_GET])

    IF nIndex == 0
        ::add_to_debug(Config():get_config('UnknownVariable'))
        RETURN .F.
    ELSE
        ::axValues[nIndex] := cast(::axValues[nIndex], cType)
    ENDIF

    FOR i := C_GET_PICTURE_GET TO C_VALID_FNC_GET

        IF !::basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

        IF cType != 'C'
            ::add_to_debug(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF

        IF i == C_GET_PICTURE_GET .AND. !is_picture(axRow[i])
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
        ELSEIF i == C_GET_COLOR_GET .AND. !is_color(axRow[i])
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
    NEXT

RETURN .T.

METHOD validate_checkbox(axRow, hVariables) CLASS Parser

    LOCAL cType
    LOCAL nIndex
    LOCAL i

    IF Len(axRow) != C_STYLE_CHB
        ::add_to_debug(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF

    FOR i := N_ROW_CHB TO N_COL_CHB

        IF !::basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

        IF cType != 'N'
            ::add_to_debug(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF

        axRow[i] := cast(axRow[i], cType)

        IF ValType(axRow[i]) != cType
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
#ifdef VALIDATE_DIMENSIONS
        ELSEIF axRow[i] < 0
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
#endif
        ENDIF
    NEXT

#ifdef VALIDATE_DIMENSIONS
    IF axRow[N_ROW_CHB] > MaxRow() .OR. axRow[N_COL_CHB] > MaxCol()
        RETURN .F.
    ENDIF
#endif

    IF Left(axRow[L_ID_VAR_CHB], 1) != VARIABLE
        ::add_to_debug(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF

    cType := SubStr(axRow[L_ID_VAR_CHB], 2, 1)

    IF AScan(::axUsedKeys, axRow[L_ID_VAR_CHB]) != 0
        ::add_to_debug(Config():get_config('VariableRepeating'))
        RETURN .F.
    ENDIF
    AAdd(::axUsedKeys, axRow[L_ID_VAR_CHB])

    axRow[L_ID_VAR_CHB] := AllTrim(Right(axRow[L_ID_VAR_CHB], Len(axRow[L_ID_VAR_CHB]) - 2))

    nIndex := AScan(::axKeys, axRow[L_ID_VAR_CHB])

    IF nIndex == 0
        ::add_to_debug(Config():get_config('UnknownVariable'))
        RETURN .F.
    ELSE
        ::axValues[nIndex] := cast(::axValues[nIndex], cType)
    ENDIF

    FOR i := C_CAPTION_CHB TO C_STYLE_CHB

        IF !::basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

        IF cType != 'C'
            ::add_to_debug(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF

        IF i == C_COLOR_CHB
            IF !is_color(axRow[i])
                ::add_to_debug(Config():get_config('IncorrectValue'))
                RETURN .F.
            ELSEIF Empty(hb_ColorIndex(axRow[i], 3)) .OR. !Empty(hb_ColorIndex(axRow[i], 4))
                ::add_to_debug(Config():get_config('IncorrectValue'))
                RETURN .F.
            ENDIF
        ELSEIF i == C_STYLE_CHB .AND. !is_Style(axRow[i])
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
    NEXT

RETURN .T.

METHOD validate_listbox(axRow, hVariables) CLASS Parser

    LOCAL cType
    LOCAL hHash
    LOCAL nIndex
    LOCAL i

    IF Len(axRow) != L_SCROLLBAR_LSB
        ::add_to_debug(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF

    FOR i := N_TOP_LSB TO N_RIGHT_LSB

        IF !::basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

        IF cType != 'N'
            ::add_to_debug(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF

        axRow[i] := cast(axRow[i], cType)

        IF ValType(axRow[i]) != cType
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
#ifdef VALIDATE_DIMENSIONS
        ELSEIF axRow[i] < 0
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
#endif
        ENDIF
    NEXT

    IF axRow[N_BOTTOM_LSB] < axRow[N_TOP_LSB] .OR. axRow[N_RIGHT_LSB] < axRow[N_LEFT_LSB]
        ::add_to_debug(Config():get_config('IncorrectDimensions'))
        RETURN .F.
#ifdef VALIDATE_DIMENSIONS
    ELSEIF axRow[N_BOTTOM_LSB] > MaxRow() .OR. axRow[N_TOP_LSB] > MaxRow() .OR. axRow[N_RIGHT_LSB] > MaxCol() .OR. axRow[N_LEFT_LSB] > MaxCol()
        ::add_to_debug(Config():get_config('IncorrectValue'))
        RETURN .F.
#endif
    ENDIF

    IF Left(axRow[NC_ID_VAR_LSB], 1) != VARIABLE
        ::add_to_debug(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF

    cType := SubStr(axRow[NC_ID_VAR_LSB], 2, 1)

    IF cType != 'N' .AND. cType != 'C'
        ::add_to_debug(Config():get_config('IncorrectDataType'))
        RETURN .F.
    ENDIF

    IF AScan(::axUsedKeys, axRow[NC_ID_VAR_LSB]) != 0
        ::add_to_debug(Config():get_config('VariableRepeating'))
        RETURN .F.
    ENDIF
    AAdd(::axUsedKeys, axRow[NC_ID_VAR_LSB])

    axRow[NC_ID_VAR_LSB] := AllTrim(Right(axRow[NC_ID_VAR_LSB], Len(axRow[NC_ID_VAR_LSB]) - 2))

    nIndex := AScan(::axKeys, axRow[NC_ID_VAR_LSB])

    IF nIndex == 0
        ::add_to_debug(Config():get_config('UnknownVariable'))
        RETURN .F.
    ELSE
        ::axValues[nIndex] := cast(::axValues[nIndex], cType)
    ENDIF

    IF ValType(::axValues[nIndex]) != cType
        ::add_to_debug(Config():get_config('IncorrectValue'))
        RETURN .F.
    ENDIF

    IF !::basic_parse(@axRow[A_LIST_LSB], @cType, hVariables)
        RETURN .F.
    ENDIF

    IF cType != 'A'
        ::add_to_debug(Config():get_config('IncorrectDataType'))
        RETURN .F.
    ENDIF

    IF ValType(axRow[A_LIST_LSB]) != 'A'
        hHash := hb_JsonDecode(axRow[A_LIST_LSB])

        IF ValType(hHash) != 'H'
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
        axRow[A_LIST_LSB] := hHash[hb_hKeys(hHash)[1]]
    ENDIF

    FOR i := C_CAPTION_LSB TO C_STATE_FNC_LSB

        IF !::basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

        IF cType != 'C'
            ::add_to_debug(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF

        IF i == C_COLOR_LSB .AND. !is_color(axRow[i])
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
    NEXT

    FOR i := L_DROPDOWN_LSB TO L_SCROLLBAR_LSB

        IF !::basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

        IF cType != 'L'
            RETURN .F.
        ENDIF

        axRow[i] := cast(axRow[i], cType)

        IF ValType(axRow[i]) != cType
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
    NEXT

    IF axRow[L_DROPDOWN_LSB]
        IF Empty(hb_ColorIndex(axRow[C_COLOR_LSB], 7)) .OR. !Empty(hb_ColorIndex(axRow[C_COLOR_LSB], 8))
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
    ELSE
        IF Empty(hb_ColorIndex(axRow[C_COLOR_LSB], 6)) .OR. !Empty(hb_ColorIndex(axRow[C_COLOR_LSB], 7))
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
    ENDIF

RETURN .T.

METHOD validate_radiogroup(axRow, hVariables) CLASS Parser

    LOCAL cType
    LOCAL hHash
    LOCAL nIndex
    LOCAL i

    IF Len(axRow) != C_VALID_FNC_RGB
        ::add_to_debug(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF

    FOR i := N_TOP_RGB TO N_RIGHT_RGB

        IF !::basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

        IF cType != 'N'
            ::add_to_debug(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF

        axRow[i] := cast(axRow[i], cType)

        IF ValType(axRow[i]) != cType
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
#ifdef VALIDATE_DIMENSIONS
        ELSEIF axRow[i] < 0
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
#endif
        ENDIF
    NEXT

    IF axRow[N_BOTTOM_RGB] < axRow[N_TOP_RGB] .OR. axRow[N_RIGHT_RGB] < axRow[N_LEFT_RGB]
        ::add_to_debug(Config():get_config('IncorrectDimensions'))
        RETURN .F.
#ifdef VALIDATE_DIMENSIONS
    ELSEIF axRow[N_BOTTOM_RGB] > MaxRow() .OR. axRow[N_TOP_RGB] > MaxRow() .OR. axRow[N_RIGHT_RGB] > MaxCol() .OR. axRow[N_LEFT_RGB] > MaxCol()
        ::add_to_debug(Config():get_config('IncorrectValue'))
        RETURN .F.
#endif
    ENDIF

    IF Left(axRow[NC_ID_VAR_RGB], 1) != VARIABLE
        ::add_to_debug(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF

    cType := SubStr(axRow[NC_ID_VAR_RGB], 2, 1)

    IF cType != 'N' .AND. cType != 'C'
        ::add_to_debug(Config():get_config('IncorrectDataType'))
        RETURN .F.
    ENDIF

    IF AScan(::axUsedKeys, axRow[NC_ID_VAR_RGB]) != 0
        ::add_to_debug(Config():get_config('VariableRepeating'))
        RETURN .F.
    ENDIF
    AAdd(::axUsedKeys, axRow[NC_ID_VAR_RGB])

    axRow[NC_ID_VAR_RGB] := AllTrim(Right(axRow[NC_ID_VAR_RGB], Len(axRow[NC_ID_VAR_RGB]) - 2))

    nIndex := AScan(::axKeys, axRow[NC_ID_VAR_RGB])

    IF nIndex == 0
        ::add_to_debug(Config():get_config('UnknownVariable'))
        RETURN .F.
    ELSE
        ::axValues[nIndex] := cast(::axValues[nIndex], cType)
    ENDIF

    axRow[NC_ID_VAR_RGB] := cast(axRow[NC_ID_VAR_RGB], cType)

    IF ValType(axRow[NC_ID_VAR_RGB]) != cType
        ::add_to_debug(Config():get_config('IncorrectValue'))
        RETURN .F.
    ENDIF

    IF !::basic_parse(@axRow[A_GROUP_RGB], @cType, hVariables)
        RETURN .F.
    ENDIF

    IF cType != 'A'
        ::add_to_debug(Config():get_config('IncorrectDataType'))
        RETURN .F.
    ENDIF

    IF ValType(axRow[A_GROUP_RGB]) != 'A'
        hHash := hb_JsonDecode(axRow[A_GROUP_RGB])
        IF ValType(hHash) != 'H'
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
        axRow[A_GROUP_RGB] := hHash[hb_hKeys(hHash)[1]]
    ENDIF

    FOR i := C_CAPTION_RGB TO C_VALID_FNC_RGB

        IF !::basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

        IF cType != 'C'
            ::add_to_debug(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF

        IF i == C_COLOR_RGB 
            IF !is_color(axRow[i])
                ::add_to_debug(Config():get_config('IncorrectValue'))
                RETURN .F.
            ELSEIF Empty(hb_ColorIndex(axRow[i], 2)) .OR. !Empty(hb_ColorIndex(axRow[i], 3))
                ::add_to_debug(Config():get_config('IncorrectValue'))
                RETURN .F.
            ENDIF
        ENDIF
    NEXT

RETURN .T.

METHOD validate_pushbutton(axRow, hVariables) CLASS Parser

    LOCAL cType
    LOCAL nIndex
    LOCAL i

    IF Len(axRow) != C_STYLE_PSB
        ::add_to_debug(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF

    FOR i := N_ROW_PSB TO N_COL_PSB

        IF !::basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

        IF cType != 'N'
            ::add_to_debug(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF

        axRow[i] := cast(axRow[i], cType)

        IF ValType(axRow[i]) != cType
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
#ifdef VALIDATE_DIMENSIONS
        ELSEIF axRow[i] < 0
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
#endif
        ENDIF
    NEXT

    IF Left(axRow[L_ID_VAR_PSB], 1) != VARIABLE
        ::add_to_debug(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF

    cType := SubStr(axRow[L_ID_VAR_PSB], 2, 1)

    IF AScan(::axUsedKeys, axRow[L_ID_VAR_PSB]) != 0
        ::add_to_debug(Config():get_config('VariableRepeating'))
        RETURN .F.
    ENDIF
    AAdd(::axUsedKeys, axRow[L_ID_VAR_PSB])

    nIndex := AScan(::axKeys, axRow[L_ID_VAR_PSB])

    IF nIndex == 0
        ::add_to_debug(Config():get_config('UnknownVariable'))
        RETURN .F.
    ELSE
        ::axValues[nIndex] := cast(::axValues[nIndex], cType)
    ENDIF

    FOR i := C_CAPTION_PSB TO C_COLOR_PSB

        IF !::basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

        IF cType != 'C'
            ::add_to_debug(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF

        IF i == C_COLOR_PSB
            IF !is_color(axRow[i])
                ::add_to_debug(Config():get_config('IncorrectValue'))
                RETURN .F.
            ELSEIF Empty(hb_ColorIndex(axRow[i], 3)) .OR. !Empty(hb_ColorIndex(axRow[i], 5))
                ::add_to_debug(Config():get_config('IncorrectValue'))
                RETURN .F.
            ENDIF
        ENDIF
    NEXT

    FOR i := C_FOCUS_FNC_PSB TO C_STYLE_PSB

        IF !::basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

        IF cType != 'C'
            ::add_to_debug(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF

        IF i == C_STYLE_PSB .AND. !is_style(axRow[i])
            ::add_to_debug(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
    NEXT

RETURN .T.

METHOD make_pushbutton(nRow, nCol, xIdVar, cCaption, cMessage, cWhen, cValid, cColor, cFocus, cState, cStyle) CLASS Parser

    MEMVAR GETLIST

    LOCAL nPosition := AScan(::axKeys, xIdVar)

    IF nPosition == 0
        RETURN .F.
    ENDIF

    SetPos(nRow, nCol)

    IF ::lUseMemvar
        AAdd(GETLIST, _GET_(::axValues[nPosition], "::axValues[nPosition]", NIL, &(cValid), &(cWhen)))
        ATail(GETLIST):control := _PushButt_(cCaption, cMessage, cColor, {|| &(cFocus)}, {|| &(cState)}, cStyle)
        ATail(GETLIST):reader := {| a, b, c, d | GUIReader(a, b, c, d)}
        ATail(GETLIST):control:display()
    ELSE
        AAdd(::aoGetList, _GET_(::axValues[nPosition], "::axValues[nPosition]", NIL, &(cValid), &(cWhen)))
        ATail(::aoGetList):control := _PushButt_(cCaption, cMessage, cColor, {|| &(cFocus)}, {|| &(cState)}, cStyle)
        ATail(::aoGetList):reader := {| a, b, c, d | GUIReader(a, b, c, d)}
        ATail(::aoGetList):control:display()
    ENDIF

RETURN .T.

METHOD make_radiogroup(nTop, nLeft, nBottom, nRight, xIdVar, aGroup, cCaption, cMessage, cColor, cFocus, cWhen, cValid) CLASS Parser

    MEMVAR GETLIST

    LOCAL nPosition := AScan(::axKeys, xIdVar)

    IF nPosition == 0
        RETURN .F.
    ENDIF

    SetPos(nTop, nLeft)

    IF ::lUseMemvar
        AAdd(GETLIST, _GET_(::axValues[nPosition], "::axValues[nPosition]", NIL, &(cValid), &(cWhen)))
        ATail(GETLIST):control := _RadioGrp_(ATail(GETLIST):row, ATail(GETLIST):col, nBottom, nRight;
                                             , ::axValues[nPosition], ::make_buttons(aGroup), cCaption;
                                             , cMessage, cColor, {|| &(cFocus)};
                                            )
        ATail(GETLIST):reader := {| a, b, c, d | GUIReader(a, b, c, d)}
        ATail(GETLIST):control:display()
    ELSE
        AAdd(::aoGetList, _GET_(::axValues[nPosition], "::axValues[nPosition]", NIL, &(cValid), &(cWhen)))
        ATail(::aoGetList):control := _RadioGrp_(ATail(::aoGetList):row, ATail(::aoGetList):col, nBottom;
                                                 , nRight, ::axValues[nPosition], ::make_buttons(aGroup);
                                                 , cCaption, cMessage, cColor, {|| &(cFocus)};
                                                )
        ATail(::aoGetList):reader := {| a, b, c, d | GUIReader(a, b, c, d)}
        ATail(::aoGetList):control:display()
    ENDIF

RETURN .T.

METHOD make_checkbox(nRow, nCol, xIdVar, cCaption, cMessage, cWhen, cValid, cColor, cFocus, cState, cStyle) CLASS Parser

    MEMVAR GETLIST

    LOCAL nPosition := AScan(::axKeys, xIdVar)

    IF nPosition == 0
        RETURN .F.
    ENDIF

    SetPos(nRow, nCol)

    IF ::lUseMemvar
        AAdd(GETLIST, _GET_(::axValues[nPosition], '::axValues[nPosition]', NIL, &(cValid), &(cWhen)))
        ATail(GETLIST):control := _CheckBox_(::axValues[nPosition], cCaption, cMessage, cColor, {|| &(cFocus)}, {|| &(cState)}, cStyle)
        ATail(GETLIST):reader := {|a, b, c, d| GUIReader(a, b, c, d)}
        ATail(GETLIST):control:display()
    ELSE
        AAdd(::aoGetList, _GET_(::axValues[nPosition], '::axValues[nPosition]', NIL, &(cValid), &(cWhen)))
        ATail(::aoGetList):control := _CheckBox_(::axValues[nPosition], cCaption, cMessage, cColor, {|| &(cFocus)}, {|| &(cState)}, cStyle)
        ATail(::aoGetList):reader := {|a, b, c, d| GUIReader(a, b, c, d)}
        ATail(::aoGetList):control:display()
    ENDIF

RETURN .T.

METHOD make_listbox(nTop, nLeft, nBottom, nRight, xIdVar, axList, cCaption, cMessage, cWhen, cValid, cColor, cFocus, cState, lDropDown, lScrollBar) CLASS Parser

    MEMVAR GETLIST

    LOCAL nPosition := AScan(::axKeys, xIdVar)

    IF nPosition == 0
        RETURN .F.
    ENDIF

    SetPos(nTop, nLeft)

    IF ::lUseMemvar
        AAdd(GETLIST, _GET_(::axValues[nPosition], "::axValues[nPosition]", NIL, &(cValid), &(cWhen)))
        ATail(GETLIST):control := _ListBox_(ATail(GETLIST):row, ATail(GETLIST):col, nBottom, nRight;
                                            , ::axValues[nPosition], axList, cCaption, cMessage, cColor;
                                            , {|| &(cFocus)}, {|| &(cState)}, lDropDown, lScrollBar;
                                           )
        ATail(GETLIST):reader := {| a, b, c, d | GUIReader(a, b, c, d)}
        ATail(GETLIST):control:display()
    ELSE
        AAdd(::aoGetList, _GET_(::axValues[nPosition], "::axValues[nPosition]", NIL, &(cValid), &(cWhen)))
        ATail(::aoGetList):control := _ListBox_(ATail(::aoGetList):row, ATail(::aoGetList):col, nBottom;
                                                , nRight, ::axValues[nPosition], axList, cCaption, cMessage;
                                                , cColor, {|| &(cFocus)}, {|| &(cState)}, lDropDown, lScrollBar;
                                               )
        ATail(::aoGetList):reader := {| a, b, c, d | GUIReader(a, b, c, d)}
        ATail(::aoGetList):control:display()
    ENDIF


RETURN .T.

METHOD make_get(nRow, nCol, cExp, cSayPicture, cColorStringSay, xIdVar, cGetPicture, cColorStringGet, cCaption, cMessage, cWhen, cValid) CLASS Parser

    MEMVAR GETLIST
    
    LOCAL nPosition := AScan(::axKeys, xIdVar)
    LOCAL lContainsSay := IF(Empty(cExp), .F., .T.)

    IF nPosition == 0
        RETURN .F.
    ENDIF

    IF lContainsSay
        DevPos(nRow, nCol)
        DevOutPict(cExp, cSayPicture, cColorStringSay)
        SetPos(Row(), Col() + 1)
    ELSE
        SetPos(nRow, nCol)
    ENDIF

    IF ::lUseMemvar
        AAdd(GETLIST, _GET_(::axValues[nPosition], "::axValues[nPosition]", cGetPicture, &(cValid), &(cWhen)))
        ATail(GETLIST):caption := cCaption
        ATail(GETLIST):caprow := ATail(GETLIST):row
        ATail(GETLIST):capcol := ATail(GETLIST):col - __CapLength(cCaption) - 1
        ATail(GETLIST):message := cMessage
        ATail(GETLIST):colordisp(cColorStringGet)
        ATail(GETLIST):display()
    ELSE
        AAdd(::aoGetList, _GET_(::axValues[nPosition], "::axValues[nPosition]", cGetPicture, &(cValid), &(cWhen)))
        ATail(::aoGetList):caption := cCaption
        ATail(::aoGetList):caprow := ATail(::aoGetList):row
        ATail(::aoGetList):capcol := ATail(::aoGetList):col - __CapLength(cCaption) - 1
        ATail(::aoGetList):message := cMessage
        ATail(::aoGetList):colordisp(cColorStringGet)
        ATail(::aoGetList):display()
    ENDIF

RETURN .T.

METHOD make_box(nTop, nLeft, nBottom, nRight, cBoxString, cColorString) CLASS Parser

    DispBox(nTop, nLeft, nBottom, nRight, cBoxString, cColorString)

RETURN .T.

METHOD make_window(nTop, nLeft, nBottom, nRight, cBox, cColor, xShadow) CLASS Parser

    LOCAL cOldColor := SetColor(cColor)
    LOCAL nOldShadow := WSetShadow(xShadow)
    LOCAL nOldWindow := WSelect()
    LOCAL lClear := !Empty(cBox) .AND. !Empty(cColor)

    ::nWindow := WOpen(nTop, nLeft, nBottom, nRight, lClear)
    WBox(cBox)

    WSelect(nOldWindow)
    WSetShadow(NToColor(nOldShadow))
    SET COLOR TO (cOldColor)
    WSelect(::nWindow)

RETURN ::nWindow != -1

METHOD make_say(nRow, nCol, cExp, cSayPicture, cColorString) CLASS Parser

    DevPos(nRow, nCol)
    DevOutPict(cExp, cSayPicture, cColorString)

RETURN .T.
