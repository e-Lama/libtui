#include "hbclass.ch"
#include "color.ch"

#include "parser.ch"

#include "setup.ch"

CREATE CLASS Parser

EXPORTED:

    METHOD prepare_form_from_database(cLanguage, cId, hVariables, cDatabase)
    METHOD prepare_form_from_record(acRows, hVariables)
    METHOD check_correctness(acRows, hVariables)
    METHOD get_answers()

    METHOD log(cLog) SETGET

    METHOD get_window_handler() INLINE ::__nWindow

    METHOD getlist(aoGetList) SETGET
    METHOD use_memvar(lUseMemVar) SETGET

HIDDEN:

    CLASSVAR __cLog AS CHARACTER INIT ''
    CLASSVAR __axKeys AS ARRAY INIT Array(0)
    CLASSVAR __axValues AS ARRAY INIT Array(0)
    CLASSVAR __axUsedKeys AS ARRAY INIT Array(0)
    CLASSVAR __nWindow AS NUMERIC INIT -1
    CLASSVAR __aoGetList AS ARRAY INIT Array(0)
    CLASSVAR __lUseMemVar AS LOGICAL INIT .T.

    METHOD __add_to_log(cTxt) INLINE ::__cLog += cTxt
    METHOD __validate_window(axRow, hVariables)
    METHOD __validate_box(axRow, hVariables)
    METHOD __validate_say(axRow, hVariables)
    METHOD __validate_get(axRow, hVariables)
    METHOD __validate_checkbox(axRow, hVariables)
    METHOD __validate_listbox(axRow, hVariables)
    METHOD __validate_radiogroup(axRow, hVariables)
    METHOD __validate_pushbutton(axRow, hVariables)
    METHOD __validate(axRow, hVariables)

    METHOD __make_window(nTop, nLeft, nBottom, nRight, cBox, cColor, xShadow)
    METHOD __make_say(nRow, nCol, cExp, cSayPicture, cColorString)
    METHOD __make_radiogroup(nTop, nLeft, nBottom, nRight, xIdVar, acGroup, cCaption;
                             , cMessage, cColor, cFocus, cWhen, cValid;
                            )
    METHOD __make_box(nTop, nLeft, nBottom, nRight, cBoxString, cColorString)
    METHOD __make_get(nRow, nCol, cExp, cSayPicture, cColorStringSay, xIdVar, cGetPicture;
                      , cColorStringGet, cCaption, cMessage, cWhen, cValid;
                     )
    METHOD __make_listbox(nTop, nLeft, nBottom, nRight, xIdVar, axList, cCaption, cMessage;
                          , cWhen, cValid, cColor, cFocus, cState, lDropDown, lScrollBar;
                         )
    METHOD __make_checkbox(nRow, nCol, xIdVar, cCaption, cMessage, cWhen, cValid, cColor, cFocus, cState, cStyle)
    METHOD __make_pushbutton(nRow, nCol, xIdVar, cCaption, cMessage, cWhen, cValid, cColor, cFocus, cState, cStyle)

    METHOD __make_buttons(acPar)

#ifdef USE_VALIDATORS
    METHOD __corrupted_row(cRow)
#endif
    METHOD __basic_parse(xRow, cType, hVariables)
    METHOD __handle_hash(hVariables)
    METHOD __handle_object(axRow)

ENDCLASS LOCK 

METHOD getlist(aoGetList) CLASS Parser

    LOCAL aoWasGetList := clone_objects_array(::__aoGetList)

    IF aoGetList != NIL
#ifdef USE_VALIDATORS
        assert_type(aoGetList, 'A')
        AEval(aoGetList, {| oElement | assert_type(oElement, 'O')})
#endif
        ::__aoGetList := aoGetList
    ENDIF

RETURN aoWasGetList

METHOD use_memvar(lUseMemVar) CLASS Parser

    LOCAL lWasUseMemvar := ::__lUseMemVar

    IF lUseMemVar != NIL
#ifdef USE_VALIDATORS
        assert_type(lUseMemVar, 'L')
#endif
        ::__lUseMemVar := lUseMemVar
    ENDIF

RETURN lWasUseMemvar

METHOD log(cLog) CLASS Parser

    LOCAL cWasLog := ::__cLog

    IF cLog != NIL
#ifdef USE_VALIDATORS
        assert_type(cLog, 'C')
#endif
        ::__cLog := cLog
    ENDIF

RETURN cWasLog

METHOD get_answers() CLASS Parser

    LOCAL hAnswers := hb_Hash()
    LOCAL i

    FOR i := 1 TO Len(::__axKeys)
        hAnswers[::__axKeys[i]] := ::__axValues[i]
    NEXT

RETURN hAnswers

METHOD __make_buttons(acPar) CLASS Parser

    LOCAL aoButtons := Array(0)
    LOCAL cRow
    LOCAL acParams
    
    FOR EACH cRow IN acPar
        acParams := hb_ATokens(cRow, ',')
        AAdd(aoButtons, RadioButto(Val(acParams[N_ROW_RBT]), Val(acParams[N_COL_RBT]), acParams[C_CAPTION_RBT], acParams[C_VALUE_RBT]))
    NEXT

RETURN aoButtons
    
METHOD __handle_hash(hVariables) CLASS Parser
    
    ::__axKeys := hb_HKeys(hVariables)
    ::__axValues := hb_HValues(hVariables)
    ::__axUsedKeys := Array(0)

RETURN NIL

METHOD __handle_object(axRow) CLASS Parser

    DO CASE 
        CASE axRow[OBJECT] == OBJECT_WINDOW
            RETURN ::__make_window(axRow[N_TOP_WN];
                                     , axRow[N_LEFT_WN];
                                     , axRow[N_BOTTOM_WN];
                                     , axRow[N_RIGHT_WN];
                                     , axRow[C_BOX_WN];
                                     , axRow[C_COLOR_WN];
                                     , axRow[NC_SHADOW_WN];
                                     )
        CASE axRow[OBJECT] == OBJECT_BOX
            RETURN ::__make_box(axRow[N_TOP_BOX];
                                     , axRow[N_LEFT_BOX];
                                     , axRow[N_BOTTOM_BOX];
                                     , axRow[N_RIGHT_BOX];
                                     , axRow[C_BOX_BOX];
                                     , axRow[C_COLOR_BOX];
                                     )
        CASE axRow[OBJECT] == OBJECT_SAY 
            RETURN ::__make_say(axRow[N_ROW_SAY];
                                     , axRow[N_COL_SAY];
                                     , axRow[C_EXPRESSION_SAY];
                                     , axRow[C_PICTURE_SAY];
                                     , axRow[C_COLOR_SAY];
                                     )
        CASE axRow[OBJECT] == OBJECT_GET
            RETURN ::__make_get(axRow[N_ROW_GET];
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
            RETURN ::__make_checkbox(axRow[N_ROW_CHB];
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
            RETURN ::__make_listbox(axRow[N_TOP_LSB];
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
            RETURN ::__make_radiogroup(axRow[N_TOP_RGB];
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
            RETURN ::__make_pushbutton(axRow[N_ROW_PSB];
                                     , axRow[N_COL_PSB];
                                     , axRow[L_ID_VAR_PSB];
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

#ifdef USE_VALIDATORS
    assert_type(acRows, 'A')
    assert_type(hVariables, 'H')

    AEval(acRows, {| cElement | assert_type(cElement, 'C')})
#endif

    ::__handle_hash(hVariables)
    ::__nWindow := -1

    acRowsCopy := AClone(acRows)

    FOR EACH acRow IN acRowsCopy
        acRow := hb_ATokens(acRow, LINE_SEPARATOR)

        IF acRow[OBJECT] == OBJECT_WINDOW
            IF lWasWindow
                RETURN .F.
            ENDIF
            lWasWindow := .T.
            IF acRow:__enumIndex() != 1
                ::__add_to_log(Config():get_config('WindowMustBeFirst'))
                RETURN .F.
            ENDIF
        ENDIF

        IF !::__validate(acRow, hVariables)
            RETURN .F.
        ENDIF
    NEXT

RETURN .T.

METHOD prepare_form_from_record(acRows, hVariables) CLASS Parser

    LOCAL lWasWindow := .F.
    LOCAL acRowsCopy
    LOCAL acRow

#ifdef USE_VALIDATORS
    assert_type(acRows, 'A')
    assert_type(hVariables, 'H')

    AEval(acRows, {| cElement | assert_type(cElement, 'C')})
#endif

    ::__handle_hash(hVariables)
    ::__nWindow := -1

    acRowsCopy := AClone(acRows)

    FOR EACH acRow IN acRowsCopy
        acRow := hb_ATokens(acRow, LINE_SEPARATOR)

        IF acRow[OBJECT] == OBJECT_WINDOW
            IF lWasWindow
                RETURN .F.
            ENDIF

            lWasWindow := .T.

            IF acRow:__enumIndex() != 1
                ::__add_to_log(Config():get_config('WindowMustBeFirst'))
                RETURN .F.
            ENDIF
        ENDIF

        IF !::__validate(@acRow, hVariables)
            RETURN .F.
        ENDIF

        IF !::__handle_object(acRow)
            RETURN .F.
        ENDIF

    NEXT

RETURN .T.
 
METHOD prepare_form_from_database(cLanguage, cId, hVariables, cDatabase) CLASS Parser

    LOCAL nOldSelect := Select()
    LOCAL nOldRecNo
    LOCAL lResult

#ifdef USE_VALIDATORS
    assert_type(cLanguage, 'C')
    assert_type(cId, 'C')
    assert_type(hVariables, 'H')
#endif

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

#ifdef USE_VALIDATORS
    assert_type(field->code, 'M')
#endif

    lResult := ::prepare_form_from_record(hb_ATokens(field->code, OBJECT_SEPARATOR), hVariables)

    GO nOldRecNo
    SELECT (nOldSelect)

RETURN lResult

#ifdef USE_VALIDATORS
METHOD __corrupted_row(cRow) CLASS Parser

    IF Len(cRow) < 2
        RETURN .T.
    ELSEIF Left(cRow, 1) != VARIABLE .AND. Left(cRow, 1) != CONSTANT
        RETURN .T.
    ELSEIF !is_data_type(SubStr(cRow, 2, 1))
        RETURN .T.
    ENDIF

RETURN .F.
#endif

METHOD __basic_parse(xRow, cType, hVariables) CLASS Parser

    LOCAL lInHash := .F.

#ifdef USE_VALIDATORS
    IF ::__corrupted_row(xRow)
        ::__add_to_log(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF
#endif
   
    IF Left(xRow, 1) == VARIABLE
        lInHash := .T.
    ENDIF

    cType := SubStr(xRow, 2, 1)
    xRow := Right(xRow, Len(xRow) - 2)

    IF lInHash
        xRow := AllTrim(xRow)
        IF hb_hHasKey(hVariables, xRow)
            IF AScan(::__axUsedKeys, xRow) != 0
#ifdef USE_VALIDATORS
                ::__add_to_log(Config():get_config('VariableRepeating'))
#endif
                RETURN .F.
            ENDIF
            AAdd(::__axUsedKeys, xRow)
            xRow := hVariables[xRow] 
        ELSE
#ifdef USE_VALIDATORS
            ::__add_to_log(Config():get_config('CorruptionDetected'))
#endif
            RETURN .F.
        ENDIF
    ENDIF

RETURN .T.

METHOD __validate(axRow, hVariables) CLASS Parser

    DO CASE
        CASE axRow[OBJECT] == OBJECT_WINDOW
            RETURN ::__validate_window(@axRow, hVariables)
        CASE axRow[OBJECT] == OBJECT_BOX
            RETURN ::__validate_box(@axRow, hVariables)
        CASE axRow[OBJECT] == OBJECT_SAY
            RETURN ::__validate_say(@axRow, hVariables)
        CASE axRow[OBJECT] == OBJECT_GET
            RETURN ::__validate_get(@axRow, hVariables)
        CASE axRow[OBJECT] == OBJECT_CHECKBOX
            RETURN ::__validate_checkbox(@axRow, hVariables)
        CASE axRow[OBJECT] == OBJECT_LISTBOX
            RETURN ::__validate_listbox(@axRow, hVariables)
        CASE axRow[OBJECT] == OBJECT_RADIOGROUP
            RETURN ::__validate_radiogroup(@axRow, hVariables)
        CASE axRow[OBJECT] == OBJECT_PUSHBUTTON
            RETURN ::__validate_pushbutton(@axRow, hVariables)
        CASE Empty(axRow[OBJECT])
            ::__add_to_log(Config():get_config('EmptyObject'))
        OTHERWISE
            ::__add_to_log(Config():get_config('UnknownObject'))
    ENDCASE

RETURN .F.

METHOD __validate_window(axRow, hVariables) CLASS Parser

    LOCAL cType
    LOCAL i

#ifdef USE_VALIDATORS
    IF Len(axRow) != NC_SHADOW_WN
        ::__add_to_log(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF
#endif

    FOR i := N_TOP_WN TO N_RIGHT_WN

        IF !::__basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

#ifdef USE_VALIDATORS
        IF cType != 'N'
            ::__add_to_log(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF
#endif

        axRow[i] := cast(axRow[i], cType)

#ifdef USE_VALIDATORS
        IF ValType(axRow[i]) != cType
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
#endif
#ifdef VALIDATE_DIMENSIONS
        IF axRow[i] < 0
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
#endif
    NEXT

#ifdef USE_VALIDATORS
    IF axRow[N_BOTTOM_WN] < axRow[N_TOP_WN] .OR. axRow[N_RIGHT_WN] < axRow[N_LEFT_WN]
        ::__add_to_log(Config():get_config('IncorrectDimensions'))
        RETURN .F.
    ENDIF
#endif
#ifdef VALIDATE_DIMENSIONS
    IF axRow[N_BOTTOM_WN] > MaxRow() .OR. axRow[N_TOP_WN] > MaxRow() .OR. axRow[N_RIGHT_WN] > MaxCol() .OR. axRow[N_LEFT_WN] > MaxCol()
        ::__add_to_log(Config():get_config('IncorrectValue'))
        RETURN .F.
    ENDIF
#endif

    IF !::__basic_parse(@axRow[C_BOX_WN], @cType, hVariables)
        RETURN .F.
    ENDIF

#ifdef USE_VALIDATORS
    IF !is_box(hb_Translate(axRow[C_BOX_WN], 'EN', hb_cdpSelect()))
        ::__add_to_log(Config():get_config('IncorrectValue'))
        RETURN .F.
    ENDIF
#endif

    IF !::__basic_parse(@axRow[C_COLOR_WN], @cType, hVariables)
        RETURN .F.
    ENDIF

#ifdef USE_VALIDATORS
    IF !is_color(axRow[C_COLOR_WN])
        ::__add_to_log(Config():get_config('IncorrectValue'))
        RETURN .F.
    ENDIF
#endif

    IF !::__basic_parse(@axRow[NC_SHADOW_WN], @cType, hVariables)
        RETURN .F.
    ENDIF

    axRow[NC_SHADOW_WN] := cast(axRow[NC_SHADOW_WN], cType)

#ifdef USE_VALIDATORS
    IF ValType(axRow[NC_SHADOW_WN]) != cType
        ::__add_to_log(Config():get_config('IncorrectValue'))
        RETURN .F.
    ENDIF
#endif

#ifdef USE_VALIDATORS
    IF cType == 'C'
        IF AScan({'N', 'B', 'G', 'BG', 'R', 'RB', 'GR', 'W', 'N+', 'B+', 'G+', 'BG+', 'R+', 'RB+', 'GR+', 'W+'}, axRow[NC_SHADOW_WN]) == 0
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
    ELSEIF cType == 'N'
        IF axRow[NC_SHADOW_WN] != -1
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
    ELSE
        ::__add_to_log(Config():get_config('IncorrectDataType'))
        RETURN .F.
    ENDIF
#endif

RETURN .T.

METHOD __validate_box(axRow, hVariables) CLASS Parser

    LOCAL cType
    LOCAL i

#ifdef USE_VALIDATORS
    IF Len(axRow) != C_COLOR_BOX
        ::__add_to_log(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF
#endif

    FOR i := N_TOP_BOX TO N_RIGHT_BOX

        IF !::__basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

#ifdef USE_VALIDATORS
        IF cType != 'N'
            ::__add_to_log(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF
#endif

        axRow[i] := cast(axRow[i], cType)

#ifdef USE_VALIDATORS
        IF ValType(axRow[i]) != cType
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
#endif
#ifdef VALIDATE_DIMENSIONS
        IF axRow[i] < 0
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
#endif
    NEXT

#ifdef USE_VALIDATORS
    IF axRow[N_BOTTOM_BOX] < axRow[N_TOP_BOX] .OR. axRow[N_RIGHT_BOX] < axRow[N_LEFT_BOX]
        ::__add_to_log(Config():get_config('IncorrectDimensions'))
        RETURN .F.
    ENDIF
#endif
#ifdef VALIDATE_DIMENSIONS
    IF axRow[N_BOTTOM_BOX] > MaxRow() .OR. axRow[N_TOP_BOX] > MaxRow() .OR. axRow[N_RIGHT_BOX] > MaxCol() .OR. axRow[N_LEFT_BOX] > MaxCol()
        ::__add_to_log(Config():get_config('IncorrectValue'))
        RETURN .F.
    ENDIF
#endif

    IF !::__basic_parse(@axRow[C_BOX_BOX], @cType, hVariables)
        RETURN .F.
    ENDIF

#ifdef USE_VALIDATORS
    IF !is_box(hb_Translate(axRow[C_BOX_BOX], 'EN', hb_cdpSelect()))
        ::__add_to_log(Config():get_config('IncorrectValue'))
        RETURN .F.
    ENDIF
#endif

    IF !::__basic_parse(@axRow[C_COLOR_BOX], @cType, hVariables)
        RETURN .F.
    ENDIF

#ifdef USE_VALIDATORS
    IF !is_color(axRow[C_COLOR_BOX])
        ::__add_to_log(Config():get_config('IncorrectValue'))
        RETURN .F.
    ENDIF
#endif
 
RETURN .T.

METHOD __validate_say(axRow, hVariables) CLASS Parser

    LOCAL cType
    LOCAL i

#ifdef USE_VALIDATORS
    IF Len(axRow) != C_COLOR_SAY
        ::__add_to_log(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF
#endif

    FOR i := N_ROW_SAY TO N_COL_SAY

        IF !::__basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

#ifdef USE_VALIDATORS
        IF cType != 'N'
            ::__add_to_log(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF
#endif

        axRow[i] := cast(axRow[i], cType)

#ifdef USE_VALIDATORS
        IF ValType(axRow[i]) != cType
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
#endif
#ifdef VALIDATE_DIMENSIONS
        IF axRow[i] < 0
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
#endif
    NEXT

#ifdef VALIDATE_DIMENSIONS
    IF axRow[N_ROW_SAY] > MaxRow() .OR. axRow[N_COL_SAY] > MaxCol()
        ::__add_to_log(Config():get_config('IncorrectValue'))
        RETURN .F.
    ENDIF
#endif

    IF !::__basic_parse(@axRow[C_EXPRESSION_SAY], @cType, hVariables)
        RETURN .F.
    ENDIF

#ifdef USE_VALIDATORS
    IF !(cType $ 'N;L;C;D')
        ::__add_to_log(Config():get_config('IncorrectDataType'))
        RETURN .F.
    ENDIF
#endif

    axRow[C_EXPRESSION_SAY] := cast(axRow[C_EXPRESSION_SAY], cType)

#ifdef USE_VALIDATORS
    IF ValType(axRow[C_EXPRESSION_SAY]) != cType
        ::__add_to_log(Config():get_config('IncorrectValue'))
        RETURN .F.
    ENDIF
#endif

    FOR i := C_PICTURE_SAY TO C_COLOR_SAY

        IF !::__basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

#ifdef USE_VALIDATORS
        IF cType != 'C'
            ::__add_to_log(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF

        IF i == C_PICTURE_SAY .AND. !is_picture(axRow[i])
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ELSEIF i == C_COLOR_SAY .AND. !is_color(axRow[i])
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
#endif
    NEXT

RETURN .T.

METHOD __validate_get(axRow, hVariables) CLASS Parser

    LOCAL axSayPart := Array(C_SAY_COLOR_GET)
    LOCAL nIndex
    LOCAL cType
    LOCAL i

#ifdef USE_VALIDATORS
    IF Len(axRow) != C_VALID_FNC_GET
        ::__add_to_log(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF
#endif
    
    ACopy(axRow, axSayPart, N_ROW_GET, C_SAY_COLOR_GET, N_ROW_SAY)

    IF !::__validate_say(@axSayPart, hVariables)
        RETURN .F.
    ENDIF

    FOR i := N_ROW_GET TO C_SAY_COLOR_GET
        axRow[i] := axSayPart[i]
    NEXT

#ifdef USE_VALIDATORS
    IF Left(axRow[X_ID_VAR_GET], 1) != VARIABLE
        ::__add_to_log(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF
#endif

    cType := SubStr(axRow[X_ID_VAR_GET], 2, 1)

#ifdef USE_VALIDATORS
    IF AScan({'C', 'D', 'L', 'N'}, cType) == 0
        ::__add_to_log(Config():get_config('IncorrectDataType'))
        RETURN .F.
    ENDIF

    IF AScan(::__axUsedKeys, axRow[X_ID_VAR_GET]) != 0
        ::__add_to_log(Config():get_config('VariableRepeating'))
        RETURN .F.
    ENDIF
#endif
    AAdd(::__axUsedKeys, axRow[X_ID_VAR_GET])

    axRow[X_ID_VAR_GET] := AllTrim(Right(axRow[X_ID_VAR_GET], Len(axRow[X_ID_VAR_GET]) - 2))

    nIndex := AScan(::__axKeys, axRow[X_ID_VAR_GET])

#ifdef USE_VALIDATORS
    IF nIndex == 0
        ::__add_to_log(Config():get_config('UnknownVariable'))
        RETURN .F.
    ELSE
#endif
        ::__axValues[nIndex] := cast(::__axValues[nIndex], cType)
#ifdef USE_VALIDATORS
    ENDIF
#endif

    FOR i := C_GET_PICTURE_GET TO C_VALID_FNC_GET

        IF !::__basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

#ifdef USE_VALIDATORS
        IF cType != 'C'
            ::__add_to_log(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF

        IF i == C_GET_PICTURE_GET .AND. !is_picture(axRow[i])
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ELSEIF i == C_GET_COLOR_GET .AND. !is_color(axRow[i])
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
#endif
    NEXT

RETURN .T.

METHOD __validate_checkbox(axRow, hVariables) CLASS Parser

    LOCAL cType
    LOCAL nIndex
    LOCAL i

#ifdef USE_VALIDATORS
    IF Len(axRow) != C_STYLE_CHB
        ::__add_to_log(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF
#endif

    FOR i := N_ROW_CHB TO N_COL_CHB

        IF !::__basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

#ifdef USE_VALIDATORS
        IF cType != 'N'
            ::__add_to_log(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF
#endif

        axRow[i] := cast(axRow[i], cType)

#ifdef USE_VALIDATORS
        IF ValType(axRow[i]) != cType
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
#endif
#ifdef VALIDATE_DIMENSIONS
        IF axRow[i] < 0
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
#endif
    NEXT

#ifdef VALIDATE_DIMENSIONS
    IF axRow[N_ROW_CHB] > MaxRow() .OR. axRow[N_COL_CHB] > MaxCol()
        RETURN .F.
    ENDIF
#endif

#ifdef USE_VALIDATORS
    IF Left(axRow[L_ID_VAR_CHB], 1) != VARIABLE
        ::__add_to_log(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF
#endif

    cType := SubStr(axRow[L_ID_VAR_CHB], 2, 1)

#ifdef USE_VALIDATORS
    IF cType != 'L'
        ::__add_to_log(Config():get_config('IncorrectDataType'))
        RETURN .F.
    ENDIF

    IF AScan(::__axUsedKeys, axRow[L_ID_VAR_CHB]) != 0
        ::__add_to_log(Config():get_config('VariableRepeating'))
        RETURN .F.
    ENDIF
#endif
    AAdd(::__axUsedKeys, axRow[L_ID_VAR_CHB])

    axRow[L_ID_VAR_CHB] := AllTrim(Right(axRow[L_ID_VAR_CHB], Len(axRow[L_ID_VAR_CHB]) - 2))

    nIndex := AScan(::__axKeys, axRow[L_ID_VAR_CHB])

#ifdef USE_VALIDATORS
    IF nIndex == 0
        ::__add_to_log(Config():get_config('UnknownVariable'))
        RETURN .F.
    ELSE
#endif
        ::__axValues[nIndex] := cast(::__axValues[nIndex], cType)
#ifdef USE_VALIDATORS
    ENDIF
#endif

    FOR i := C_CAPTION_CHB TO C_STYLE_CHB

        IF !::__basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

#ifdef USE_VALIDATORS
        IF cType != 'C'
            ::__add_to_log(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF

        IF i == C_COLOR_CHB
            IF !is_color(axRow[i])
                ::__add_to_log(Config():get_config('IncorrectValue'))
                RETURN .F.
            ELSEIF Empty(hb_ColorIndex(axRow[i], 3)) .OR. !Empty(hb_ColorIndex(axRow[i], 4))
                ::__add_to_log(Config():get_config('IncorrectValue'))
                RETURN .F.
            ENDIF
        ELSEIF i == C_STYLE_CHB .AND. !is_checkbox_style(axRow[i])
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
#endif
    NEXT

RETURN .T.

METHOD __validate_listbox(axRow, hVariables) CLASS Parser

    LOCAL cType
    LOCAL hHash
    LOCAL nIndex
    LOCAL i

#ifdef USE_VALIDATORS
    IF Len(axRow) != L_SCROLLBAR_LSB
        ::__add_to_log(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF
#endif

    FOR i := N_TOP_LSB TO N_RIGHT_LSB

        IF !::__basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

#ifdef USE_VALIDATORS
        IF cType != 'N'
            ::__add_to_log(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF
#endif

        axRow[i] := cast(axRow[i], cType)

#ifdef USE_VALIDATORS
        IF ValType(axRow[i]) != cType
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
#endif
#ifdef VALIDATE_DIMENSIONS
        IF axRow[i] < 0
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
#endif
    NEXT

#ifdef USE_VALIDATORS
    IF axRow[N_BOTTOM_LSB] < axRow[N_TOP_LSB] .OR. axRow[N_RIGHT_LSB] < axRow[N_LEFT_LSB]
        ::__add_to_log(Config():get_config('IncorrectDimensions'))
        RETURN .F.
    ENDIF
#endif
#ifdef VALIDATE_DIMENSIONS
    IF axRow[N_BOTTOM_LSB] > MaxRow() .OR. axRow[N_TOP_LSB] > MaxRow() .OR. axRow[N_RIGHT_LSB] > MaxCol() .OR. axRow[N_LEFT_LSB] > MaxCol()
        ::__add_to_log(Config():get_config('IncorrectValue'))
        RETURN .F.
    ENDIF
#endif

#ifdef USE_VALIDATORS
    IF Left(axRow[NC_ID_VAR_LSB], 1) != VARIABLE
        ::__add_to_log(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF
#endif

    cType := SubStr(axRow[NC_ID_VAR_LSB], 2, 1)

#ifdef USE_VALIDATORS
    IF cType != 'N' .AND. cType != 'C'
        ::__add_to_log(Config():get_config('IncorrectDataType'))
        RETURN .F.
    ENDIF

    IF AScan(::__axUsedKeys, axRow[NC_ID_VAR_LSB]) != 0
        ::__add_to_log(Config():get_config('VariableRepeating'))
        RETURN .F.
    ENDIF
#endif
    AAdd(::__axUsedKeys, axRow[NC_ID_VAR_LSB])

    axRow[NC_ID_VAR_LSB] := AllTrim(Right(axRow[NC_ID_VAR_LSB], Len(axRow[NC_ID_VAR_LSB]) - 2))

    nIndex := AScan(::__axKeys, axRow[NC_ID_VAR_LSB])

#ifdef USE_VALIDATORS
    IF nIndex == 0
        ::__add_to_log(Config():get_config('UnknownVariable'))
        RETURN .F.
    ELSE
#endif
        ::__axValues[nIndex] := cast(::__axValues[nIndex], cType)
#ifdef USE_VALIDATORS
    ENDIF
#endif

#ifdef USE_VALIDATORS
    IF ValType(::__axValues[nIndex]) != cType
        ::__add_to_log(Config():get_config('IncorrectValue'))
        RETURN .F.
    ENDIF
#endif

    IF !::__basic_parse(@axRow[A_LIST_LSB], @cType, hVariables)
        RETURN .F.
    ENDIF

#ifdef USE_VALIDATORS
    IF cType != 'A'
        ::__add_to_log(Config():get_config('IncorrectDataType'))
        RETURN .F.
    ENDIF
#endif

    IF ValType(axRow[A_LIST_LSB]) != 'A'
        hHash := hb_JsonDecode(axRow[A_LIST_LSB])

#ifdef USE_VALIDATORS
        IF ValType(hHash) != 'H'
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
#endif
        axRow[A_LIST_LSB] := hHash[hb_hKeys(hHash)[1]]
    ENDIF

    FOR i := C_CAPTION_LSB TO C_STATE_FNC_LSB

        IF !::__basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

#ifdef USE_VALIDATORS
        IF cType != 'C'
            ::__add_to_log(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF

        IF i == C_COLOR_LSB .AND. !is_color(axRow[i])
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
#endif
    NEXT

    FOR i := L_DROPDOWN_LSB TO L_SCROLLBAR_LSB

        IF !::__basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

#ifdef USE_VALIDATORS
        IF cType != 'L'
            RETURN .F.
        ENDIF
#endif

        axRow[i] := cast(axRow[i], cType)

#ifdef USE_VALIDATORS
        IF ValType(axRow[i]) != cType
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
#endif
    NEXT

#ifdef USE_VALIDATORS
    IF axRow[L_DROPDOWN_LSB]
        IF Empty(hb_ColorIndex(axRow[C_COLOR_LSB], 7)) .OR. !Empty(hb_ColorIndex(axRow[C_COLOR_LSB], 8))
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
    ELSE
        IF Empty(hb_ColorIndex(axRow[C_COLOR_LSB], 6)) .OR. !Empty(hb_ColorIndex(axRow[C_COLOR_LSB], 7))
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
    ENDIF
#endif

RETURN .T.

METHOD __validate_radiogroup(axRow, hVariables) CLASS Parser

    LOCAL cType
    LOCAL hHash
    LOCAL nIndex
    LOCAL i

#ifdef USE_VALIDATORS
    IF Len(axRow) != C_VALID_FNC_RGB
        ::__add_to_log(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF
#endif

    FOR i := N_TOP_RGB TO N_RIGHT_RGB

        IF !::__basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

#ifdef USE_VALIDATORS
        IF cType != 'N'
            ::__add_to_log(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF
#endif

        axRow[i] := cast(axRow[i], cType)

#ifdef USE_VALIDATORS
        IF ValType(axRow[i]) != cType
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
#endif
#ifdef VALIDATE_DIMENSIONS
        IF axRow[i] < 0
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
#endif
    NEXT

#ifdef USE_VALIDATORS
    IF axRow[N_BOTTOM_RGB] < axRow[N_TOP_RGB] .OR. axRow[N_RIGHT_RGB] < axRow[N_LEFT_RGB]
        ::__add_to_log(Config():get_config('IncorrectDimensions'))
        RETURN .F.
    ENDIF
#endif
#ifdef VALIDATE_DIMENSIONS
    IF axRow[N_BOTTOM_RGB] > MaxRow() .OR. axRow[N_TOP_RGB] > MaxRow() .OR. axRow[N_RIGHT_RGB] > MaxCol() .OR. axRow[N_LEFT_RGB] > MaxCol()
        ::__add_to_log(Config():get_config('IncorrectValue'))
        RETURN .F.
    ENDIF
#endif

#ifdef USE_VALIDATORS
    IF Left(axRow[NC_ID_VAR_RGB], 1) != VARIABLE
        ::__add_to_log(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF
#endif

    cType := SubStr(axRow[NC_ID_VAR_RGB], 2, 1)

#ifdef USE_VALIDATORS
    IF cType != 'N' .AND. cType != 'C'
        ::__add_to_log(Config():get_config('IncorrectDataType'))
        RETURN .F.
    ENDIF

    IF AScan(::__axUsedKeys, axRow[NC_ID_VAR_RGB]) != 0
        ::__add_to_log(Config():get_config('VariableRepeating'))
        RETURN .F.
    ENDIF
#endif
    AAdd(::__axUsedKeys, axRow[NC_ID_VAR_RGB])

    axRow[NC_ID_VAR_RGB] := AllTrim(Right(axRow[NC_ID_VAR_RGB], Len(axRow[NC_ID_VAR_RGB]) - 2))

    nIndex := AScan(::__axKeys, axRow[NC_ID_VAR_RGB])

#ifdef USE_VALIDATORS
    IF nIndex == 0
        ::__add_to_log(Config():get_config('UnknownVariable'))
        RETURN .F.
    ELSE
#endif
        ::__axValues[nIndex] := cast(::__axValues[nIndex], cType)
#ifdef USE_VALIDATORS
    ENDIF
#endif

    axRow[NC_ID_VAR_RGB] := cast(axRow[NC_ID_VAR_RGB], cType)

#ifdef USE_VALIDATORS
    IF ValType(axRow[NC_ID_VAR_RGB]) != cType
        ::__add_to_log(Config():get_config('IncorrectValue'))
        RETURN .F.
    ENDIF
#endif

    IF !::__basic_parse(@axRow[A_GROUP_RGB], @cType, hVariables)
        RETURN .F.
    ENDIF

#ifdef USE_VALIDATORS
    IF cType != 'A'
        ::__add_to_log(Config():get_config('IncorrectDataType'))
        RETURN .F.
    ENDIF
#endif

    IF ValType(axRow[A_GROUP_RGB]) != 'A'
        hHash := hb_JsonDecode(axRow[A_GROUP_RGB])
#ifdef USE_VALIDATORS
        IF ValType(hHash) != 'H'
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
#endif
        axRow[A_GROUP_RGB] := hHash[hb_hKeys(hHash)[1]]
    ENDIF

    FOR i := C_CAPTION_RGB TO C_VALID_FNC_RGB

        IF !::__basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

#ifdef USE_VALIDATORS
        IF cType != 'C'
            ::__add_to_log(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF

        IF i == C_COLOR_RGB 
            IF !is_color(axRow[i])
                ::__add_to_log(Config():get_config('IncorrectValue'))
                RETURN .F.
            ELSEIF Empty(hb_ColorIndex(axRow[i], 2)) .OR. !Empty(hb_ColorIndex(axRow[i], 3))
                ::__add_to_log(Config():get_config('IncorrectValue'))
                RETURN .F.
            ENDIF
        ENDIF
#endif
    NEXT

RETURN .T.

METHOD __validate_pushbutton(axRow, hVariables) CLASS Parser

    LOCAL cType
    LOCAL nIndex
    LOCAL i

#ifdef USE_VALIDATORS
    IF Len(axRow) != C_STYLE_PSB
        ::__add_to_log(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF
#endif

    FOR i := N_ROW_PSB TO N_COL_PSB

        IF !::__basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

#ifdef USE_VALIDATORS
        IF cType != 'N'
            ::__add_to_log(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF
#endif

        axRow[i] := cast(axRow[i], cType)

#ifdef USE_VALIDATORS
        IF ValType(axRow[i]) != cType
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
#endif
#ifdef VALIDATE_DIMENSIONS
        IF axRow[i] < 0
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
#endif
    NEXT

#ifdef VALIDATE_DIMENSIONS
    IF Left(axRow[L_ID_VAR_PSB], 1) != VARIABLE
        ::__add_to_log(Config():get_config('CorruptionDetected'))
        RETURN .F.
    ENDIF
#endif

    cType := SubStr(axRow[L_ID_VAR_PSB], 2, 1)

#ifdef VALIDATE_DIMENSIONS
    IF cType != 'L'
        ::__add_to_log(Config():get_config('IncorrectDataType'))
        RETURN .F.
    ENDIF
#endif

#ifdef VALIDATE_DIMENSIONS
    IF AScan(::__axUsedKeys, axRow[L_ID_VAR_PSB]) != 0
        ::__add_to_log(Config():get_config('VariableRepeating'))
        RETURN .F.
    ENDIF
#endif
    AAdd(::__axUsedKeys, axRow[L_ID_VAR_PSB])

    axRow[L_ID_VAR_PSB] := AllTrim(Right(axRow[L_ID_VAR_PSB], Len(axRow[L_ID_VAR_PSB]) - 2))

    nIndex := AScan(::__axKeys, axRow[L_ID_VAR_PSB])

#ifdef VALIDATE_DIMENSIONS
    IF nIndex == 0
        ::__add_to_log(Config():get_config('UnknownVariable'))
        RETURN .F.
    ELSE
#endif
        ::__axValues[nIndex] := cast(::__axValues[nIndex], cType)
#ifdef VALIDATE_DIMENSIONS
    ENDIF
#endif

    FOR i := C_CAPTION_PSB TO C_COLOR_PSB

        IF !::__basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

#ifdef VALIDATE_DIMENSIONS
        IF cType != 'C'
            ::__add_to_log(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF

        IF i == C_COLOR_PSB
            IF !is_color(axRow[i])
                ::__add_to_log(Config():get_config('IncorrectValue'))
                RETURN .F.
            ELSEIF Empty(hb_ColorIndex(axRow[i], 3)) .OR. !Empty(hb_ColorIndex(axRow[i], 5))
                ::__add_to_log(Config():get_config('IncorrectValue'))
                RETURN .F.
            ENDIF
        ENDIF
#endif
    NEXT

    FOR i := C_FOCUS_FNC_PSB TO C_STYLE_PSB

        IF !::__basic_parse(@axRow[i], @cType, hVariables)
            RETURN .F.
        ENDIF

#ifdef VALIDATE_DIMENSIONS
        IF cType != 'C'
            ::__add_to_log(Config():get_config('IncorrectDataType'))
            RETURN .F.
        ENDIF

        IF i == C_STYLE_PSB .AND. !is_pushbutton_style(axRow[i])
            ::__add_to_log(Config():get_config('IncorrectValue'))
            RETURN .F.
        ENDIF
#endif
    NEXT

RETURN .T.

METHOD __make_pushbutton(nRow, nCol, xIdVar, cCaption, cMessage, cWhen, cValid;
                         , cColor, cFocus, cState, cStyle) CLASS Parser

    MEMVAR GETLIST

    LOCAL nPosition := AScan(::__axKeys, xIdVar)

    IF nPosition == 0
        RETURN .F.
    ENDIF

    SetPos(nRow, nCol)

    IF ::__lUseMemVar
        AAdd(GETLIST, _GET_(::__axValues[nPosition], "::__axValues[nPosition]", NIL, &(cValid), &(cWhen)))
        ATail(GETLIST):control := _PushButt_(cCaption, cMessage, cColor, {|| &(cFocus)}, {|| &(cState)}, cStyle)
        ATail(GETLIST):reader := {| a, b, c, d | GUIReader(a, b, c, d)}
        ATail(GETLIST):control:display()
    ELSE
        AAdd(::__aoGetList, _GET_(::__axValues[nPosition], "::__axValues[nPosition]", NIL, &(cValid), &(cWhen)))
        ATail(::__aoGetList):control := _PushButt_(cCaption, cMessage, cColor, {|| &(cFocus)}, {|| &(cState)}, cStyle)
        ATail(::__aoGetList):reader := {| a, b, c, d | GUIReader(a, b, c, d)}
        ATail(::__aoGetList):control:display()
    ENDIF

RETURN .T.

METHOD __make_radiogroup(nTop, nLeft, nBottom, nRight, xIdVar, acGroup, cCaption;
                         , cMessage, cColor, cFocus, cWhen, cValid) CLASS Parser

    MEMVAR GETLIST

    LOCAL nPosition := AScan(::__axKeys, xIdVar)

    IF nPosition == 0
        RETURN .F.
    ENDIF

    SetPos(nTop, nLeft)

    IF ::__lUseMemVar
        AAdd(GETLIST, _GET_(::__axValues[nPosition], "::__axValues[nPosition]", NIL, &(cValid), &(cWhen)))
        ATail(GETLIST):control := _RadioGrp_(ATail(GETLIST):row, ATail(GETLIST):col, nBottom, nRight;
                                             , ::__axValues[nPosition], ::__make_buttons(acGroup), cCaption;
                                             , cMessage, cColor, {|| &(cFocus)};
                                            )
        ATail(GETLIST):reader := {| a, b, c, d | GUIReader(a, b, c, d)}
        ATail(GETLIST):control:display()
    ELSE
        AAdd(::__aoGetList, _GET_(::__axValues[nPosition], "::__axValues[nPosition]", NIL, &(cValid), &(cWhen)))
        ATail(::__aoGetList):control := _RadioGrp_(ATail(::__aoGetList):row, ATail(::__aoGetList):col, nBottom;
                                                 , nRight, ::__axValues[nPosition], ::__make_buttons(acGroup);
                                                 , cCaption, cMessage, cColor, {|| &(cFocus)};
                                                )
        ATail(::__aoGetList):reader := {| a, b, c, d | GUIReader(a, b, c, d)}
        ATail(::__aoGetList):control:display()
    ENDIF

RETURN .T.

METHOD __make_checkbox(nRow, nCol, xIdVar, cCaption, cMessage, cWhen, cValid;
                       , cColor, cFocus, cState, cStyle) CLASS Parser

    MEMVAR GETLIST

    LOCAL nPosition := AScan(::__axKeys, xIdVar)

    IF nPosition == 0
        RETURN .F.
    ENDIF

    SetPos(nRow, nCol)

    IF ::__lUseMemVar
        AAdd(GETLIST, _GET_(::__axValues[nPosition], '::__axValues[nPosition]', NIL, &(cValid), &(cWhen)))
        ATail(GETLIST):control := _CheckBox_(::__axValues[nPosition], cCaption, cMessage, cColor, {|| &(cFocus)}, {|| &(cState)}, cStyle)
        ATail(GETLIST):reader := {|a, b, c, d| GUIReader(a, b, c, d)}
        ATail(GETLIST):control:display()
    ELSE
        AAdd(::__aoGetList, _GET_(::__axValues[nPosition], '::__axValues[nPosition]', NIL, &(cValid), &(cWhen)))
        ATail(::__aoGetList):control := _CheckBox_(::__axValues[nPosition], cCaption, cMessage, cColor, {|| &(cFocus)}, {|| &(cState)}, cStyle)
        ATail(::__aoGetList):reader := {|a, b, c, d| GUIReader(a, b, c, d)}
        ATail(::__aoGetList):control:display()
    ENDIF

RETURN .T.

METHOD __make_listbox(nTop, nLeft, nBottom, nRight, xIdVar, axList, cCaption, cMessage;
                      , cWhen, cValid, cColor, cFocus, cState, lDropDown, lScrollBar;
                     ) CLASS Parser

    MEMVAR GETLIST

    LOCAL nPosition := AScan(::__axKeys, xIdVar)

    IF nPosition == 0
        RETURN .F.
    ENDIF

    SetPos(nTop, nLeft)

    IF ::__lUseMemVar
        AAdd(GETLIST, _GET_(::__axValues[nPosition], "::__axValues[nPosition]", NIL, &(cValid), &(cWhen)))
        ATail(GETLIST):control := _ListBox_(ATail(GETLIST):row, ATail(GETLIST):col, nBottom, nRight;
                                            , ::__axValues[nPosition], axList, cCaption, cMessage, cColor;
                                            , {|| &(cFocus)}, {|| &(cState)}, lDropDown, lScrollBar;
                                           )
        ATail(GETLIST):reader := {| a, b, c, d | GUIReader(a, b, c, d)}
        ATail(GETLIST):control:display()
    ELSE
        AAdd(::__aoGetList, _GET_(::__axValues[nPosition], "::__axValues[nPosition]", NIL, &(cValid), &(cWhen)))
        ATail(::__aoGetList):control := _ListBox_(ATail(::__aoGetList):row, ATail(::__aoGetList):col, nBottom;
                                                , nRight, ::__axValues[nPosition], axList, cCaption, cMessage;
                                                , cColor, {|| &(cFocus)}, {|| &(cState)}, lDropDown, lScrollBar;
                                               )
        ATail(::__aoGetList):reader := {| a, b, c, d | GUIReader(a, b, c, d)}
        ATail(::__aoGetList):control:display()
    ENDIF


RETURN .T.

METHOD __make_get(nRow, nCol, cExp, cSayPicture, cColorStringSay, xIdVar, cGetPicture;
                  , cColorStringGet, cCaption, cMessage, cWhen, cValid) CLASS Parser

    MEMVAR GETLIST
    
    LOCAL nPosition := AScan(::__axKeys, xIdVar)
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

    IF ::__lUseMemVar
        AAdd(GETLIST, _GET_(::__axValues[nPosition], "::__axValues[nPosition]", cGetPicture, &(cValid), &(cWhen)))
        ATail(GETLIST):caption := cCaption
        ATail(GETLIST):caprow := ATail(GETLIST):row
        ATail(GETLIST):capcol := ATail(GETLIST):col - __CapLength(cCaption) - 1
        ATail(GETLIST):message := cMessage
        ATail(GETLIST):colordisp(cColorStringGet)
        ATail(GETLIST):display()
    ELSE
        AAdd(::__aoGetList, _GET_(::__axValues[nPosition], "::__axValues[nPosition]", cGetPicture, &(cValid), &(cWhen)))
        ATail(::__aoGetList):caption := cCaption
        ATail(::__aoGetList):caprow := ATail(::__aoGetList):row
        ATail(::__aoGetList):capcol := ATail(::__aoGetList):col - __CapLength(cCaption) - 1
        ATail(::__aoGetList):message := cMessage
        ATail(::__aoGetList):colordisp(cColorStringGet)
        ATail(::__aoGetList):display()
    ENDIF

RETURN .T.

METHOD __make_box(nTop, nLeft, nBottom, nRight, cBoxString, cColorString) CLASS Parser

    DispBox(nTop, nLeft, nBottom, nRight, cBoxString, cColorString)

RETURN .T.

METHOD __make_window(nTop, nLeft, nBottom, nRight, cBox, cColor, xShadow) CLASS Parser

    LOCAL cOldColor := SetColor(cColor)
    LOCAL nOldShadow := WSetShadow(xShadow)
    LOCAL nOldWindow := WSelect()
    LOCAL lClear := !Empty(cBox) .AND. !Empty(cColor)

    ::__nWindow := WOpen(nTop, nLeft, nBottom, nRight, lClear)
    WBox(cBox)

    WSelect(nOldWindow)
    WSetShadow(NToColor(nOldShadow))
    SET COLOR TO (cOldColor)
    WSelect(::__nWindow)

RETURN ::__nWindow != -1

METHOD __make_say(nRow, nCol, cExp, cSayPicture, cColorString) CLASS Parser

    DevPos(nRow, nCol)
    DevOutPict(cExp, cSayPicture, cColorString)

RETURN .T.
