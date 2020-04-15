#include "dbstruct.ch"

#include "functions.ch"

#include "setup.ch"

FUNCTION append(xValue, xTimeout)

    LOCAL nStarted := hb_MilliSeconds()
    LOCAL lNetErr := .T.

#ifdef USE_VALIDATORS
    IF PCount() > 2
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ENDIF
#endif

    IF ValType(xValue) $ 'L,U'
        hb_Default(@xValue, .F.)
#ifdef USE_VALIDATORS
    ELSE
        throw(ARGUMENT_TYPE_EXCEPTION)
#endif
    ENDIF

#ifdef USE_VALIDATORS
    IF ValType(xTimeout) == 'N'
        IF xTimeout <= 0
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
    ELSEIF ValType(xTimeout) != 'U'
        throw(ARGUMENT_TYPE_EXCEPTION)
    ELSE
        hb_Default(@xTimeout, Config():get_config('AppendTimeout'))
    ENDIF
#else
    hb_Default(@xTimeout, Config():get_config('AppendTimeout'))
#endif

    DO WHILE lNetErr .AND. hb_MilliSeconds() - nStarted < xTimeout
        dbAppend(xValue)
        lNetErr := NetErr()

        IF lNetErr
            hb_idleSleep((hb_Random() + 0.1) / 4)
        ENDIF
    ENDDO

RETURN !lNetErr

FUNCTION row_to_hash(acOmmit, hAdd, nRecNo, cAlias)

    LOCAL nOldSelect := Select()
    LOCAL nOldRecNo := RecNo()
    LOCAL hHash := hb_Hash()
    LOCAL axStructure
    LOCAL cKey
    LOCAL axRow

#ifdef USE_VALIDATORS
    IF Alias(Select(nOldSelect)) == ''
        throw(RUNTIME_EXCEPTION)
    ELSEIF PCount() > 4
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ELSEIF !(ValType(acOmmit) $ 'A;U')
        throw(ARGUMENT_TYPE_EXCEPTION)
    ELSEIF !(ValType(hAdd) $ 'H;U')
        throw(ARGUMENT_TYPE_EXCEPTION)
    ELSEIF !(ValType(cAlias) $ 'C;U')
        throw(ARGUMENT_TYPE_EXCEPTION)
    ELSEIF !(ValType(nRecNo) $ 'N;U')
        throw(ARGUMENT_TYPE_EXCEPTION)
    ELSEIF ValType(acOmmit) == 'A'
        AEval(acOmmit, {| cElement | assert_type(cElement, 'C')})
    ELSEIF ValType(hAdd) == 'H'
        FOR EACH cKey IN hb_hKeys(hAdd)
            assert_type(cKey, 'C')
            IF !(ValType(hAdd[cKey]) $ 'C;N;D;L')
                throw(ARGUMENT_TYPE_EXCEPTION)
            ENDIF
        NEXT
    ENDIF
#endif

    IF ValType(cAlias) == 'C'
        SELECT (cAlias)
    ENDIF

    IF ValType(nRecNo) == 'N'
        GO nRecNo
    ENDIF

    axStructure := dbStruct()

    IF ValType(acOmmit) == 'A'
        FOR EACH axRow IN axStructure
            IF AScan(acOmmit, axRow[DBS_NAME]) == 0
                hHash[axRow[DBS_NAME]] := field->&(axRow[DBS_NAME])
            ENDIF
        NEXT
    ELSE
        FOR EACH axRow IN axStructure
            hHash[axRow[DBS_NAME]] := field->&(axRow[DBS_NAME])
        NEXT
    ENDIF

    IF ValType(hAdd) == 'H'
        FOR EACH cKey IN hb_hKeys(hAdd)
#ifdef USE_VALIDATORS
            IF hb_hHasKey(hHash, cKey)
                throw(RUNTIME_EXCEPTION)
            ENDIF
#endif

            hHash[cKey] := hAdd[cKey]
        NEXT
    ENDIF

    SELECT (nOldSelect)
    GO nOldRecNo

RETURN hHash

FUNCTION hash_to_row(hHash, lAppendBlank)

    LOCAL axStructure := dbStruct()
    LOCAL axField

#ifdef USE_VALIDATORS
    IF PCount() != 1 .AND. PCount() != 2
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ELSEIF ValType(hHash) != 'H'
        throw(ARGUMENT_TYPE_EXCEPTION)
    ELSEIF ValType(lAppendBlank) != 'L'
        throw(ARGUMENT_TYPE_EXCEPTION)
    ENDIF
#endif

    IF lAppendBlank
        IF !append(.F.)
            RETURN .F.
        ENDIF
    ELSE
        IF !dbRLock(RecNo())
            RETURN .F.
        ENDIF
    ENDIF

    FOR EACH axField IN axStructure
        IF hb_hHasKey(hHash, axField[DBS_NAME])
            field->&(axField[DBS_NAME]) := hHash[axField[DBS_NAME]]
        ENDIF
    NEXT

RETURN .T.
