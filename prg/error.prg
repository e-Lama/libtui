#include "error.ch"

#include "functions.ch"

#include "setup.ch"

PROCEDURE throw(cDescription)

    LOCAL oError := ErrorNew()
    LOCAL n := 0

    oError:description := cDescription
    oError:severity := ES_ERROR
    oError:cargo := 'EXCEPTION: '

    DO WHILE !Empty(ProcName(++n))
        oError:cargo += hb_StrFormat('Called from %1$s(%2$d)' + hb_OsNewLine(), ProcName(n), ProcLine(n))
    ENDDO

    BREAK oError

#pragma ENABLEWARNINGS = Off //-es2 and -w3 flags makes RETURN impossible without this pragma
RETURN
#pragma ENABLEWARNINGS = On

PROCEDURE assert_type(xValue, xType, cDescription)

    LOCAL nPCount := PCount()

#ifdef USE_VALIDATORS
    IF nPCount < 2 .OR. nPCount > 3
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ENDIF
#endif

    IF ValType(xType) == 'C'
        xType := {xType}
#ifdef USE_VALIDATORS
    ELSEIF ValType(xType) != 'A'
        throw(ARGUMENT_VALUE_EXCEPTION)
#endif
    ENDIF

#ifdef USE_VALIDATORS
    AEval(xType, {| cElement | IF(is_data_type(cElement), , throw(ARGUMENT_VALUE_EXCEPTION))})
#endif

    IF AScan(xType, ValType(xValue)) == 0
        IF nPCount == 3
            assert_type(cDescription, 'C')
            throw(cDescription)
        ELSE
            throw(ARGUMENT_TYPE_EXCEPTION)
        ENDIF
    ENDIF

RETURN

PROCEDURE assert_length(xValue, nLength, cDescription)

    LOCAL nPCount := PCount()

#ifdef USE_VALIDATORS
    IF nPCount < 2 .OR. nPCount > 3
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ELSEIF !(ValType(xValue) $ 'A;H;C')
        throw(ARGUMENT_TYPE_EXCEPTION)
    ELSEIF Len(xValue) != nLength
#else
    IF Len(xValue) != nLength
#endif
        IF nPCount == 3
            assert_type(cDescription, 'C')
            throw(cDescription)
        ELSE
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
    ENDIF

RETURN

PROCEDURE assert_data_type(cType, cDescription)

    LOCAL nPCount := PCount()

#ifdef USE_VALIDATORS
    IF nPCount < 1 .OR. nPCount > 2
        throw(ARGUMENTS_NUMBER_EXCEPTION)
    ELSEIF !is_data_type(cType)
#else
    IF !is_data_type(cType)
#endif
        IF nPCount == 2
            assert_type(cDescription, 'C')
            throw(cDescription)
        ELSE
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
    ENDIF

RETURN

FUNCTION standard_error_handler(oError)

    LOCAL cMessage := IF(ValType(oError:cargo) == 'C' .AND. 'EXCEPTION' $ oError:cargo, 'Exception: ' + oError:description, __get_message(oError))
    LOCAL cOsError := IF(Empty(oError:osCode), '', ';' + hb_StrFormat('(DOS Error %1$d)', oError:osCode))
    LOCAL n := 0
    LOCAL acStack
    LOCAL cCall
    LOCAL xKey

#ifdef HB_CLP_STRICT
    LOCAL xHandler

    xHandler := __handle_harbour_errors(oError)

    IF xHandler != NIL
        RETURN xHandler
    ENDIF
#endif

    WSetShadow(-1)
    Alert(cMessage + cOsError)

    WAClose()

    SET PRINTER TO log.txt
    SET PRINTER ON

    ? DToC(Date()) + Time()
    ? 'ARGS:'
    IF ValType(oError:args) == 'A'
        FOR EACH xKey IN oError:args
            ? xKey
        NEXT
    ELSE
    ? 'NIL'
    ENDIF

    ? 'canDefault: ' + cast(oError:canDefault, 'C')
    ? 'canRetry: ' + cast(oError:canRetry, 'C')
    ? 'canSubstitute: ' + cast(oError:canSubstitute, 'C')
    ? 'description: ' + oError:description
    ? 'filename: ' + oError:filename
    ? 'genCode: ' + LTrim(Str(oError:genCode))
    ? 'operation: ' + oError:operation
    ? 'osCode: ' + LTrim(Str(oError:osCode))
    ? 'severity: ' + LTrim(Str(oError:severity))
    ? 'subcode: ' + LTrim(Str(oError:subcode))
    ? 'subsystem: ' + oError:subsystem
    ? 'tries: ' + LTrim(Str(oError:tries))

    IF ValType(oError:cargo) == 'C'
        IF EXCEPTION $ oError:cargo
            ? 'cargo: EXCEPTION'
        ELSE
            ? 'cargo: ' + oError:cargo
        ENDIF
    ELSEIF ValType(oError:cargo) == 'N'
        ? 'cargo: ' + Str(oError:cargo)
    ELSEIF ValType(oError:cargo) == 'D'
        ? 'cargo: ' + DToC(oError:cargo)
    ELSE
        ? 'cargo type: ' + ValType(oError:cargo)
    ENDIF

    ? 'Error level: ' + LTrim(Str(ErrorLevel()))
    ?
    ? '--------------------------'
    ? 'Alias: ' + Alias()

    IF Alias() != ''
        ? 'Index number: ' + LTrim(Str(IndexOrd()))
        ? 'Index key: ' + IndexKey()
        ? 'RecNo/Records: ' + LTrim(Str(RecNo())) + '/' + LTrim(Str(LastRec()))
    ENDIF

    ?

    IF ValType(oError:cargo) == 'C' .AND. EXCEPTION $ oError:cargo
        acStack := hb_ATokens(Right(oError:cargo, Len(oError:cargo) - Len(EXCEPTION)), Chr(10))
        FOR EACH cCall IN acStack
            ? cCall
        NEXT
    ELSE
        DO WHILE !Empty(ProcName(++n))
            ? hb_StrFormat('Called from %1$s(%2$d)  ', ProcName(n), ProcLine(n))
        ENDDO
    ENDIF

    QUIT

RETURN NIL

//errsys.prg
STATIC FUNCTION __get_message(oError)

   LOCAL cMessage := IF(oError:severity > ES_WARNING, 'Error', 'Warning') + ' '

   IF hb_IsString(oError:subsystem)
      cMessage += oError:subsystem()
   ELSE
      cMessage += '???'
   ENDIF

   IF hb_IsNumeric(oError:subCode)
      cMessage += '/' + hb_NToS(oError:subCode)
   ELSE
      cMessage += '/???'
   ENDIF

   IF hb_IsString(oError:description)
      cMessage += '  ' + oError:description
   ENDIF

   DO CASE
       CASE !Empty(oError:filename)
          cMessage += ': ' + oError:filename
       CASE !Empty(oError:operation)
          cMessage += ': ' + oError:operation
   ENDCASE

RETURN cMessage

//Because of a compatibility some errors are allowed
#ifdef HB_CLP_STRICT
STATIC FUNCTION __handle_harbour_errors(oError)

    LOCAL n := 0

    IF oError:description == 'Zero divisor'
        DO WHILE !Empty(ProcName(++n))
            IF ProcName(n) == 'LISTBOX:SCROLLBARPOS'
                RETURN 0
            ENDIF
        ENDDO
    ENDIF

RETURN NIL
#endif
