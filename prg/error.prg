#include "error.ch"

#include "functions.ch"

FUNCTION standard_error_handler(oError)

    LOCAL cMessage := IF(ValType(oError:cargo) == 'C' .AND. 'EXCEPTION' $ oError:cargo, 'Exception: ' + oError:description, get_message(oError))
    LOCAL cOsError := IF(Empty(oError:osCode), '', ';' + hb_StrFormat('(DOS Error %1$d)', oError:osCode))
    LOCAL n := 0
    LOCAL xHandler
    LOCAL acStack
    LOCAL cCall
    LOCAL xKey

    xHandler := handle_harbour_errors(oError)

    IF xHandler != NIL
        RETURN xHandler
    ENDIF

    WSetShadow(-1)
    Alert(cMessage + cOsError)

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
STATIC FUNCTION get_message(oError)

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
STATIC FUNCTION handle_harbour_errors(oError)

    LOCAL n := 0

    IF oError:description == 'Zero divisor'
        DO WHILE !Empty(ProcName(++n))
            IF ProcName(n) == 'LISTBOX:SCROLLBARPOS'
                RETURN 0
            ENDIF
        ENDDO
    ENDIF

RETURN NIL
