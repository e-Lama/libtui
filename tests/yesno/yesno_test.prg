#include "yesno.ch"

REQUEST HB_CODEPAGE_UTF8EX

PROCEDURE main()

    LOCAL oError

    hb_cdpSelect('UTF8EX') 

    BEGIN SEQUENCE

        ErrorBlock({| oError | standard_error_handler(oError)})

        IF Config():init_config()
            Alert(Str(Alert('', {'a','a','a'})))
            AlertLG():create_centered(.F.)
            Alert(Transform(YesNo({''}), 'L'))
            Alert(Str(Dialog('Question?', {':)', ':(', ';/'})))
            Alert(Str(Dialog('Long text long text long text long text long text long text ' + hb_OsNewLine() + 'End', {'Ok', 'Nope', 'Long option...'})))
            Alert(Str(Inform('Inform')))
            Alert(Str(Dialog('Żółć?', {'Ż', 'Żó', 'Żół', 'ół', 'łó'})))

            AlertLG():create_centered(.T.)
            Alert(Transform(NoYes({'center', 'center', 'center'}), 'L'))
        ELSE
            Alert(':(')
        ENDIF
    RECOVER USING oError
        standard_error_handler(oError)
    END SEQUENCE
RETURN
