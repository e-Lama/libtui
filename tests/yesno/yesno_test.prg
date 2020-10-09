#include "inkey.ch"

#include "yesno.ch"

REQUEST HB_CODEPAGE_UTF8EX

PROCEDURE main()

    LOCAL oError
    LOCAL cLongText := 'Wikipedia is a multilingual online encyclopedia created and maintained as an open collaboration project[4] by a community of volunteer editors using a wiki-based editing system.[5] It is the largest and most popular general reference work on the World Wide Web.[6][7][8] It is also one of the 15 most popular websites ranked by Alexa, as of June 2020.[9] It features exclusively free content and no commercial ads and is owned and supported by the Wikimedia Foundation, a non-profit organization funded primarily through donations.'
    LOCAL lAnwser
    LOCAL nAnwser

    Set(_SET_EVENTMASK, INKEY_ALL)

    hb_cdpSelect('UTF8EX') 

    BEGIN SEQUENCE

        ErrorBlock({| oError | standard_error_handler(oError)})

        IF Config():init_config()
            AlertLG():create_centered(.F.)

            ? YesNo({''})
            ? Dialog('Question?', {':)', ':(', ';/'})
            ? YesNo(cLongText)
            ? Dialog('Long text long text long text long text long text long text ' + hb_OsNewLine() + 'End', {'Ok', 'Nope', 'Long option...'})
            ? Inform('Inform')
            ? Dialog('Long text long text long text long text long text long text long text long text veryd', {'Ok', 'Nope', 'Long option...'})
            ? Dialog('Żółć?', {'Ż', 'Żó', 'Żół', 'ół', 'łó'})

            AlertLG():create_centered(.T.)

            ? NoYes({'center', 'center', 'center'})

            @ YESNO 'Nice?' TO lAnwser ANWSERS {'Yes', 'No'} CYCLIC MOVABLE FAST ACCEPT
            ? lAnwser
            @ INFORM cLongText TO nAnwser
            ? nAnwser
        ELSE
            Alert(':(')
        ENDIF
    RECOVER USING oError
        standard_error_handler(oError)
    END SEQUENCE
RETURN
