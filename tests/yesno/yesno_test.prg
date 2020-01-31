#include "yesno.ch"

REQUEST HB_CODEPAGE_UTF8EX

PROCEDURE main()

    LOCAL lResult

    hb_cdpSelect('UTF8EX') 

    AlertLG():create_centered(.F.)
    YesNo({''})
    Dialog('Question?', {':)', ':(', ';/'})
    Dialog('Long text long text long text long text long text long text ' + hb_OsNewLine() + 'End', {'Ok', 'Nope', 'Long option...'})
    Inform('Inform')
    Dialog('Żółć?', {'Ż', 'Żó', 'Żół', 'ół', 'łó'})

    AlertLG():create_centered(.T.)
    NoYes({'center', 'center', 'center'})

    altd()
    lResult := YESNO {'te', 'st'} ANWSERS {'a', 'b'} COLOR 'N/B' OPTION 2 ACCEPTFIRST

    Alert(ValType(lResult))

RETURN
