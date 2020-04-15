#include "hbclass.ch"
#include "box.ch"

#include "functions.ch"

#include "setup.ch"

CREATE CLASS Window

EXPORTED:
    
    METHOD refresh_window()

    METHOD border(cBorder) SETGET
    METHOD title(cTitle) SETGET
    METHOD header(cHeader) SETGET
    METHOD footer(cFooter) SETGET

    METHOD border_color(cColor) SETGET
    METHOD title_color(cColor) SETGET
    METHOD header_color(cColor) SETGET
    METHOD footer_color(cColor) SETGET

    METHOD refresh_border()
    METHOD refresh_title(lRefreshBorder)
    METHOD refresh_header()
    METHOD refresh_footer()
    METHOD refresh_header_footer() INLINE DispBegin(), ::refresh_header(), ::refresh_footer(), DispEnd()

    METHOD get_top() INLINE IF(Empty(::__cBorder), ::__nTop, ::__nTop + 1)
    METHOD get_bottom() INLINE IF(Empty(::__cBorder), ::__nBottom, ::__nBottom - 1)
    METHOD get_left() INLINE IF(Empty(::__cBorder), ::__nLeft, ::__nLeft + 1)
    METHOD get_right() INLINE IF(Empty(::__cBorder), ::__nRight, ::__nRight - 1)

    METHOD center_row() INLINE (::get_top() + ::get_bottom()) / 2
    METHOD center_col() INLINE (::get_left() + ::get_right()) / 2

    METHOD apply_config()

    METHOD clear_screen(cColor)

HIDDEN:

    CLASSVAR __nTop AS NUMERIC INIT 0
    CLASSVAR __nLeft AS NUMERIC INIT 0
    CLASSVAR __nBottom AS NUMERIC INIT MaxRow()
    CLASSVAR __nRight AS NUMERIC INIT MaxCol()
    CLASSVAR __cColorUnspec AS CHARACTER INIT SetColor()
    CLASSVAR __bRowPrint AS CODEBLOCK INIT {|| QQout('blokRowPrint')}
    CLASSVAR __bAction AS CODEBLOCK INIT {|| QQout('action')}
    CLASSVAR __cTitle AS CHARACTER INIT ''

    CLASSVAR __cHeader AS CHARACTER INIT ''
    CLASSVAR __cFooter AS CHARACTER INIT ''
    CLASSVAR __cHeaderColor AS CHARACTER INIT SetColor()
    CLASSVAR __cFooterColor AS CHARACTER INIT SetColor()

    CLASSVAR __cBorder AS CHARACTER INIT B_DOUBLE
    CLASSVAR __cBorderColor AS CHARACTER INIT SetColor()

    CLASSVAR __cTitleColor AS CHARACTER INIT SetColor()

ENDCLASS LOCKED

METHOD clear_screen(cColor) CLASS Window

    LOCAL cOldColor := SetColor()

    IF cColor != NIL
#ifdef USE_VALIDATORS
        assert_type(cColor, 'C')
        IF !is_color(cColor)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif
        SET COLOR TO (cColor)
    ENDIF

    @ ::get_top(), ::get_left() CLEAR TO ::get_bottom(), ::get_right()

    SET COLOR TO (cOldColor)

RETURN NIL

METHOD refresh_window() CLASS Window

    DispBegin()
    IF !Empty(::__cBorder)
        ::refresh_border()
        IF !Empty(::__cTitle)
            ::refresh_title(.F.)
        ENDIF
    ENDIF
    ::refresh_footer()
    ::refresh_header()
    DispEnd()

RETURN NIL

METHOD refresh_border() CLASS Window

    LOCAL nOldWindow := WSelect()
    LOCAL cOldColor

    WSelect(0)
    cOldColor := SetColor(::__cBorderColor)
    @ ::__nTop, ::__nLeft, ::__nBottom, ::__nRight BOX ::__cBorder COLOR ::__cBorderColor

    SET COLOR TO (cOldColor)
    WSelect(nOldWindow)

RETURN NIL

METHOD refresh_title(lRefreshBorder) CLASS Window

    LOCAL nOldWindow := WSelect()
    LOCAL nCol := Max(Int((::__nLeft + ::__nRight - Len(::__cTitle)) / 2), ::__nLeft)
    LOCAL cOldColor

#ifdef USE_VALIDATORS
    IF ValType(lRefreshBorder) != 'U' .AND. ValType(lRefreshBorder) != 'L'
        throw(ARGUMENT_TYPE_EXCEPTION)
    ENDIF
#endif
    
    IF (ValType(lRefreshBorder) == 'U' .AND. !Empty(::__cBorder)) .OR. lRefreshBorder
        ::refresh_border()
    ENDIF

    WSelect(0)
    cOldColor := SetColor(::__cTitleColor)
    @ ::__nTop, nCol SAY Left(::__cTitle, ::__nRight - ::__nLeft)

    SET COLOR TO (cOldColor)
    WSelect(nOldWindow)

RETURN NIL

METHOD refresh_footer() CLASS Window

    LOCAL nOldWindow := WSelect()
    LOCAL nCol := Max(Int((::get_left() + ::get_right() - Len(::__cFooter)) / 2), ::get_left()) 
    LOCAL cOldColor

    WSelect(0)
    cOldColor := SetColor(::__cFooterColor)
    @ ::get_bottom(), ::get_left() CLEAR TO ::get_bottom(), ::get_right()
    @ ::get_bottom(), nCol SAY Left(::__cFooter, ::get_right() - ::get_left())

    SET COLOR TO (cOldColor)
    WSelect(nOldWindow)

RETURN NIL

METHOD refresh_header() CLASS Window

    LOCAL nOldWindow := WSelect()
    LOCAL nCol := Max(Int((::get_left() + ::get_right() - Len(::__cHeader)) / 2), ::get_left()) 
    LOCAL cOldColor

    WSelect(0)
    cOldColor := SetColor(::__cHeaderColor)
    @ ::get_top(), ::get_left() CLEAR TO ::get_top(), ::get_right()
    @ ::get_top(), nCol SAY Left(::__cHeader, ::get_right() - ::get_left())

    SET COLOR TO (cOldColor)
    WSelect(nOldWindow)

RETURN NIL

METHOD apply_config() CLASS Window

    ::__nTop := Config():get_config('WindowUpperLeftCornerX')
    ::__nLeft := Config():get_config('WindowUpperLeftCornerY')
    ::__nBottom := Config():get_config('WindowHeight')
    ::__nRight := Config():get_config('WindowWidth')
    ::__cColorUnspec := Config():get_config('DefaultColor')
    ::__cBorder := Config():get_config('WindowBorder')
    ::__cHeaderColor := Config():get_config('HeaderColor')
    ::__cFooterColor := Config():get_config('FooterColor')
    ::__cBorderColor := Config():get_config('BorderColor')
    ::__cTitleColor := Config():get_config('TitleColor')
    ::__cTitle := Config():get_config('Title')
 
RETURN NIL

METHOD border(cBorder) CLASS Window

    LOCAL cWasBorder := ::__cBorder

    IF cBorder != NIL
#ifdef USE_VALIDATORS
        assert_type(cBorder, 'C')
        IF !is_box(hb_Translate(cBorder, 'EN', hb_cdpSelect()))
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif
        ::__cBorder := cBorder
    ENDIF

RETURN cWasBorder

METHOD title(cTitle) CLASS Window

    LOCAL cWasTitle := ::__cTitle

    IF cTitle != NIL
#ifdef USE_VALIDATORS
        assert_type(cTitle, 'C')
#endif
        ::__cTitle := cTitle
    ENDIF

RETURN cWasTitle

METHOD header(cHeader) CLASS Window

    LOCAL cWasHeader := ::__cHeader

    IF cHeader != NIL
#ifdef USE_VALIDATORS
        assert_type(cHeader, 'C')
#endif
        ::__cHeader := cHeader
    ENDIF

RETURN cWasHeader

METHOD footer(cFooter) CLASS Window

    LOCAL cWasFooter := ::__cFooter

    IF cFooter != NIL
#ifdef USE_VALIDATORS
        assert_type(cFooter, 'C')
#endif
        ::__cFooter := cFooter
    ENDIF

RETURN cWasFooter

METHOD border_color(cColor) CLASS Window

    LOCAL cWasColor := ::__cBorderColor

    IF cColor != NIL
#ifdef USE_VALIDATORS
        assert_type(cColor, 'C')
        IF !is_color(cColor)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif
        ::__cBorderColor := cColor
    ENDIF

RETURN cWasColor

METHOD title_color(cColor) CLASS Window

    LOCAL cWasColor := ::__cTitleColor

    IF cColor != NIL
#ifdef USE_VALIDATORS
        assert_type(cColor, 'C')
        IF !is_color(cColor)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif
        ::__cTitleColor := cColor
    ENDIF

RETURN cWasColor

METHOD header_color(cColor) CLASS Window

    LOCAL cWasColor := ::__cHeaderColor

    IF cColor != NIL
#ifdef USE_VALIDATOR
        assert_type(cColor, 'C')
        IF !is_color(cColor)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif
        ::__cHeaderColor := cColor
    ENDIF

RETURN cWasColor

METHOD footer_color(cColor) CLASS Window

    LOCAL cWasColor := ::__cFooterColor

    IF cColor != NIL
#ifdef USE_VALIDATORS
        assert_type(cColor, 'C')
        IF !is_color(cColor)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
#endif
        ::__cFooterColor := cColor
    ENDIF

RETURN cWasColor
