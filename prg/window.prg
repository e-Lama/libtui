#include "hbclass.ch"
#include "box.ch"

#include "functions.ch"

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

    METHOD get_top() INLINE IF(Empty(::cBorder), ::nTop, ::nTop + 1)
    METHOD get_bottom() INLINE IF(Empty(::cBorder), ::nBottom, ::nBottom - 1)
    METHOD get_left() INLINE IF(Empty(::cBorder), ::nLeft, ::nLeft + 1)
    METHOD get_right() INLINE IF(Empty(::cBorder), ::nRight, ::nRight - 1)

    METHOD center_row() INLINE (::get_top() + ::get_bottom()) / 2
    METHOD center_col() INLINE (::get_left() + ::get_right()) / 2

    METHOD apply_config()

    METHOD clear_screen(cColor)

HIDDEN:

    CLASSVAR nTop AS NUMERIC INIT 0
    CLASSVAR nLeft AS NUMERIC INIT 0
    CLASSVAR nBottom AS NUMERIC INIT MaxRow()
    CLASSVAR nRight AS NUMERIC INIT MaxCol()
    CLASSVAR cColorUnspec AS CHARACTER INIT SetColor()
    CLASSVAR bRowPrint AS CODEBLOCK INIT {|| QQout('blokRowPrint')}
    CLASSVAR bAction AS CODEBLOCK INIT {|| QQout('action')}
    CLASSVAR cTitle AS CHARACTER INIT ''

    CLASSVAR cHeader AS CHARACTER INIT ''
    CLASSVAR cFooter AS CHARACTER INIT ''
    CLASSVAR cHeaderColor AS CHARACTER INIT SetColor()
    CLASSVAR cFooterColor AS CHARACTER INIT SetColor()

    CLASSVAR cBorder AS CHARACTER INIT B_DOUBLE
    CLASSVAR cBorderColor AS CHARACTER INIT SetColor()

    CLASSVAR cTitleColor AS CHARACTER INIT SetColor()

ENDCLASS LOCKED

METHOD clear_screen(cColor) CLASS Window

    LOCAL cOldColor := SetColor()

    IF cColor != NIL
        assert_type(cColor, 'C')
        IF !is_color(cColor)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF

        SET COLOR TO (cColor)
    ENDIF

    @ ::get_top(), ::get_left() CLEAR TO ::get_bottom(), ::get_right()

    SET COLOR TO (cOldColor)

RETURN NIL

METHOD refresh_window() CLASS Window

    DispBegin()
    IF !Empty(::cBorder)
        ::refresh_border()
        IF !Empty(::cTitle)
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
    cOldColor := SetColor(::cBorderColor)
    @ ::nTop, ::nLeft, ::nBottom, ::nRight BOX ::cBorder COLOR ::cBorderColor

    SET COLOR TO (cOldColor)
    WSelect(nOldWindow)

RETURN NIL

METHOD refresh_title(lRefreshBorder) CLASS Window

    LOCAL nOldWindow := WSelect()
    LOCAL nCol := Max(Int((::nLeft + ::nRight - Len(::cTitle)) / 2), ::nLeft)
    LOCAL cOldColor

    IF ValType(lRefreshBorder) != 'U' .AND. ValType(lRefreshBorder) != 'L'
        throw(ARGUMENT_TYPE_EXCEPTION)
    ENDIF
    
    IF (ValType(lRefreshBorder) == 'U' .AND. !Empty(::cBorder)) .OR. lRefreshBorder
        ::refresh_border()
    ENDIF

    WSelect(0)
    cOldColor := SetColor(::cTitleColor)
    @ ::nTop, nCol SAY Left(::cTitle, ::nRight - ::nLeft)

    SET COLOR TO (cOldColor)
    WSelect(nOldWindow)

RETURN NIL

METHOD refresh_footer() CLASS Window

    LOCAL nOldWindow := WSelect()
    LOCAL nCol := Max(Int((::get_left() + ::get_right() - Len(::cFooter)) / 2), ::get_left()) 
    LOCAL cOldColor

    WSelect(0)
    cOldColor := SetColor(::cFooterColor)
    @ ::get_bottom(), ::get_left() CLEAR TO ::get_bottom(), ::get_right()
    @ ::get_bottom(), nCol SAY Left(::cFooter, ::get_right() - ::get_left())

    SET COLOR TO (cOldColor)
    WSelect(nOldWindow)

RETURN NIL

METHOD refresh_header() CLASS Window

    LOCAL nOldWindow := WSelect()
    LOCAL nCol := Max(Int((::get_left() + ::get_right() - Len(::cHeader)) / 2), ::get_left()) 
    LOCAL cOldColor

    WSelect(0)
    cOldColor := SetColor(::cHeaderColor)
    @ ::get_top(), ::get_left() CLEAR TO ::get_top(), ::get_right()
    @ ::get_top(), nCol SAY Left(::cHeader, ::get_right() - ::get_left())

    SET COLOR TO (cOldColor)
    WSelect(nOldWindow)

RETURN NIL

METHOD apply_config() CLASS Window

    ::nTop := Config():get_config('WindowUpperLeftCornerX')
    ::nLeft := Config():get_config('WindowUpperLeftCornerY')
    ::nBottom := Config():get_config('WindowHeight')
    ::nRight := Config():get_config('WindowWidth')
    ::cColorUnspec := Config():get_config('DefaultColor')
    ::cBorder := Config():get_config('WindowBorder')
    ::cHeaderColor := Config():get_config('HeaderColor')
    ::cFooterColor := Config():get_config('FooterColor')
    ::cBorderColor := Config():get_config('BorderColor')
    ::cTitleColor := Config():get_config('TitleColor')
    ::cTitle := Config():get_config('Title')
 
RETURN NIL

METHOD border(cBorder) CLASS Window

    LOCAL cWasBorder := ::cBorder

    IF cBorder != NIL
        assert_type(cBorder, 'C')
        IF !is_box(cBorder)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
        ::cBorder := cBorder
    ENDIF

RETURN cWasBorder

METHOD title(cTitle) CLASS Window

    LOCAL cWasTitle := ::cTitle

    IF cTitle != NIL
        assert_type(cTitle, 'C')
        ::cTitle := cTitle
    ENDIF

RETURN cWasTitle

METHOD header(cHeader) CLASS Window

    LOCAL cWasHeader := ::cHeader

    IF cHeader != NIL
        assert_type(cHeader, 'C')
        ::cHeader := cHeader
    ENDIF

RETURN cWasHeader

METHOD footer(cFooter) CLASS Window

    LOCAL cWasFooter := ::cFooter

    IF cFooter != NIL
        assert_type(cFooter, 'C')
        ::cFooter := cFooter
    ENDIF

RETURN cWasFooter

METHOD border_color(cColor) CLASS Window

    LOCAL cWasColor := ::cBorderColor

    IF cColor != NIL
        assert_type(cColor, 'C')
        IF !is_color(cColor)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
        ::cBorderColor := cColor
    ENDIF

RETURN cWasColor

METHOD title_color(cColor) CLASS Window

    LOCAL cWasColor := ::cTitleColor

    IF cColor != NIL
        assert_type(cColor, 'C')
        IF !is_color(cColor)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
        ::cTitleColor := cColor
    ENDIF

RETURN cWasColor

METHOD header_color(cColor) CLASS Window

    LOCAL cWasColor := ::cHeaderColor

    IF cColor != NIL
        assert_type(cColor, 'C')
        IF !is_color(cColor)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
        ::cHeaderColor := cColor
    ENDIF

RETURN cWasColor

METHOD footer_color(cColor) CLASS Window

    LOCAL cWasColor := ::cFooterColor

    IF cColor != NIL
        assert_type(cColor, 'C')
        IF !is_color(cColor)
            throw(ARGUMENT_VALUE_EXCEPTION)
        ENDIF
        ::cFooterColor := cColor
    ENDIF

RETURN cWasColor
