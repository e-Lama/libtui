PROCEDURE main()

    MEMVAR GETLIST

    LOCAL oError

    PUBLIC GETLIST

    BEGIN SEQUENCE

        ErrorBlock({| oError | standard_error_handler(oError)})

        SET CURSOR OFF

        IF Config():init_config({'Header' => 'Hello world!', 'Footer' => 'Footer', 'Title' => 'Main window', 'Failure' => ':('})

            Window():header(Config():get_config('Header'))
            Window():footer(Config():get_config('Footer'))
            Window():title(Config():get_config('Title'))
            Window():apply_config()
            Window():refresh_window()

            Inkey(0)
        ELSE
            ? 'Failure'
        ENDIF
    RECOVER USING oError
        standard_error_handler(oError)
    END SEQUENCE

RETURN
