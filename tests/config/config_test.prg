PROCEDURE main()

    LOCAL hConfig := hb_Hash(;
                            'Title', 'Test';
                            )
    LOCAL oError 

    BEGIN SEQUENCE

        ErrorBlock({| oError | standard_error_handler(oError)})

        IF Config():init_config(hConfig)
            Inform('O.K.')
        ELSE
            ? 'Sth is wrong!'
        ENDIF
    RECOVER USING oError
        standard_error_handler(oError)
    END SEQUENCE

RETURN

