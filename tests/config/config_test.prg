PROCEDURE main()

    LOCAL hConfig := hb_Hash(;
                            'Title', 'Test';
                            )

    IF Config():init_config(hConfig)
        Inform('O.K.')
    ELSE
        ? 'Sth is wrong!'
    ENDIF

RETURN

