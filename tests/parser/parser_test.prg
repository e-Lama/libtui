PROCEDURE main()

    MEMVAR GETLIST

    LOCAL hHash := {'Variable' => 'var1', 'Variable1' => .F., 'Variable2' => 'var2', 'Variable3' => 'Var3'} 
    LOCAL aoMyGetList := Array(0)

    BEGIN SEQUENCE

        ErrorBlock({| oError | standard_error_handler(oError)})

        IF Config():init_config({=>})
            Parser():getlist(aoMyGetList)
            Parser():prepare_form_from_database('LANGUAGE', 'ID', hHash)
            ReadModal(aoMyGetList)
            WClose()
            hHash := Parser():get_answers()
            ? hHash['Variable']
            ? hHash['Variable2']
            ? hHash['Variable3']
            Inkey(0)
        ELSE
            ? 'Sth is wrong!'
        ENDIF

    END SEQUENCE

RETURN

