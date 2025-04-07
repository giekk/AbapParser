REPORT z_gt_test_5.

TYPE-POOLS: slis.

TABLES: zgt_usr.

TYPES: BEGIN OF ty_user_table,
    id TYPE c LENGTH 4,
    uname TYPE string,
    fname TYPE string,
    lname TYPE string,
    bdate TYPE dats,
    role TYPE c LENGTH 5,
END OF ty_user_table.

*----------------------------------------------------------------------*
*       CLASS exception DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS exception DEFINITION.
  PUBLIC SECTION.
    DATA: lx_error TYPE REF TO cx_sy_dyn_call_illegal_type,
          lv_error TYPE string,
          px_error TYPE REF TO cx_sy_dyn_call_param_not_found,
          pv_error TYPE string.

    METHODS: exc_illegal_type,
             exc_param_not_found.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.                    "exception DEFINITION

*----------------------------------------------------------------------*
*       CLASS exception IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS exception IMPLEMENTATION.
  METHOD exc_illegal_type.
    lv_error = lx_error->get_text( ).
    MESSAGE lv_error TYPE 'E'.
    CLEAR lv_error.
  ENDMETHOD.                    "exc_illegal_type
  METHOD exc_param_not_found.
    pv_error = px_error->get_text( ).
    MESSAGE pv_error TYPE 'E'.
    CLEAR pv_error.
  ENDMETHOD.                    "exc_param_not_found
ENDCLASS.                    "exception IMPLEMENTATION

" Variabili
DATA: new_record        TYPE zgt_usr,
      it_user_table     TYPE TABLE OF ty_user_table,
      ls_user_table     LIKE LINE OF it_user_table.

DATA: t_fieldcat    TYPE slis_t_fieldcat_alv,
      wa_fieldcat   TYPE slis_fieldcat_alv,
      lt_header     TYPE slis_t_listheader,
      ls_header     TYPE slis_listheader,
      lv_lines      TYPE i,
      lt_line       LIKE ls_header-info,
      lv_linesc(10) TYPE c.

DATA: g_repid TYPE sy-repid.

DATA: it_excel_data TYPE TABLE OF alsmex_tabline,
      ls_excel_row  LIKE LINE OF it_excel_data.

DATA: lanswer, err TYPE bapi_msg.

DATA: exc TYPE REF TO exception.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE title_b1.
SELECTION-SCREEN SKIP 1.
PARAMETERS: p_id(4) TYPE c,
            p_uname TYPE string VISIBLE LENGTH 15,
            p_fname TYPE string VISIBLE LENGTH 15,
            p_lname TYPE string VISIBLE LENGTH 15,
            p_bdate TYPE dats,
            p_role TYPE c LENGTH 5 AS LISTBOX VISIBLE LENGTH 10 DEFAULT ''.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE title_b2.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: r_src RADIOBUTTON GROUP rad1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 5(20) label_b1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: r_add RADIOBUTTON GROUP rad1.
SELECTION-SCREEN COMMENT 5(20) label_b2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: r_del RADIOBUTTON GROUP rad1.
SELECTION-SCREEN COMMENT 5(20) label_b3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: r_mod RADIOBUTTON GROUP rad1.
SELECTION-SCREEN COMMENT 5(20) label_b4.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN PUSHBUTTON 1(5) p_import USER-COMMAND import_button.
SELECTION-SCREEN COMMENT 7(30) label_b5.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
  title_b1 = 'Parametri di selezione'.
  title_b2 = 'Comandi'.
  label_b1 = 'Cerca'.
  label_b2 = 'Aggiungi'.
  label_b3 = 'Elimina'.
  label_b4 = 'Modifica'.
  label_b5 = 'Importa Excel'.

  MOVE: '@EX@' TO p_import.
  PERFORM populate_roles.

* Gestione della richiesta di valore per il campo a discesa
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_role.
  PERFORM populate_roles.

* --------------------------------------------------------------- *

START-OF-SELECTION.

AT SELECTION-SCREEN.

  CREATE OBJECT exc.

  CASE sy-ucomm.
    WHEN 'IMPORT_BUTTON'.

      IF r_add = 'X'.

        PERFORM import_excel_file.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question  = 'Importare i seguenti utenti?'
            text_button_1  = 'Si'
            icon_button_1  = 'ICON_OKAY'
            text_button_2  = 'No'
            icon_button_2  = 'ICON_CANCEL'
          IMPORTING
            answer         = lanswer
          EXCEPTIONS
            text_not_found = 1
            OTHERS         = 2.

        IF lanswer EQ '1'.

          " Inserisco record importati in tabella
          PERFORM add_records_table CHANGING new_record.

          " Creazione del titolo
          PERFORM top_of_page_1.

          " Creazione del field catalog
          PERFORM create_fieldcatalog.

          g_repid = sy-repid.

          TRY.
              CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
                EXPORTING
                  it_fieldcat            = t_fieldcat[]
                  i_callback_program     = g_repid
                  i_callback_top_of_page = 'TOP_OF_PAGE_1'
                TABLES
                  t_outtab               = it_user_table[]
                EXCEPTIONS
                  program_error          = 1
                  OTHERS                 = 2.
            CATCH cx_sy_dyn_call_param_not_found INTO exc->px_error.
              exc->exc_param_not_found( ).
            CATCH cx_sy_dyn_call_illegal_type INTO exc->lx_error.
              exc->exc_illegal_type( ).
          ENDTRY.

          RETURN.
        ELSE.
          RETURN.
        ENDIF.
      ELSE.
        MESSAGE 'Selezionare casella "Aggiungi"' TYPE 'I'.
        RETURN.
      ENDIF.
  ENDCASE.

  CASE 'X'.
    WHEN r_add.
      PERFORM set_user_data USING p_id p_uname p_fname p_lname p_bdate p_role
                            CHANGING new_record.

      PERFORM add_user_data USING new_record.
    WHEN r_src.
      PERFORM search_user_data USING p_id p_uname p_fname p_lname p_bdate p_role.
    WHEN r_del.
      PERFORM delete_user_data USING p_id p_uname p_fname p_lname p_bdate p_role.
    WHEN r_mod.
      PERFORM modify_user_data USING p_id p_uname p_fname p_lname p_bdate p_role.
    WHEN OTHERS.
  ENDCASE.

  " Creazione del titolo
  PERFORM top_of_page_2.

  " Creazione del field catalog
  PERFORM create_fieldcatalog.

  g_repid = sy-repid.

  TRY.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          it_fieldcat            = t_fieldcat[]
          i_callback_program     = g_repid
          i_callback_top_of_page = 'TOP_OF_PAGE_2'
        TABLES
          t_outtab               = it_user_table[]
        EXCEPTIONS
          program_error          = 1
          OTHERS                 = 2.
    CATCH cx_sy_dyn_call_param_not_found INTO exc->px_error.
      exc->exc_param_not_found( ).
    CATCH cx_sy_dyn_call_illegal_type INTO exc->lx_error.
      exc->exc_illegal_type( ).
  ENDTRY.

*&---------------------------------------------------------------------*
*&      Form  set_user_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->_ID        text
*      -->_UNAME     text
*      -->_FNAME     text
*      -->_LNAME     text
*      -->_BDATE     text
*      -->_ROLE      text
*      <--_RECORD    text
*----------------------------------------------------------------------*
FORM set_user_data USING _id TYPE c
                         _uname TYPE string
                         _fname TYPE string
                         _lname TYPE string
                         _bdate TYPE dats
                         _role TYPE c
                   CHANGING _record TYPE zgt_usr.
  _record-u_id = _id.
  _record-u_uname = _uname.
  _record-u_fname = _fname.
  _record-u_lname = _lname.
  _record-u_bdate = _bdate.
  _record-u_role = _role.
ENDFORM.                    "set_user_data

*&---------------------------------------------------------------------*
*&      Form  add_user_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->_IT_USER_TABLE  text
*      -->_RECORD         text
*----------------------------------------------------------------------*
FORM add_user_data USING _record TYPE zgt_usr.
  " Inserisco il nuovo record in tabella
  INSERT INTO zgt_usr VALUES _record.

  " Estraggo i record dalla tabella
  SELECT u_id u_uname u_fname u_lname u_bdate u_role
      INTO TABLE it_user_table
      FROM zgt_usr.
ENDFORM.                    "add_user_data

*&---------------------------------------------------------------------*
*&      Form  search_user_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->_IT_USER_TABLE  text
*      -->_ID             text
*      -->_UNAME          text
*      -->_FNAME          text
*      -->_LNAME          text
*      -->_BDATE          text
*      -->_ROLE           text
*----------------------------------------------------------------------*
FORM search_user_data USING _id _uname _fname _lname _bdate _role.

  " Se i campi sono vuoti estraggo tutti i record
  IF _id IS INITIAL AND _uname IS INITIAL AND _fname IS INITIAL AND _lname IS INITIAL AND _bdate IS INITIAL AND _role IS INITIAL.
    SELECT u_id u_uname u_fname u_lname u_bdate u_role
      INTO TABLE it_user_table
      FROM zgt_usr.

  ELSE.
    SELECT u_id u_uname u_fname u_lname u_bdate u_role
        INTO TABLE it_user_table
        FROM zgt_usr
        WHERE ( u_id = _id OR
                u_uname = _uname OR
                u_fname = _fname OR
                u_lname = _lname OR
                u_bdate = _bdate OR
                u_role = _role ).

  ENDIF.
  IF sy-subrc <> 0.
    MESSAGE 'Nessun utente trovato' TYPE 'E'.
  ENDIF.

ENDFORM.                    "search_user_data

*&---------------------------------------------------------------------*
*&      Form  delete_user_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->_IT_USER_TABLE  text
*      -->_ID             text
*      -->_UNAME          text
*      -->_FNAME          text
*      -->_LNAME          text
*      -->_BDATE          text
*      -->_ROLE           text
*----------------------------------------------------------------------*
FORM delete_user_data USING _id _uname _fname _lname _bdate _role.

  " Verifica se il record esiste
  DATA: temp_id TYPE zgt_usr-u_id.
  SELECT SINGLE u_id
    INTO temp_id
    FROM zgt_usr
    WHERE u_id = _id.

  IF sy-subrc = 0.
    CLEAR temp_id.
    DELETE FROM zgt_usr
        WHERE u_id = _id.

    SELECT u_id u_uname u_fname u_lname u_bdate u_role
        INTO TABLE it_user_table
        FROM zgt_usr.
  ELSE.
    MESSAGE 'Nessun utente trovato' TYPE 'E'.
  ENDIF.

ENDFORM.                    "delete_user_data

*&---------------------------------------------------------------------*
*&      Form  modify_user_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->_IT_USER_TABLE  text
*      -->_ID             text
*      -->_UNAME          text
*      -->_FNAME          text
*      -->_LNAME          text
*      -->_BDATE          text
*      -->_ROLE           text
*----------------------------------------------------------------------*
FORM modify_user_data USING _id TYPE c
                            _uname TYPE string
                            _fname TYPE string
                            _lname TYPE string
                            _bdate TYPE dats
                            _role TYPE c.

  " Verifica se il record esiste
  DATA: temp_id TYPE zgt_usr-u_id.
  SELECT SINGLE u_id
    INTO temp_id
    FROM zgt_usr
    WHERE u_id = _id.

  IF sy-subrc = 0.
    CLEAR temp_id.
    " Aggiorna il record con i nuovi valori solo se i campi non sono nulli
    IF _uname IS NOT INITIAL.
      UPDATE zgt_usr SET u_uname = _uname WHERE u_id = _id.
    ENDIF.
    IF _fname IS NOT INITIAL.
      UPDATE zgt_usr SET u_fname = _fname WHERE u_id = _id.
    ENDIF.
    IF _lname IS NOT INITIAL.
      UPDATE zgt_usr SET u_lname = _lname WHERE u_id = _id.
    ENDIF.
    IF _bdate IS NOT INITIAL.
      UPDATE zgt_usr SET u_bdate = _bdate WHERE u_id = _id.
    ENDIF.
    IF _role IS NOT INITIAL.
      UPDATE zgt_usr SET u_role = _role WHERE u_id = _id.
    ENDIF.

    " Estraggo i record aggiornati dalla tabella
    SELECT u_id u_uname u_fname u_lname u_bdate u_role
      INTO TABLE it_user_table
      FROM zgt_usr.
  ELSE.
    " Gestione del caso in cui il record non esiste
    MESSAGE 'Record non trovato' TYPE 'E'.
  ENDIF.

ENDFORM.                    "modify_user_data

* Popolazione del campo a discesa
FORM populate_roles.
  DATA: lt_roles TYPE TABLE OF vrm_value,
        ls_role  TYPE vrm_value.

  CLEAR lt_roles.

  ls_role-key = 'User'.
  ls_role-text = 'User'.
  APPEND ls_role TO lt_roles.

  ls_role-key = 'Admin'.
  ls_role-text = 'Admin'.
  APPEND ls_role TO lt_roles.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_ROLE'
      values = lt_roles.
ENDFORM.                    "populate_roles

*&---------------------------------------------------------------------*
*&      Form  create_fieldcatalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_fieldcatalog.
  CLEAR t_fieldcat[].

  wa_fieldcat-tabname = 'USER_TABLE_STRUCT'.
  wa_fieldcat-fieldname = 'ID'.
  wa_fieldcat-seltext_m = 'ID'.
  wa_fieldcat-outputlen = 4.
  APPEND wa_fieldcat TO t_fieldcat.

  wa_fieldcat-fieldname = 'UNAME'.
  wa_fieldcat-seltext_m = 'Username'.
  wa_fieldcat-outputlen = 15.
  APPEND wa_fieldcat TO t_fieldcat.

  wa_fieldcat-fieldname = 'FNAME'.
  wa_fieldcat-seltext_m = 'First Name'.
  wa_fieldcat-outputlen = 15.
  APPEND wa_fieldcat TO t_fieldcat.

  wa_fieldcat-fieldname = 'LNAME'.
  wa_fieldcat-seltext_m = 'Last Name'.
  wa_fieldcat-outputlen = 15.
  APPEND wa_fieldcat TO t_fieldcat.

  wa_fieldcat-fieldname = 'BDATE'.
  wa_fieldcat-seltext_m = 'Birth Date'.
  wa_fieldcat-outputlen = 10.
  APPEND wa_fieldcat TO t_fieldcat.

  wa_fieldcat-fieldname = 'ROLE'.
  wa_fieldcat-seltext_m = 'Role'.
  wa_fieldcat-outputlen = 5.
  APPEND wa_fieldcat TO t_fieldcat.

ENDFORM.                    "create_fieldcatalog

*&---------------------------------------------------------------------*
*&      Form  import_excel_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM import_excel_file.

  DATA: lv_file_name  TYPE ibipparms-path,
        lv_rc TYPE i.

  " Apri il file dialog
  TRY.
      CALL FUNCTION 'F4_FILENAME'
        EXPORTING
          program_name  = sy-repid
          dynpro_number = sy-dynnr
        IMPORTING
          file_name     = lv_file_name
        EXCEPTIONS
          mask_too_long = 1
          OTHERS        = 2.
    CATCH cx_sy_dyn_call_illegal_type INTO exc->lx_error.
      exc->exc_illegal_type( ).
  ENDTRY.

  IF lv_file_name IS INITIAL.
    MESSAGE 'Nessun file selezionato' TYPE 'E'.
    RETURN.
  ENDIF.

  " Importa il file Excel
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = lv_file_name
      i_begin_col             = 1
      i_begin_row             = 1
      i_end_col               = 256
      i_end_row               = 65536
    TABLES
      intern                  = it_excel_data[]
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
    MESSAGE 'Errore durante l''importazione del file Excel' TYPE 'E'.
  ELSE.
    " Processa i dati importati
    PERFORM process_excel_data.
  ENDIF.
ENDFORM.                    "import_excel_file

*&---------------------------------------------------------------------*
*&      Form  process_excel_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->_IT_EXCEL_DATA  text
*      -->_LS_EXCEL_ROW   text
*      -->_LS_USER_TABLE  text
*----------------------------------------------------------------------*
FORM process_excel_data.
  DATA: lv_row TYPE i,
        lv_col TYPE i,
        lv_value TYPE string.

  LOOP AT it_excel_data INTO ls_excel_row.
    lv_row = ls_excel_row-row.
    lv_col = ls_excel_row-col.
    lv_value = ls_excel_row-value.

    CASE lv_col.
      WHEN 1.
        ls_user_table-id = lv_value.
      WHEN 2.
        ls_user_table-uname = lv_value.
      WHEN 3.
        ls_user_table-fname = lv_value.
      WHEN 4.
        ls_user_table-lname = lv_value.
      WHEN 5.
        ls_user_table-bdate = lv_value.
      WHEN 6.
        ls_user_table-role = lv_value.
    ENDCASE.

    " Aggiungi il record alla tabella interna quando la riga è completa
    IF lv_col = 6.
      APPEND ls_user_table TO it_user_table.
      CLEAR ls_user_table.
    ENDIF.
    "process_excel_data
  ENDLOOP.
ENDFORM.                    "process_excel_data

*&---------------------------------------------------------------------*
*&      Form  add_records_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->_IT_USER_TABLE  text
*      <--_RECORD         text
*----------------------------------------------------------------------*
FORM add_records_table CHANGING _record TYPE zgt_usr.

  FIELD-SYMBOLS: <fs_user_table> TYPE ty_user_table.

  LOOP AT it_user_table ASSIGNING <fs_user_table>.
    _record-u_id = <fs_user_table>-id.
    _record-u_uname = <fs_user_table>-uname.
    _record-u_fname = <fs_user_table>-fname.
    _record-u_lname = <fs_user_table>-lname.
    _record-u_bdate = <fs_user_table>-bdate.
    _record-u_role = <fs_user_table>-role.
    INSERT INTO zgt_usr VALUES _record.
  ENDLOOP.
ENDFORM.                    "add_records_table

*&---------------------------------------------------------------------*
*&      Form  create_alv_title
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_HEADER  text
*----------------------------------------------------------------------*
FORM top_of_page_1.

  CLEAR lt_header.

  ls_header-typ = 'H'. " Tipo di riga (H = Header)
  ls_header-info = 'Utenti Importati'. " Testo del titolo
  APPEND ls_header TO lt_header.

  ls_header-typ = 'S'. " Tipo di riga (S = Subheader)
  ls_header-info = 'Visualizzazione dati utente'. " Testo del sottotitolo
  APPEND ls_header TO lt_header.

  DESCRIBE TABLE it_user_table LINES lv_lines.
  lv_linesc = lv_lines.
  CONCATENATE 'Totale record importati: ' lv_linesc
  INTO lt_line SEPARATED BY space.
  ls_header-typ = 'A'.
  ls_header-info = lt_line.
  APPEND ls_header TO lt_header.

  TRY.
      CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
        EXPORTING
          it_list_commentary = lt_header.
    CATCH cx_sy_dyn_call_illegal_type INTO exc->lx_error.
      exc->exc_illegal_type( ).
  ENDTRY.

ENDFORM.                    "create_alv_title

*&---------------------------------------------------------------------*
*&      Form  top_of_page_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM top_of_page_2.

  CLEAR lt_header.

  ls_header-typ = 'H'. " Tipo di riga (H = Header)
  ls_header-info = 'Utenti'. " Testo del titolo
  APPEND ls_header TO lt_header.

  ls_header-typ = 'S'. " Tipo di riga (S = Subheader)
  ls_header-info = 'Visualizzazione dati utente'. " Testo del sottotitolo
  APPEND ls_header TO lt_header.

  DESCRIBE TABLE it_user_table LINES lv_lines.
  lv_linesc = lv_lines.
  CONCATENATE 'Totale record: ' lv_linesc
  INTO lt_line SEPARATED BY space.
  ls_header-typ = 'A'.
  ls_header-info = lt_line.
  APPEND ls_header TO lt_header.

  TRY.
      CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
        EXPORTING
          it_list_commentary = lt_header.
    CATCH cx_sy_dyn_call_illegal_type INTO exc->lx_error.
      exc->exc_illegal_type( ).
  ENDTRY.

ENDFORM.                    "top_of_page_2
