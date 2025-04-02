REPORT z_gt_test.

" Definisco una struttura di tipo USER_DATA per contenere i campi input dei parametri
TYPES: BEGIN OF user_data,
     id(4) TYPE c,
     uname TYPE string,
     fname TYPE string,
     lname TYPE string,
     bdate TYPE dats,
     role(5) TYPE c,
     descr TYPE string,
   END OF user_data.

* Definizione parametri di input
PARAMETERS: p_id(4) TYPE c,
            p_uname TYPE string VISIBLE LENGTH 15,
            p_fname TYPE string VISIBLE LENGTH 15,
            p_lname TYPE string VISIBLE LENGTH 15,
            p_bdate TYPE dats,
            p_role TYPE c LENGTH 5 AS LISTBOX VISIBLE LENGTH 10 DEFAULT 'User'.


* Definizione di una lista a discesa per il campo p_role
INITIALIZATION.
  PERFORM populate_roles.

* Gestione della richiesta di valore per il campo a discesa
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_role.
  PERFORM populate_roles.

* Popolazione del campo a discesa
FORM populate_roles.
  DATA: lt_roles TYPE TABLE OF vrm_value,
        ls_role  TYPE vrm_value.

  CLEAR lt_roles.

  ls_role-key = 'Admin'.
  ls_role-text = 'Admin'.
  APPEND ls_role TO lt_roles.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_ROLE'
      values = lt_roles.
ENDFORM.                    "populate_roles

*----------------------------------------------------------------------*
*       CLASS user_report DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS user_report DEFINITION.
  PUBLIC SECTION.
    METHODS: display_data,
             set_user_data.

  PRIVATE SECTION.
    " Definisco un array user_array per memorizzare i dati degli utenti
    DATA: it_user_data TYPE TABLE OF user_data.
    DATA: ls_user_data TYPE user_data.
ENDCLASS.                    "user_report DEFINITION

*----------------------------------------------------------------------*
*       CLASS user_report IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS user_report IMPLEMENTATION.
  METHOD set_user_data.
    " Definisco una variabile temporanea di tipo USER_DATA
    " Copio i parametri di input nei campi dati della struttura
    ls_user_data-id = p_id.
    ls_user_data-uname = p_uname.
    ls_user_data-fname = p_fname.
    ls_user_data-lname = p_lname.
    ls_user_data-bdate = p_bdate.
    ls_user_data-role = p_role.
    " Aggiungo ls_user_data ad user_array
    APPEND ls_user_data TO it_user_data.
  ENDMETHOD.                    "set_user_data
  METHOD display_data.
    " Logica del programma
    SKIP 1.        " Salta una riga
    " Stampo i dati utente inseriti nella struttura privata ls_user_data
    LOOP AT it_user_data INTO ls_user_data.
      WRITE: / 'ID: ', ls_user_data-id,
          / 'Username: ', ls_user_data-uname,
          / 'First name: ', ls_user_data-fname,
          / 'Last name: ', ls_user_data-lname,
          / 'Birth date: ', ls_user_data-bdate,
          / 'Role: ', ls_user_data-role.
    ENDLOOP.
  ENDMETHOD.                    "display_data
ENDCLASS.                    "user_report IMPLEMENTATION

* Report body
START-OF-SELECTION.
  WRITE 'Nuovo utente creato'.
  SKIP 1.
  WRITE 'Dati utente:'.
  DATA report TYPE REF TO user_report.
  CREATE OBJECT report.
  report->set_user_data( ).
  report->display_data( ).