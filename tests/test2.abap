REPORT z_gt_test_2.

TYPES: BEGIN OF ty_user_table,
    id    TYPE c LENGTH 4,
    uname TYPE string,
    fname TYPE string,
    lname TYPE string,
    bdate TYPE dats,
    role  TYPE c LENGTH 5,
END OF ty_user_table.

DATA: it_user_table TYPE TABLE OF ty_user_table,
      ls_user_table LIKE LINE OF it_user_table.


FIELD-SYMBOLS: <fs_user_table> TYPE ty_user_table.



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



START-OF-SELECTION.
  " Assegno dei dati al mio record con la funzione set_user_data
  PERFORM set_user_data USING p_id p_uname p_fname p_lname p_bdate p_role
                        CHANGING ls_user_table.

  " Appendo il mio record alla tabella user_table
  APPEND ls_user_table TO it_user_table.



  LOOP AT it_user_table ASSIGNING <fs_user_table>.
    " Controllo se l'assegnazione è avvenuta con successo
    IF sy-subrc = 0.
      WRITE: / 'Field symbol <fs_user_table> points to lv_value:'.
      " Stampo i valori
      WRITE: / 'ID: ', <fs_user_table>-id,
          / 'Username: ', <fs_user_table>-uname,
          / 'First name: ', <fs_user_table>-fname,
          / 'Last name: ', <fs_user_table>-lname,
          / 'Birth date: ', <fs_user_table>-bdate,
          / 'Role: ', <fs_user_table>-role.
    ENDIF.
  ENDLOOP.

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

*&---------------------------------------------------------------------*
*&      Form  set_user_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->_ID            text
*      -->_UNAME         text
*      -->_FNAME         text
*      -->_LNAME         text
*      -->_BDATE         text
*      -->_ROLE          text
*      <--_LS_USER_DATA  text
*----------------------------------------------------------------------*
FORM set_user_data USING _id TYPE c
                                _uname TYPE string
                                _fname TYPE string
                                _lname TYPE string
                                _bdate TYPE dats
                                _role TYPE c
                          CHANGING _ls_user_data TYPE ty_user_table.
  _ls_user_data-id = _id.
  _ls_user_data-uname = _uname.
  _ls_user_data-fname = _fname.
  _ls_user_data-lname = _lname.
  _ls_user_data-bdate = _bdate.
  _ls_user_data-role = _role.
ENDFORM.                    "set_user_data