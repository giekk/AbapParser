REPORT z_gt_test_3.

TABLES: zgt_usr.

TYPES: BEGIN OF ty_user_table,
    id TYPE zgt_usr-u_id,
END OF ty_user_table.

DATA: new_record        TYPE zgt_usr,
      it_user_table     TYPE TABLE OF ty_user_table,
      user_table_lines  TYPE i.


FIELD-SYMBOLS: <record> TYPE ty_user_table.

*--------------------------------------------------------------------*
* Parametri per la maschera di selezione
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

PARAMETERS: p_id TYPE c LENGTH 4.

SELECTION-SCREEN END OF BLOCK b1.


*--------------------------------------------------------------------*
* Codice/Query
START-OF-SELECTION.

  new_record-u_id = p_id.
  INSERT INTO zgt_usr VALUES new_record.

  SELECT u_id
      INTO TABLE it_user_table
      FROM zgt_usr.

  IF sy-subrc = 0.
    DESCRIBE TABLE it_user_table LINES user_table_lines.
    WRITE: / 'Query eseguita con successo. Numero di record estratti:', user_table_lines.
  ELSE.
    WRITE: / 'Errore nell''esecuzione della query. Codice di ritorno:', sy-subrc.
  ENDIF.

  " Stampo record
  SKIP 1.
  WRITE: 'Records:'.
  SKIP 1.
  LOOP AT it_user_table ASSIGNING <record>.
    WRITE: / 'ID: ', <record>-id.
  ENDLOOP.