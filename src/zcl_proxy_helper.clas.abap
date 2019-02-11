CLASS zcl_proxy_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES code_lines TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    CLASS-METHODS generate_subroutine_pool
      IMPORTING
        it_code   TYPE zcl_proxy_helper=>code_lines
      EXPORTING
        e_prog    TYPE string
        e_subrc   TYPE sy-subrc
        e_message TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_proxy_helper IMPLEMENTATION.

  METHOD generate_subroutine_pool.
* helper method that generates the local class that implements the mock object functions
    FREE: e_prog,
          e_subrc,
          e_message.

    CATCH SYSTEM-EXCEPTIONS generate_subpool_dir_full = 9.
      GENERATE SUBROUTINE POOL it_code NAME e_prog MESSAGE e_message.
      IF sy-subrc IS NOT INITIAL.
        e_subrc = sy-subrc.
      ENDIF.
    ENDCATCH.
    IF sy-subrc = 9.
      e_subrc = sy-subrc.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
