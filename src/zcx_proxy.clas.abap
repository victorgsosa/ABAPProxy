CLASS zcx_proxy DEFINITION
  PUBLIC
  INHERITING FROM cx_no_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF zcx_proxy,
        msgid TYPE symsgid VALUE 'ZPROXY',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'ATTR1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_proxy .
    DATA: attr1 TYPE syst_msgv,
          attr2 TYPE syst_msgv,
          attr3 TYPE syst_msgv,
          attr4 TYPE syst_msgv.
    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !attr1     type syst_msgv optional
        !attr2     TYPE syst_msgv optional
        !attr3     TYPE syst_msgv optional
        !attr4     TYPE syst_msgv optional.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcx_proxy IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
    me->attr1 = attr1.
    me->attr2 = attr2.
    me->attr3 = attr3.
    me->attr4 = attr4.
  ENDMETHOD.
ENDCLASS.
