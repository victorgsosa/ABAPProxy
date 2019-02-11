INTERFACE zif_invocation_handler
  PUBLIC .

  METHODS invoke
    IMPORTING
      proxy      TYPE REF TO object
      method     TYPE abap_methdescr
      object     TYPE REF TO object OPTIONAL
    CHANGING
      parameters TYPE abap_parmbind_tab.

ENDINTERFACE.
