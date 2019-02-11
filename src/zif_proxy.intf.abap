INTERFACE zif_proxy
  PUBLIC .
  TYPES interfaces TYPE STANDARD TABLE OF REF TO cl_abap_intfdescr WITH DEFAULT KEY.
  METHODS get_invocation_handler RETURNING VALUE(r_invocation_handler) TYPE REF TO zif_invocation_handler.
  METHODS set_invocation_handler IMPORTING i_invocation_handler TYPE REF TO zif_invocation_handler.
  METHODS get_interfaces RETURNING VALUE(r_interfaces) TYPE zif_proxy=>interfaces.
  METHODS set_interfaces IMPORTING i_interfaces TYPE zif_proxy=>interfaces.
  METHODS get_super_class RETURNING VALUE(r_super_class) TYPE REF TO cl_abap_classdescr.
  METHODS set_super_class IMPORTING i_super_class TYPE REF TO cl_abap_classdescr.
  METHODS get_object RETURNING VALUE(r_object) TYPE REF TO object.
  METHODS set_object IMPORTING i_object TYPE REF TO object.
ENDINTERFACE.
