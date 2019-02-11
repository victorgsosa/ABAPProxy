*"* use this source file for your ABAP unit test classes
CLASS lcl_invocation_handler DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PUBLIC SECTION.
    DATA invoked TYPE abap_bool.
    METHODS constructor.
    INTERFACES zif_invocation_handler.

ENDCLASS.



CLASS lcl_invocation_handler IMPLEMENTATION.

  METHOD constructor.
    me->invoked = abap_false.
  ENDMETHOD.

  METHOD zif_invocation_handler~invoke.
    me->invoked = abap_true.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_invocation_handler_w_obj DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PUBLIC SECTION.
    DATA object TYPE REF TO object.
    METHODS constructor.
    INTERFACES zif_invocation_handler.

ENDCLASS.

CLASS lcl_invocation_handler_w_obj IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.

  METHOD zif_invocation_handler~invoke.
    me->object = object.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_proxy_factory_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      test_create_with_one_interface FOR TESTING,
      test_create_without_interface FOR TESTING,
      test_create_w_final_s_class FOR TESTING,
      test_create_w_s_class_wo_arg FOR TESTING,
      test_create_w_s_class_bad_arg FOR TESTING,
      test_create_w_s_class_w_arg FOR TESTING.
ENDCLASS.

CLASS lcl_proxy_factory_test IMPLEMENTATION.

  METHOD test_create_with_one_interface.
    DATA interfaces TYPE zif_proxy=>interfaces.
    DATA interface_descr TYPE REF TO cl_abap_intfdescr.
    DATA proxy TYPE REF TO if_os_transaction.
    interface_descr ?= cl_abap_intfdescr=>describe_by_name( 'if_os_transaction' ).
    APPEND interface_descr TO interfaces.
    DATA(invocation_handler) = NEW lcl_invocation_handler( ).
    proxy ?= zcl_proxy_factory=>proxy_for( i_invocation_handler = invocation_handler i_interfaces = interfaces ).
    cl_abap_unit_assert=>assert_equals( msg = 'Handler must be on no invoked state' exp = abap_false act = invocation_handler->invoked ).
    proxy->start( ).
    cl_abap_unit_assert=>assert_equals( msg = 'Handler must be on invoked state' exp = abap_true act = invocation_handler->invoked ).
  ENDMETHOD.



  METHOD test_create_w_final_s_class.
    DATA interfaces TYPE zif_proxy=>interfaces.
    DATA interface_descr TYPE REF TO cl_abap_intfdescr.
    DATA super_class_descr TYPE REF TO cl_abap_classdescr.
    DATA proxy TYPE REF TO cl_os_ex_flight.
    DATA raised TYPE abap_bool VALUE abap_false..
    interface_descr ?= cl_abap_intfdescr=>describe_by_name( 'if_os_transaction' ).
    super_class_descr ?= cl_abap_classdescr=>describe_by_name( 'cl_os_ex_flight' ).
    APPEND interface_descr TO interfaces.
    DATA(invocation_handler) = NEW lcl_invocation_handler( ).
    TRY.
        proxy ?= zcl_proxy_factory=>proxy_for( i_invocation_handler = invocation_handler i_interfaces = interfaces i_super_class = super_class_descr ).
      CATCH zcx_proxy.
        raised = abap_true.
    ENDTRY.
    cl_abap_unit_assert=>assert_equals( msg = 'An exception must be raised' exp = abap_true act = raised ).
  ENDMETHOD.

  METHOD test_create_without_interface.
    DATA interfaces TYPE zif_proxy=>interfaces.
    DATA proxy TYPE REF TO zif_proxy.
    DATA raised TYPE abap_bool VALUE abap_false.
    DATA(invocation_handler) = NEW lcl_invocation_handler( ).
    TRY.
        proxy ?= zcl_proxy_factory=>proxy_for( i_invocation_handler = invocation_handler i_interfaces = interfaces ).
      CATCH zcx_proxy.
        raised = abap_true.
    ENDTRY.
    cl_abap_unit_assert=>assert_equals( msg = 'An exception must not be raised' exp = abap_false act = raised ).
  ENDMETHOD.

  METHOD test_create_w_s_class_wo_arg.
    DATA interfaces TYPE zif_proxy=>interfaces.
    DATA interface_descr TYPE REF TO cl_abap_intfdescr.
    DATA super_class_descr TYPE REF TO cl_abap_classdescr.
    DATA proxy TYPE REF TO cl_os_state.
    interface_descr ?= cl_abap_intfdescr=>describe_by_name( 'if_os_transaction' ).
    super_class_descr ?= cl_abap_classdescr=>describe_by_name( 'cl_os_state' ).
    APPEND interface_descr TO interfaces.
    DATA(invocation_handler) = NEW lcl_invocation_handler_w_obj( ).
    proxy ?= zcl_proxy_factory=>proxy_for( i_invocation_handler = invocation_handler i_interfaces = interfaces i_super_class = super_class_descr ).
    proxy->set_state_from_object( i_object = proxy ).
    cl_abap_unit_assert=>assert_bound( msg = 'Original object must be bound' act = invocation_handler->object ).
  ENDMETHOD.

  METHOD test_create_w_s_class_w_arg.
    DATA interfaces TYPE zif_proxy=>interfaces.
    DATA interface_descr TYPE REF TO cl_abap_intfdescr.
    DATA super_class_descr TYPE REF TO cl_abap_classdescr.
    DATA proxy TYPE REF TO cl_os_exception_info.
    DATA id TYPE t100-arbgb VALUE 'id'.
    DATA no TYPE t100-msgnr VALUE '001'.
    DATA(r_id) = REF #( id ).
    DATA(r_no) = REF #( no ).

    DATA(parameters) = VALUE abap_parmbind_tab(
        ( kind = cl_abap_objectdescr=>exporting name = 'ID' value = r_id )
        ( kind = cl_abap_objectdescr=>exporting name = 'NR' value = r_no )
    ).
    interface_descr ?= cl_abap_intfdescr=>describe_by_name( 'if_os_transaction' ).
    super_class_descr ?= cl_abap_classdescr=>describe_by_name( 'cl_os_exception_info' ).
    APPEND interface_descr TO interfaces.
    DATA(invocation_handler) = NEW lcl_invocation_handler_w_obj( ).
    proxy ?= zcl_proxy_factory=>proxy_for(
        i_invocation_handler = invocation_handler
        i_interfaces = interfaces
        i_super_class = super_class_descr
        i_parameters = parameters
    ).
    proxy->raise( ).
    cl_abap_unit_assert=>assert_bound( msg = 'Original object must be bound' act = invocation_handler->object ).
  ENDMETHOD.

  METHOD test_create_w_s_class_bad_arg.
    DATA interfaces TYPE zif_proxy=>interfaces.
    DATA interface_descr TYPE REF TO cl_abap_intfdescr.
    DATA super_class_descr TYPE REF TO cl_abap_classdescr.
    DATA proxy TYPE REF TO cl_os_exception_info.
    DATA id TYPE t100-arbgb VALUE 'id'.
    DATA no TYPE t100-msgnr VALUE '001'.
    DATA raised TYPE abap_bool VALUE abap_false.
    DATA(r_id) = REF #( id ).
    DATA(r_no) = REF #( no ).

    DATA(parameters) = VALUE abap_parmbind_tab(
        ( kind = cl_abap_objectdescr=>exporting name = 'IDO' value = r_id )
        ( kind = cl_abap_objectdescr=>exporting name = 'NR' value = r_no )
    ).
    interface_descr ?= cl_abap_intfdescr=>describe_by_name( 'if_os_transaction' ).
    super_class_descr ?= cl_abap_classdescr=>describe_by_name( 'cl_os_exception_info' ).
    APPEND interface_descr TO interfaces.
    DATA(invocation_handler) = NEW lcl_invocation_handler_w_obj( ).
    TRY.
        proxy ?= zcl_proxy_factory=>proxy_for(
            i_invocation_handler = invocation_handler
            i_interfaces = interfaces
            i_super_class = super_class_descr
            i_parameters = parameters
        ).
      CATCH  zcx_proxy.
        raised = abap_true.
    ENDTRY.
    cl_abap_unit_assert=>assert_equals( msg = 'An exception must be raised' exp = abap_true act = raised ).
  ENDMETHOD.

ENDCLASS.
