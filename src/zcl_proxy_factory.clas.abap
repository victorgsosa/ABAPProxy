CLASS zcl_proxy_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PROTECTED .

  PUBLIC SECTION.

    CLASS-METHODS proxy_for
      IMPORTING
                i_invocation_handler TYPE REF TO zif_invocation_handler
                i_interfaces         TYPE zif_proxy=>interfaces
                i_super_class        TYPE REF TO cl_abap_classdescr OPTIONAL
                i_parameters         TYPE abap_parmbind_tab OPTIONAL
      RETURNING VALUE(r_proxy)       TYPE REF TO zif_proxy.


  PROTECTED SECTION.

    METHODS constructor
      IMPORTING
        i_invocation_handler TYPE REF TO zif_invocation_handler
        i_interfaces         TYPE zif_proxy=>interfaces
        i_super_class        TYPE REF TO cl_abap_classdescr OPTIONAL
        i_parameters         TYPE abap_parmbind_tab OPTIONAL.
    METHODS create
      RETURNING VALUE(r_proxy) TYPE REF TO zif_proxy.
  PRIVATE SECTION.
    CONSTANTS constructor_name TYPE abap_methname VALUE 'CONSTRUCTOR'.
    DATA invocation_handler TYPE REF TO zif_invocation_handler.
    DATA super_class TYPE REF TO cl_abap_classdescr.
    DATA interfaces TYPE zif_proxy=>interfaces.
    DATA parameters TYPE abap_parmbind_tab.
    METHODS generate_class_name
      RETURNING
        VALUE(r_class_name) TYPE abap_classname.
    METHODS generate_definition_header
      IMPORTING
        i_class_name    TYPE abap_classname
      RETURNING
        VALUE(r_result) TYPE zcl_proxy_helper=>code_lines.
    METHODS get_methods_by_visibility
      IMPORTING
        i_objectdescr   TYPE REF TO cl_abap_objectdescr
        i_visibility    TYPE abap_visibility
      RETURNING
        VALUE(r_result) TYPE abap_methdescr_tab.
    METHODS generate_interface
      IMPORTING
        i_interface     TYPE REF TO cl_abap_intfdescr
      RETURNING
        VALUE(r_result) TYPE string.
    METHODS generate_method_definition
      IMPORTING
        i_method      TYPE abap_methdescr
      RETURNING
        VALUE(r_code) TYPE zcl_proxy_helper=>code_lines.
    METHODS create_instance
      IMPORTING
        i_final_class_name TYPE string
      RETURNING
        VALUE(r_result)    TYPE REF TO zif_proxy.
    METHODS generate_class_definition
      IMPORTING
        i_class_name  TYPE abap_classname
      RETURNING
        VALUE(r_code) TYPE zcl_proxy_helper=>code_lines.
    METHODS generate_class_implementation
      IMPORTING
        i_class_name  TYPE abap_classname
      RETURNING
        VALUE(r_code) TYPE zcl_proxy_helper=>code_lines.
    METHODS generate_method_implementation
      IMPORTING
        i_method      TYPE abap_methdescr
        i_interface   TYPE REF TO cl_abap_intfdescr OPTIONAL
      RETURNING
        VALUE(r_code) TYPE zcl_proxy_helper=>code_lines.
    METHODS create_object_instance
      RETURNING
        VALUE(r_result) TYPE REF TO object.
    METHODS generate_method_redefinition
      IMPORTING
        i_method TYPE abap_methdescr
      CHANGING
        c_code   TYPE zcl_proxy_helper=>code_lines.
    METHODS parameter_mapping
      IMPORTING
        i_method TYPE abap_methdescr
      CHANGING
        c_code   TYPE zcl_proxy_helper=>code_lines.
    METHODS generate_constructor
      IMPORTING
        i_method TYPE abap_methdescr
      CHANGING
        c_code   TYPE zcl_proxy_helper=>code_lines.
    METHODS generate_definition_parameters
      IMPORTING
        i_method        TYPE abap_methdescr
      RETURNING
        VALUE(r_result) TYPE zcl_proxy_helper=>code_lines.
    METHODS generate_calling_parameters
      IMPORTING
        i_method        TYPE abap_methdescr
      RETURNING
        VALUE(r_result) TYPE zcl_proxy_helper=>code_lines.
    METHODS generate_parameter_definition
      IMPORTING
        i_method        TYPE abap_methdescr
        i_parameter     TYPE abap_parmdescr
      RETURNING
        VALUE(r_result) TYPE string.
    METHODS generate_parameter_calling
      IMPORTING
        i_parameter     TYPE abap_parmdescr
        i_var_name      TYPE string
      RETURNING
        VALUE(r_result) TYPE string.
ENDCLASS.



CLASS zcl_proxy_factory IMPLEMENTATION.

  METHOD constructor.

    me->invocation_handler = i_invocation_handler.
    me->super_class = i_super_class.
    me->interfaces = i_interfaces.
    me->parameters = i_parameters.

  ENDMETHOD.
  METHOD proxy_for.
    DATA(proxy_factory) = NEW zcl_proxy_factory( i_invocation_handler = i_invocation_handler i_interfaces = i_interfaces i_super_class = i_super_class i_parameters = i_parameters ).
    r_proxy = proxy_factory->create( ).
  ENDMETHOD.

  METHOD create.
    DATA code TYPE zcl_proxy_helper=>code_lines.
    DATA(class_name) = generate_class_name( ) .
    APPEND 'program.' TO code.
    APPEND LINES OF generate_class_definition( class_name ) TO code.
    APPEND LINES OF generate_class_implementation( class_name ) TO code.
    zcl_proxy_helper=>generate_subroutine_pool( EXPORTING it_code = code IMPORTING e_prog = DATA(prog) e_subrc = DATA(subrc) e_message = DATA(message) ).
    IF subrc NE 0.
      RAISE EXCEPTION TYPE zcx_proxy
        EXPORTING
          textid = zcx_proxy=>zcx_proxy
          attr1  = CONV #( message ).
    ENDIF.
    DATA(final_class_name) = |\\PROGRAM={ prog }\\CLASS={ class_name }|.
    r_proxy = create_instance( final_class_name ).
  ENDMETHOD.


  METHOD generate_class_name.
    TRY.
        r_class_name = |LCL_{ cl_system_uuid=>create_uuid_c26_static( ) }|.
      CATCH cx_uuid_error INTO DATA(exception).
        RAISE EXCEPTION TYPE zcx_proxy
          EXPORTING
            previous = exception.
    ENDTRY.
  ENDMETHOD.


  METHOD generate_definition_header.
    CLEAR r_result.
    APPEND COND string(
        WHEN me->super_class IS BOUND THEN
           |class { i_class_name } definition inheriting from { me->super_class->get_relative_name( ) }.|
        ELSE
            |class { i_class_name } definition.|
    ) TO r_result.
  ENDMETHOD.


  METHOD get_methods_by_visibility.
    CLEAR r_result.
    IF i_objectdescr IS NOT INITIAL.
      APPEND LINES OF
          VALUE abap_methdescr_tab(
              FOR method IN i_objectdescr->methods
              WHERE ( visibility = i_visibility )
              ( method )
          ) TO r_result.
    ENDIF.
  ENDMETHOD.




  METHOD generate_interface.
    r_result = |interfaces { i_interface->get_relative_name( ) } all methods final.|.
  ENDMETHOD.


  METHOD generate_method_definition.
    IF i_method-name = constructor_name.
      APPEND |methods { i_method-name }| TO r_code.
      APPEND LINES OF generate_definition_parameters( i_method ) TO r_code.
      APPEND '.' TO r_code.
    ELSE.
      APPEND |methods { i_method-name } redefinition.| TO r_code.
    ENDIF.
  ENDMETHOD.



  METHOD create_instance.
    CLEAR r_result.
    TRY.
        IF me->parameters IS NOT INITIAL.
          CREATE OBJECT r_result TYPE (i_final_class_name)
              PARAMETER-TABLE me->parameters.
        ELSE.
          CREATE OBJECT r_result TYPE (i_final_class_name).
        ENDIF.
      CATCH cx_sy_dyn_call_error INTO DATA(exception).
        RAISE EXCEPTION TYPE zcx_proxy
          EXPORTING
            previous = exception.
    ENDTRY.
    r_result->set_invocation_handler( me->invocation_handler ).
    r_result->set_interfaces( me->interfaces ).
    r_result->set_super_class( me->super_class ).
    IF me->super_class IS BOUND.
      DATA(original_object) = create_object_instance( ).
      r_result->set_object( original_object ).
    ENDIF.
  ENDMETHOD.


  METHOD generate_class_definition.

    APPEND LINES OF generate_definition_header( i_class_name ) TO r_code.
    APPEND 'public section.' TO r_code.
    APPEND 'interfaces zif_proxy.' TO r_code.
    APPEND LINES OF VALUE zcl_proxy_helper=>code_lines( FOR interface IN me->interfaces ( generate_interface( interface ) ) ) TO r_code.
    DATA(public_methods) = get_methods_by_visibility( i_objectdescr = me->super_class i_visibility = cl_abap_objectdescr=>public ).
    LOOP AT public_methods INTO DATA(public_method).
      APPEND LINES OF generate_method_definition( public_method ) TO r_code.
    ENDLOOP.
    APPEND 'protected section.' TO r_code.
    DATA(protected_methods) = get_methods_by_visibility( i_objectdescr = me->super_class i_visibility = cl_abap_objectdescr=>protected ).
    LOOP AT protected_methods INTO DATA(protected_method).
      APPEND LINES OF generate_method_definition( protected_method ) TO r_code.
    ENDLOOP.
    APPEND 'private section.' TO r_code.
    APPEND 'data invocation_handler type ref to zif_invocation_handler.' TO r_code.
    APPEND 'data interfaces type zif_proxy=>interfaces.' TO r_code.
    APPEND 'data super_class type ref to cl_abap_classdescr.' TO r_code.
    APPEND 'data object type ref to object.' TO r_code.
    APPEND 'endclass.' TO r_code.

  ENDMETHOD.




  METHOD generate_class_implementation.
    APPEND |class { i_class_name } implementation.| TO r_code.
    APPEND 'method zif_proxy~get_invocation_handler. r_invocation_handler = me->invocation_handler. endmethod.' TO r_code.
    APPEND 'method zif_proxy~set_invocation_handler. me->invocation_handler = i_invocation_handler. endmethod.' TO r_code.
    APPEND 'method zif_proxy~get_interfaces. r_interfaces = me->interfaces. endmethod.' TO r_code.
    APPEND 'method zif_proxy~set_interfaces. me->interfaces = i_interfaces. endmethod.' TO r_code.
    APPEND 'method zif_proxy~get_super_class. r_super_class = me->super_class. endmethod.' TO r_code.
    APPEND 'method zif_proxy~set_super_class. me->super_class = i_super_class. endmethod.' TO r_code.
    APPEND 'method zif_proxy~get_object. r_object = me->object. endmethod.' TO r_code.
    APPEND 'method zif_proxy~set_object. me->object = i_object. endmethod.' TO r_code.
    IF me->super_class IS BOUND.
      DATA(super_class_methods) = VALUE abap_methdescr_tab( FOR method IN me->super_class->methods WHERE ( visibility <> cl_abap_objectdescr=>private ) ( method ) ).
      LOOP AT super_class_methods INTO DATA(super_class_method).
        APPEND LINES OF generate_method_implementation( i_method = super_class_method ) TO r_code.
      ENDLOOP.
    ENDIF.
    LOOP AT me->interfaces INTO DATA(interface).
      LOOP AT interface->methods INTO DATA(interface_method).
        APPEND LINES OF generate_method_implementation( i_method = interface_method i_interface = interface ) TO r_code.
      ENDLOOP.
    ENDLOOP.
    APPEND 'endclass.' TO r_code.
  ENDMETHOD.


  METHOD generate_method_implementation.
    CLEAR r_code.
    APPEND COND #( WHEN i_interface IS BOUND THEN |method { i_interface->get_relative_name( ) }~{ i_method-name }.| ELSE  |method { i_method-name }.| ) TO r_code.
    IF i_method-name = constructor_name.
      generate_constructor(
    EXPORTING
      i_method = i_method
    CHANGING
      c_code = r_code ).
    ELSE.
      generate_method_redefinition(
            EXPORTING
              i_method    = i_method
            CHANGING
              c_code = r_code ).
    ENDIF.
    APPEND 'endmethod.' TO r_code.
  ENDMETHOD.


  METHOD create_object_instance.
    DATA(type) = me->super_class->get_relative_name( ).
    IF me->parameters IS NOT INITIAL.
      CREATE OBJECT r_result TYPE (type)
          PARAMETER-TABLE
              me->parameters.
    ELSE.
      CREATE OBJECT r_result TYPE (type).
    ENDIF.
  ENDMETHOD.


  METHOD generate_method_redefinition.


    APPEND 'data method_parameters type abap_parmdescr_tab.' TO c_code.
    APPEND 'data exceptions type abap_excpdescr_tab.' TO c_code.

    parameter_mapping(
          EXPORTING
            i_method = i_method
          CHANGING
            c_code = c_code ).

    LOOP AT i_method-parameters INTO DATA(parameter).
      APPEND |append value abap_parmdescr( | TO c_code.
      APPEND |length = { parameter-length } | TO c_code.
      APPEND |decimals = { parameter-decimals } | TO c_code.
      APPEND |type_kind = '{ parameter-type_kind }' | TO c_code.
      APPEND |name = '{ parameter-name }' | TO c_code.
      APPEND |parm_kind = '{ parameter-parm_kind }' | TO c_code.
      APPEND |by_value = '{ parameter-by_value }' | TO c_code.
      APPEND |is_optional = '{ parameter-is_optional }' ) to method_parameters.| TO c_code.
    ENDLOOP.
    APPEND LINES OF VALUE zcl_proxy_helper=>code_lines(
        FOR exception IN i_method-exceptions (
            |append value abap_excpdescr( name = '{ exception-name }' is_resumable = '{ exception-is_resumable }' ) to exceptions.|
        )
    ) TO c_code.
    APPEND |data(method) = value abap_methdescr( | TO c_code.
    APPEND |parameters = method_parameters exceptions = exceptions | TO c_code.
    APPEND |name = '{ i_method-name }' | TO c_code.
    APPEND |for_event = '{ i_method-for_event }' | TO c_code.
    APPEND |of_class = '{ i_method-of_class }' | TO c_code.
    APPEND |visibility = '{ i_method-visibility }' | TO c_code.
    APPEND |is_interface = '{ i_method-is_interface }' | TO c_code.
    APPEND |is_inherited = '{ i_method-is_inherited }' | TO c_code.
    APPEND |is_redefined = '{ i_method-is_redefined }' | TO c_code.
    APPEND |is_abstract = '{ i_method-is_abstract }' | TO c_code.
    APPEND |is_final = '{ i_method-is_final }' | TO c_code.
    APPEND |is_class = '{ i_method-is_class }' | TO c_code.
    APPEND |alias_for = '{ i_method-alias_for }' | TO c_code.
    APPEND |is_raising_excps = '{ i_method-is_raising_excps }' ). | TO c_code.
    APPEND 'me->zif_proxy~get_invocation_handler( )->invoke( exporting proxy = me method = method object = me->zif_proxy~get_object( ) changing parameters = parameters ).' TO c_code.


  ENDMETHOD.


  METHOD parameter_mapping.
    APPEND 'data parameters type abap_parmbind_tab.' TO c_code.
    APPEND 'data r_parameter type ref to data.' TO c_code.
    APPEND 'field-symbols <parameter> type any.' TO c_code.
    LOOP AT i_method-parameters INTO DATA(parameter) .
      APPEND |assign ('{ parameter-name }') to <parameter>. | TO c_code.
      APPEND |get reference of <parameter> into r_parameter. | TO c_code.
      APPEND |insert value abap_parmbind( name = '{ parameter-name }' kind = '{ parameter-parm_kind }' value = r_parameter ) into table parameters.| TO c_code.
    ENDLOOP.

  ENDMETHOD.


  METHOD generate_constructor.
    IF me->parameters IS NOT INITIAL.
      APPEND 'call method super->constructor( ' TO c_code.
      APPEND LINES OF generate_calling_parameters( i_method ) TO c_code.
      APPEND ').' TO c_code.
    ELSE.
      APPEND 'super->constructor( ).' TO c_code.
    ENDIF.
  ENDMETHOD.


  METHOD generate_definition_parameters.
    IF line_exists( i_method-parameters[ parm_kind = cl_abap_objectdescr=>importing ] ).
      APPEND 'IMPORTING' TO r_result.
      APPEND LINES OF VALUE zcl_proxy_helper=>code_lines(
          FOR parameter IN i_method-parameters
          WHERE ( parm_kind = cl_abap_objectdescr=>importing )
          ( generate_parameter_definition( i_method = i_method i_parameter = parameter ) )
      ) TO r_result.
    ENDIF.
    IF line_exists( i_method-parameters[ parm_kind = cl_abap_objectdescr=>exporting ] ).
      APPEND 'EXPORTING' TO r_result.
      APPEND LINES OF VALUE zcl_proxy_helper=>code_lines(
          FOR parameter IN i_method-parameters
          WHERE ( parm_kind = cl_abap_objectdescr=>exporting )
          ( generate_parameter_definition( i_method = i_method i_parameter = parameter ) )
      ) TO r_result.
    ENDIF.
    IF line_exists( i_method-parameters[ parm_kind = cl_abap_objectdescr=>changing ] ).
      APPEND 'CHANGING' TO r_result.
      APPEND LINES OF VALUE zcl_proxy_helper=>code_lines(
          FOR parameter IN i_method-parameters
          WHERE ( parm_kind = cl_abap_objectdescr=>changing )
          ( generate_parameter_definition( i_method = i_method i_parameter = parameter ) )
      ) TO r_result.
    ENDIF.
    IF line_exists( i_method-parameters[ parm_kind = cl_abap_objectdescr=>returning ] ).
      APPEND 'RETURNIG' TO r_result.
      APPEND LINES OF VALUE zcl_proxy_helper=>code_lines(
          FOR parameter IN i_method-parameters
          WHERE ( parm_kind = cl_abap_objectdescr=>returning )
          ( generate_parameter_definition( i_method = i_method i_parameter = parameter ) )
      ) TO r_result.
    ENDIF.
  ENDMETHOD.


  METHOD generate_calling_parameters.
    IF line_exists( i_method-parameters[ parm_kind = cl_abap_objectdescr=>importing ] ).
      APPEND 'EXPORTING' TO r_result.
      APPEND LINES OF VALUE zcl_proxy_helper=>code_lines(
          FOR parameter IN i_method-parameters
          WHERE ( parm_kind = cl_abap_objectdescr=>importing )
          ( generate_parameter_calling( i_parameter = parameter i_var_name = CONV #( parameter-name ) ) )
      ) TO r_result.
    ENDIF.
    IF line_exists( i_method-parameters[ parm_kind = cl_abap_objectdescr=>exporting ] ).
      APPEND 'IMPORTING' TO r_result.
      APPEND LINES OF VALUE zcl_proxy_helper=>code_lines(
          FOR parameter IN i_method-parameters
          WHERE ( parm_kind = cl_abap_objectdescr=>importing )
          ( generate_parameter_calling( i_parameter = parameter i_var_name = CONV #( parameter-name ) ) )
      ) TO r_result.
    ENDIF.
    IF line_exists( i_method-parameters[ parm_kind = cl_abap_objectdescr=>changing ] ).
      APPEND 'CHANGING' TO r_result.
      APPEND LINES OF VALUE zcl_proxy_helper=>code_lines(
          FOR parameter IN i_method-parameters
          WHERE ( parm_kind = cl_abap_objectdescr=>changing )
          ( generate_parameter_calling( i_parameter = parameter i_var_name = CONV #( parameter-name ) ) )
      ) TO r_result.
    ENDIF.
    IF line_exists( i_method-parameters[ parm_kind = cl_abap_objectdescr=>returning ] ).
      APPEND 'RECEIVING' TO r_result.
      APPEND LINES OF VALUE zcl_proxy_helper=>code_lines(
          FOR parameter IN i_method-parameters
          WHERE ( parm_kind = cl_abap_objectdescr=>returning )
          ( generate_parameter_calling( i_parameter = parameter i_var_name = CONV #( parameter-name ) ) )
      ) TO r_result.
    ENDIF.
  ENDMETHOD.


  METHOD generate_parameter_definition.
    TRY.
        DATA(parameter_type) = cl_oo_object=>get_instance( CONV #( me->super_class->get_relative_name( ) ) )->get_parameter_type( cpdname = i_method-name sconame = i_parameter-name ).
      CATCH cx_class_error INTO DATA(exception).
        RAISE EXCEPTION TYPE zcx_proxy
          EXPORTING
            previous = exception.
    ENDTRY.
    r_result = SWITCH string( i_parameter-by_value WHEN abap_true THEN 'VALUE(' ELSE '' ) &&
               i_parameter-name &&
               SWITCH string( i_parameter-by_value WHEN abap_true THEN ')' ELSE '' ) &&
               SWITCH string( i_parameter-type_kind
                WHEN cl_abap_typedescr=>typekind_oref OR cl_abap_typedescr=>typekind_dref THEN | type ref to |
                ELSE | type |
               ) &&
               parameter_type-type &&
               SWITCH string( i_parameter-is_optional WHEN abap_true THEN | optional| ELSE '' ).

  ENDMETHOD.


  METHOD generate_parameter_calling.
    r_result = |{ i_parameter-name } = { i_var_name }|.
  ENDMETHOD.

ENDCLASS.
