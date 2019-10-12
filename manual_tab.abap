*&---------------------------------------------------------------------*
*& Report  ZRRE_MANUAL_TABCHANGE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zrre_manual_tab.

TABLES: sscrfields.

DATA:gv_objtype TYPE objtype.
DATA:gt_selfields LIKE TABLE OF se16n_seltab WITH HEADER LINE.
DATA:gv_edit TYPE boole_d.
DATA:gs_user TYPE zrre_d_tc_cfg.

PARAMETERS:
  p_tname LIKE dd02l-tabname,
  p_maxl LIKE sy-tabix DEFAULT '10',
  p_tech TYPE char1 AS CHECKBOX,
  p_lines TYPE char1 AS CHECKBOX.

SELECTION-SCREEN FUNCTION KEY 1.

INITIALIZATION.
  sscrfields-functxt_01 = text-fc1.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'P_TNAME'.
      screen-required = 2.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'FC01'.
      CASE sy-mandt.
        WHEN '800'.
          IF sy-uname EQ 'WANGJUAN'.
            CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
              EXPORTING
                action    = 'U'
                view_name = 'ZRRE_D_TC_CFG'.
          ELSE.
            MESSAGE '请联系五矿地产运维组增加权限' TYPE 'S'.
          ENDIF.
        WHEN OTHERS.
          CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
            EXPORTING
              action    = 'U'
              view_name = 'ZRRE_D_TC_CFG'.
      ENDCASE.
    WHEN OTHERS.
      IF p_tname IS NOT INITIAL.
        SELECT SINGLE * INTO gs_user FROM zrre_d_tc_cfg WHERE uname = sy-uname
                                                          AND begin_date <= sy-datum
                                                          AND end_date >= sy-datum.

        IF sy-subrc = 0.
          gv_edit = gs_user-is_edit.
          IF ( ( p_tname+0(4) = 'ZRRE' OR p_tname+0(1) = 'Y' OR p_tname+0(5) = '/RER/'
                 OR p_tname = 'ZTDIREQ01'
                 OR p_tname = 'ZTDILOG01'
                 OR ( sy-uname = 'SHIYH' AND p_tname = 'BKPF' )
              )
             AND sy-mandt = '800'
             AND gv_edit EQ abap_true )
          OR sy-mandt <> '800'
          OR gv_edit NE abap_true.

            CALL FUNCTION 'INTERN_DD_TABL_TYPE'
              EXPORTING
                objname              = p_tname
                objstate             = 'M'
              IMPORTING
                objtype              = gv_objtype
              EXCEPTIONS
                object_not_found     = 1
                object_not_specified = 2
                OTHERS               = 3.
            IF sy-subrc <> 0.
              MESSAGE '输入的表不存在' TYPE 'E'.
            ELSE.
              IF gv_objtype <> 'T' AND gv_objtype <> 'V'.
                MESSAGE '输入的表或视图' TYPE 'E'.
              ENDIF.
            ENDIF.
          ELSE.
            MESSAGE '非地产项目自定义表需申请权限' TYPE 'E'.
          ENDIF.
        ELSE.
          MESSAGE '请联系五矿地产运维组增加权限' TYPE 'E'.
        ENDIF.
      ELSE.
        MESSAGE '表名必输' TYPE 'E'.
      ENDIF.
  ENDCASE.

START-OF-SELECTION.
  PERFORM call_selection_cond.

*&---------------------------------------------------------------------*
*&      Form  CALL_SELECTION_COND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_selection_cond .
  DATA selid          TYPE  rsdynsel-selid.
  DATA: field_tab     TYPE TABLE OF rsdsfields,
        lw_field      TYPE rsdsfields.
  DATA: table_tab     TYPE TABLE OF rsdstabs.
  DATA table          LIKE LINE OF table_tab.
  DATA:field_ranges   TYPE  rsds_trange,
        lw_rsds_r TYPE rsds_range,
        lw_rsds TYPE rsds_frange,
        lw_selopt TYPE rsdsselopt.
  table-prim_tab = p_tname.
  APPEND table TO table_tab.

  DATA : ref_table_des TYPE REF TO cl_abap_structdescr.
  DATA : idetails TYPE abap_compdescr_tab,
         xdetails TYPE abap_compdescr.
  ref_table_des ?= cl_abap_tabledescr=>describe_by_name( p_tname ).
  idetails[] = ref_table_des->components[].
  DELETE idetails WHERE type_kind = 'y'.
  LOOP AT idetails INTO xdetails FROM 2 TO 10.
    lw_field-tablename = p_tname.
    lw_field-fieldname = xdetails-name.
    APPEND lw_field TO field_tab.
  ENDLOOP.

  CALL FUNCTION 'FREE_SELECTIONS_INIT'
    EXPORTING
      kind         = gv_objtype
    IMPORTING
      selection_id = selid
    TABLES
      tables_tab   = table_tab
      fields_tab   = field_tab
    EXCEPTIONS
      OTHERS       = 4.

  CALL FUNCTION 'FREE_SELECTIONS_DIALOG'
    EXPORTING
      selection_id = selid
      title        = '选择条件'
      as_window    = ' '
    IMPORTING
      field_ranges = field_ranges
    TABLES
      fields_tab   = field_tab
    EXCEPTIONS
      OTHERS       = 4.
  IF sy-subrc = 0.
    LOOP AT field_ranges INTO lw_rsds_r.
      LOOP AT lw_rsds_r-frange_t INTO lw_rsds.
        gt_selfields-field = lw_rsds-fieldname.
        LOOP AT lw_rsds-selopt_t INTO lw_selopt.
          gt_selfields-sign = lw_selopt-sign.
          gt_selfields-option = lw_selopt-option.
          gt_selfields-low = lw_selopt-low.
          gt_selfields-high = lw_selopt-high.
          APPEND gt_selfields.
        ENDLOOP.
        CLEAR gt_selfields.
      ENDLOOP.
    ENDLOOP.
    CASE p_lines.
      WHEN abap_true.
        PERFORM get_tablines.
      WHEN OTHERS.
        PERFORM call_tabchange.
    ENDCASE.
  ENDIF.
ENDFORM.                    " CALL_SELECTION_COND

*&---------------------------------------------------------------------*
*&      Form  ZTABCHANGE_INTERFACE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_tabchange .
  DATA: i_tab TYPE se16n_tab.
  DATA:pv_lines TYPE i.
  i_tab = p_tname.
  CALL FUNCTION 'ZTABCHANGE_INTERFACE'
    EXPORTING
      i_tab        = i_tab
      i_edit       = gv_edit
      i_sapedit    = gv_edit
      i_max_lines  = p_maxl
      i_tech_names = p_tech
    IMPORTING
      e_line_nr    = pv_lines
    TABLES
      it_selfields = gt_selfields[]
    EXCEPTIONS
      no_values    = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.                    " CALL_SE16N
*&---------------------------------------------------------------------*
*&      Form  get_tablines
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_tablines .
  DATA: i_tab TYPE se16n_tab.
  DATA:pv_lines TYPE i.
  i_tab = p_tname.
  CALL FUNCTION 'ZTABCHANGE_INTERFACE'
    EXPORTING
      i_tab        = i_tab
*     i_edit       = gv_edit
*     i_sapedit    = gv_edit
      i_line_det   = abap_true
      i_max_lines  = p_maxl
      i_no_txt     = abap_true
      i_tech_names = p_tech
    IMPORTING
      e_line_nr    = pv_lines
    TABLES
      it_selfields = gt_selfields[]
    EXCEPTIONS
      no_values    = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.                    " CALL_SE16N