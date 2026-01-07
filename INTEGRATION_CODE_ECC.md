# Integration Code for Z_SCWM_MPN_GROUP_MATERIALS in Z_FIORI_SWM_OB_TRK_PRINT_MPN

## Integration Point

**Location**: After line 1826 (after `gt_ordim_o_tt` is fully populated) and before line 1868 (SmartForm call)

**Best Location**: After line 1866 (after product code logic) and before line 1868 (SmartForm call)

---

## Step 1: Add Data Declarations

Add the following data declarations in the DATA section (around line 320, after existing declarations):

```abap
*----------------------------------------------------------------------*
* Data declarations for MPN Grouping Function Module
*----------------------------------------------------------------------*
DATA: lt_grouped_materials TYPE zscwm_mpn_group_tt,
      lw_group TYPE zscwm_mpn_group_st,
      lv_group_count TYPE i,
      lv_config_id TYPE char32,
      lv_config_level TYPE char10,
      lv_grouping_criteria TYPE char200,
      lt_return_group TYPE bapiret2_t,
      lw_return_group TYPE bapiret2,
      lv_use_config TYPE abap_bool VALUE 'X',
      lv_mpn_sequence TYPE i,
      lv_xstring_temp TYPE xstring,
      lv_filename_temp TYPE /bofu/field_name,
      lv_group_id_temp TYPE char20,
      lv_has_grouping_error TYPE abap_bool.
```

---

## Step 2: Insert Integration Code

**Insert the following code AFTER line 1866** (after the product code logic block ends with `ENDIF.`) and **BEFORE line 1868** (before `CALL FUNCTION gw_fm_name`):

```abap
*----------------------------------------------------------------------*
* Call grouping function module for configurable MPN grouping
* Added for Warehouse-wise MPN grouping functionality
*----------------------------------------------------------------------*
CLEAR: lt_grouped_materials[],
       lv_group_count,
       lv_config_id,
       lv_config_level,
       lv_grouping_criteria,
       lt_return_group[],
       lv_has_grouping_error.

* Check if configurable grouping is enabled and materials exist
IF lv_use_config = abap_true AND gt_ordim_o_tt[] IS NOT INITIAL.
  
  CALL FUNCTION 'Z_SCWM_MPN_GROUP_MATERIALS'
    EXPORTING
      iv_lgnum          = i_lgnum
      iv_veh_num        = gw_veh_num
      iv_report_no      = i_report_no
      it_ordim_o        = gt_ordim_o_tt
      it_db_proci_o     = gt_db_proci_o
      it_db_proch_o     = gt_db_proch_o
      it_db_refdoc      = gt_db_refdoc
      it_it_waveitm     = gt_it_waveitm
      it_but000         = gt_but000
      it_but020         = gt_but020
      it_adrc           = gt_adrc
      it_t005u          = gt_t005u
      it_plant          = gt_plant
      it_lnumt          = gt_lnumt
      it_delv_st        = gt_delv_st
      iv_dispatch_date  = sy-datum
      iv_use_config     = lv_use_config
    IMPORTING
      et_grouped_materials = lt_grouped_materials
      ev_group_count       = lv_group_count
      ev_config_id         = lv_config_id
      ev_config_level      = lv_config_level
      ev_grouping_criteria = lv_grouping_criteria
    TABLES
      et_return            = lt_return_group
    EXCEPTIONS
      OTHERS               = 99.

  IF sy-subrc = 0.
    * Check for errors in grouping
    CLEAR lv_has_grouping_error.
    LOOP AT lt_return_group INTO lw_return_group WHERE type = 'E'.
      lv_has_grouping_error = abap_true.
      EXIT.
    ENDLOOP.

    IF lv_has_grouping_error = abap_true.
      * Error in grouping, use default behavior (single MPN)
      APPEND LINES OF lt_return_group TO et_return.
      CLEAR: lt_grouped_materials[], lv_group_count.
    ELSE.
      * Grouping successful - append info messages
      APPEND LINES OF lt_return_group TO et_return.
      
      * If multiple groups, generate MPN for each group
      IF lv_group_count > 1.
        CLEAR: lv_mpn_sequence, e_xstring, e_filename.
        
        LOOP AT lt_grouped_materials INTO lw_group.
          lv_mpn_sequence = lv_mpn_sequence + 1.
          
          * Prepare data for SmartForm from grouped data
          CLEAR: gt_ordim_o_tt[], gt_db_proci_o[], gt_db_proch_o[],
                 gt_db_refdoc[], gt_it_waveitm[], gt_but000[],
                 gt_but020[], gt_adrc[], gt_t005u[], gt_plant[],
                 gt_lnumt[], gt_delv_st[].
          
          gt_ordim_o_tt = lw_group-it_ordim_o.
          gt_db_proci_o = lw_group-it_db_proci_o.
          gt_db_proch_o = lw_group-it_db_proch_o.
          gt_db_refdoc = lw_group-it_db_refdoc.
          gt_it_waveitm = lw_group-it_it_waveitm.
          gt_but000 = lw_group-it_but000.
          gt_but020 = lw_group-it_but020.
          gt_adrc = lw_group-it_adrc.
          gt_t005u = lw_group-it_t005u.
          gt_plant = lw_group-it_plant.
          gt_lnumt = lw_group-it_lnumt.
          gt_delv_st = lw_group-it_delv_st.
          
          * Update output filename with group identifier
          CLEAR lv_group_id_temp.
          lv_group_id_temp = lw_group-group_id.
          CONCATENATE i_report_no lv_group_id_temp 'pdf'
            INTO lv_filename_temp SEPARATED BY '.'.
          
          * Set control parameters for SmartForm
          CLEAR: gw_control_parameters, gw_output.
          gw_control_parameters-no_dialog = 'X'.
          gw_control_parameters-getotf = abap_true.
          gw_control_parameters-preview = space.
          gw_control_parameters-langu = sy-langu.
          gw_output-tdprinter = 'PDF1'.
          gw_output-tdnewid = 'X'.
          gw_output-tddelete = space.
          gw_output-tdsuffix1 = i_lgnum.
          gw_output-tdsuffix2 = i_report_no.
          
          * Call SmartForm for this group
          CLEAR: gt_otf_data_temp, gw_binsize.
          CALL FUNCTION gw_fm_name
            EXPORTING
              control_parameters = gw_control_parameters
              output_options     = gw_output
              user_settings      = gc_x
              who                = i_lgnum
              whohu              = gt_whohu
              sr_act             = gw_tu_sr_act
              gw_lnumt           = gw_lnumt
              gw_but0001         = gw_but0001
              gw_duct            = gw_duct
              im_polyster_flag   = lw_polyster_flag
            IMPORTING
              job_output_info    = gt_otf_data_temp
              gw_nlber           = gw_nlber
            TABLES
              gt_db_proci_o      = gt_db_proci_o
              gt_db_proch_o      = gt_db_proch_o
              gt_t005u           = gt_t005u
              gt_adrc            = gt_adrc
              gt_ordim_o         = gt_ordim_o_tt
              gt_db_refdoc       = gt_db_refdoc
              gt_it_waveitm      = gt_it_waveitm
              gt_but000          = gt_but000
              gt_but020          = gt_but020
              gt_plant           = gt_plant
              gt_lnumt           = gt_lnumt
              gt_delv_st         = gt_delv_st
            EXCEPTIONS
              formatting_error   = 1
              internal_error     = 2
              send_error         = 3
              user_canceled      = 4
              OTHERS             = 5.

          IF sy-subrc = 0 AND NOT gt_otf_data_temp-otfdata IS INITIAL.
            * Convert OTF to PDF
            CLEAR: lv_xstring_temp, gw_binsize, gt_lines[].
            CALL FUNCTION 'CONVERT_OTF'
              EXPORTING
                format                = 'PDF'
              IMPORTING
                bin_filesize          = gw_binsize
                bin_file              = lv_xstring_temp
              TABLES
                otf                   = gt_otf_data_temp-otfdata
                lines                 = gt_lines
              EXCEPTIONS
                err_max_linewidth     = 1
                err_format            = 2
                err_conv_not_possible = 3
                err_bad_otf           = 4
                OTHERS                = 5.

            IF sy-subrc = 0.
              * For multiple MPNs, use last group's PDF
              * Option: Can be enhanced to merge PDFs or return multiple files
              e_xstring = lv_xstring_temp.
              e_filename = lv_filename_temp.
              e_mimetype = 'application/pdf'.
            ELSE.
              * Error in PDF conversion
              CLEAR lw_return.
              lw_return-type = 'E'.
              lw_return-id = 'ZSCWM_MPN'.
              lw_return-number = '010'.
              lw_return-message_v1 = lw_group-group_id.
              MESSAGE ID lw_return-id TYPE lw_return-type NUMBER lw_return-number
                WITH lw_return-message_v1 INTO lw_return-message.
              APPEND lw_return TO et_return.
            ENDIF.
          ELSE.
            * Error in SmartForm call
            CLEAR lw_return.
            lw_return-type = 'E'.
            lw_return-id = 'ZSCWM_MPN'.
            lw_return-number = '011'.
            lw_return-message_v1 = lw_group-group_id.
            MESSAGE ID lw_return-id TYPE lw_return-type NUMBER lw_return-number
              WITH lw_return-message_v1 INTO lw_return-message.
            APPEND lw_return TO et_return.
          ENDIF.
        ENDLOOP.
        
        * Set final output and exit
        IF e_xstring IS INITIAL.
          CLEAR lw_return.
          lw_return-type = 'E'.
          lw_return-message = text-014.
          APPEND lw_return TO et_return.
        ENDIF.
        
        * Exit after processing all groups
        RETURN.
        
      ELSE.
        * Single group - use grouped data and continue with existing logic
        IF lt_grouped_materials[] IS NOT INITIAL.
          READ TABLE lt_grouped_materials INTO lw_group INDEX 1.
          IF sy-subrc = 0.
            * Assign filtered data from first group
            gt_ordim_o_tt = lw_group-it_ordim_o.
            gt_db_proci_o = lw_group-it_db_proci_o.
            gt_db_proch_o = lw_group-it_db_proch_o.
            gt_db_refdoc = lw_group-it_db_refdoc.
            gt_it_waveitm = lw_group-it_it_waveitm.
            gt_but000 = lw_group-it_but000.
            gt_but020 = lw_group-it_but020.
            gt_adrc = lw_group-it_adrc.
            gt_t005u = lw_group-it_t005u.
            gt_plant = lw_group-it_plant.
            gt_lnumt = lw_group-it_lnumt.
            gt_delv_st = lw_group-it_delv_st.
          ENDIF.
        ENDIF.
        * Continue with existing SmartForm call below
      ENDIF.
    ENDIF.
  ELSE.
    * Error calling grouping function - use default behavior
    CLEAR lw_return.
    lw_return-type = 'W'.
    lw_return-id = 'ZSCWM_MPN'.
    lw_return-number = '012'.
    MESSAGE ID lw_return-id TYPE lw_return-type NUMBER lw_return-number
      INTO lw_return-message.
    APPEND lw_return TO et_return.
    * Continue with existing logic (single MPN)
  ENDIF.
ENDIF.

* If no configuration or grouping failed, continue with existing logic
* (Default: single MPN per vehicle - existing code continues from line 1868)
```

---

## Step 3: Modify Existing SmartForm Call (Optional Enhancement)

The existing SmartForm call at line 1868 will continue to work for single MPN scenarios. However, if you want to ensure it uses the grouped data when available, you can add a check before the call:

**Note**: This is optional since the code above already handles single group scenario by assigning filtered data to the tables before the existing SmartForm call continues.

---

## Complete Code Block Location Summary

1. **Data Declarations**: Add around line 320 (in DATA section)
2. **Integration Code**: Insert after line 1866, before line 1868
3. **Existing SmartForm Call**: Remains at line 1868 (will use grouped data if single group)

---

## Key Points for ECC Compatibility

1. **No Inline Declarations**: All variables declared explicitly in DATA section
2. **Traditional ABAP Syntax**: No inline declarations in LOOP statements
3. **Explicit CLEAR Statements**: All tables cleared before assignment
4. **Traditional Field Access**: No inline field symbols or inline assignments
5. **Explicit Type Declarations**: All types explicitly declared

---

## Error Handling

The integration code handles the following scenarios:

1. **Grouping Function Error**: Falls back to default behavior (single MPN)
2. **Multiple Groups**: Loops through each group and generates PDF
3. **Single Group**: Uses filtered data and continues with existing logic
4. **PDF Conversion Error**: Logs error and continues to next group
5. **SmartForm Error**: Logs error and continues to next group

---

## Testing Checklist

- [ ] Test with single group (EV_GROUP_COUNT = 1)
- [ ] Test with multiple groups (EV_GROUP_COUNT > 1)
- [ ] Test with grouping function error (should fallback to default)
- [ ] Test with empty materials table
- [ ] Test with missing optional tables
- [ ] Verify PDF generation for each group
- [ ] Verify filename includes group identifier
- [ ] Verify error messages are logged correctly

---

## Notes

1. **Multiple PDF Handling**: Currently returns last group's PDF. Can be enhanced to:
   - Merge all PDFs into single document
   - Return table of PDFs (requires interface change)
   - Save each PDF separately

2. **Performance**: For large number of groups, consider:
   - Background processing
   - Batch processing
   - PDF merging optimization

3. **Configuration Control**: The `lv_use_config` flag can be controlled via:
   - Customizing table
   - User parameter
   - System configuration

---

## Message Class Requirements

Ensure the following messages exist in message class `ZSCWM_MPN`:

- **010**: Error converting PDF for group &1
- **011**: Error in SmartForm call for group &1
- **012**: Warning: Grouping function call failed, using default behavior

---

**End of Integration Code**

