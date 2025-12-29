# Technical Specification: Z_SCWM_MPN_GROUP_MATERIALS

## Document Information

| Field | Value |
|-------|-------|
| **Document Type** | Technical Specification (TS) |
| **Function Module** | `Z_SCWM_MPN_GROUP_MATERIALS` |
| **Function Group** | `ZSCWM_MPN` |
| **Version** | 1.0 |
| **Date** | 2025-01-XX |
| **Author** | [To be filled] |
| **Status** | Draft for Review |
| **Related Documents** | FS_Z_SCWM_MPN_GROUP_MATERIALS.md |

---

## 1. Technical Overview

### 1.1 Purpose
This technical specification details the implementation of function module `Z_SCWM_MPN_GROUP_MATERIALS` that groups dispatch materials based on configurable criteria to enable multiple Material Pickup Notes (MPNs) per vehicle.

### 1.2 Technical Architecture
- **Function Type**: Remote-Enabled Function Module (RFC)
- **Calling System**: SAP EWM (Extended Warehouse Management)
- **Integration Point**: Called from `Z_FIORI_SWM_OB_TRK_PRINT_MPN` after material collection, before SmartForm generation
- **Execution Mode**: Synchronous, single dialog step
- **Language**: ABAP

### 1.3 Technical Constraints
- Maximum execution time: < 2 seconds for 100 materials, < 5 seconds for 500 materials
- Single dialog step execution (no background processing)
- No external system calls
- Memory-efficient processing (avoid unnecessary table copies)

---

## 2. Data Dictionary Structures

### 2.1 Custom Table: ZSCWM_MPN_CONFIG

**Table Type**: Transparent Table (Client-dependent)

**Technical Name**: `ZSCWM_MPN_CONFIG`

**Delivery Class**: `A` (Application table)

**Fields**:

| Field Name | Data Element | Data Type | Length | Key | Description |
|------------|--------------|-----------|--------|-----|-------------|
| MANDT | MANDT | CLNT | 3 | X | Client |
| CONFIG_ID | ZSCWM_MPN_CONFIG_ID | CHAR | 32 | X | Configuration ID (Primary Key) |
| CONFIG_LEVEL | ZSCWM_MPN_CONFIG_LEVEL | CHAR | 10 | | Configuration Level (GLOBAL, SITE, PLANT, DIVISION, MATERIAL) |
| LEVEL_VALUE | ZSCWM_MPN_LEVEL_VALUE | CHAR | 40 | | Level Value (Site code, Plant code, Division code, Material number) |
| GROUPING_CRITERIA | ZSCWM_MPN_GROUP_CRIT | CHAR | 200 | | Grouping Criteria (Comma-separated: WH, M, PC, D) |
| EFFECTIVE_FROM_DATE | ZSCWM_MPN_EFF_FROM | DATS | 8 | | Effective From Date |
| EFFECTIVE_TO_DATE | ZSCWM_MPN_EFF_TO | DATS | 8 | | Effective To Date |
| IS_ACTIVE | ZSCWM_MPN_ACTIVE | CHAR | 1 | | Active Flag (X = Active, ' ' = Inactive) |
| CREATED_BY | ZSCWM_MPN_CREATED_BY | SYUNAME | 12 | | Created By User |
| CREATED_DATE | ZSCWM_MPN_CREATED_DATE | DATS | 8 | | Created Date |
| CREATED_TIME | ZSCWM_MPN_CREATED_TIME | TIMS | 6 | | Created Time |
| MODIFIED_BY | ZSCWM_MPN_MODIFIED_BY | SYUNAME | 12 | | Modified By User |
| MODIFIED_DATE | ZSCWM_MPN_MODIFIED_DATE | DATS | 8 | | Modified Date |
| MODIFIED_TIME | ZSCWM_MPN_MODIFIED_TIME | TIMS | 6 | | Modified Time |

**Indexes**:
- **Primary Index**: `MANDT`, `CONFIG_ID`
- **Secondary Index 1** (for configuration lookup):
  - `CONFIG_LEVEL`
  - `LEVEL_VALUE`
  - `IS_ACTIVE`
  - `EFFECTIVE_FROM_DATE`
  - `EFFECTIVE_TO_DATE`

**Table Maintenance**:
- Table Maintenance Generator: Yes
- Authorization Group: `ZSCWM_MPN`
- Delivery and Maintenance: Customer-specific

### 2.2 Custom Data Types

#### 2.2.1 Structure: ZSCWM_MPN_GROUP_ST

```abap
TYPES: BEGIN OF zscwm_mpn_group_st,
  group_id            TYPE char20,              " Unique group identifier
  group_sequence      TYPE i,                   " Sequence number
  group_criteria_value TYPE char200,            " Grouping criteria value
  group_criteria_desc  TYPE char200,            " Grouping criteria description
  it_ordim_o          TYPE zewm_ordim_o_tt,    " Materials in group
  it_db_proci_o       TYPE zewm_db_proci_o1_tt, " Product info for group
  group_summary       TYPE zscwm_mpn_group_summary_st, " Group summary
END OF zscwm_mpn_group_st.
```

#### 2.2.2 Table Type: ZSCWM_MPN_GROUP_TT

```abap
TYPES: zscwm_mpn_group_tt TYPE STANDARD TABLE OF zscwm_mpn_group_st
       WITH DEFAULT KEY.
```

#### 2.2.3 Structure: ZSCWM_MPN_GROUP_SUMMARY_ST

```abap
TYPES: BEGIN OF zscwm_mpn_group_summary_st,
  total_materials     TYPE i,                  " Number of materials
  total_quantity      TYPE quan,                " Total quantity (base unit)
  total_quantity_alt  TYPE quan,                " Total quantity (alt unit)
  warehouse_code      TYPE /scwm/de_who,        " Warehouse code
  material_number     TYPE matnr,               " Material number
  plant_code          TYPE werks_d,             " Plant code
  division_code       TYPE spart,               " Division code
END OF zscwm_mpn_group_summary_st.
```

#### 2.2.4 Structure: ZSCWM_MPN_CONFIG_ST

```abap
TYPES: BEGIN OF zscwm_mpn_config_st,
  config_id           TYPE char32,
  config_level        TYPE char10,
  level_value         TYPE char40,
  grouping_criteria   TYPE char200,
  effective_from_date TYPE dats,
  effective_to_date   TYPE dats,
  is_active           TYPE char1,
  created_by          TYPE syuname,
  created_date        TYPE dats,
  created_time        TYPE tims,
  modified_by         TYPE syuname,
  modified_date       TYPE dats,
  modified_time       TYPE tims,
END OF zscwm_mpn_config_st.
```

#### 2.2.5 Internal Structure: LTY_MATERIAL_MASTER

```abap
TYPES: BEGIN OF lty_material_master,
  matnr    TYPE matnr,              " Material number
  matid    TYPE /sapapo/matid,      " Material GUID (22-byte)
  werks    TYPE werks_d,            " Plant code
  spart    TYPE spart,              " Division
  matkl    TYPE matkl,              " Material group
END OF lty_material_master.

TYPES: lty_material_master_tt TYPE STANDARD TABLE OF lty_material_master
       WITH DEFAULT KEY.
```

#### 2.2.6 Internal Structure: LTY_GROUP_KEY

```abap
TYPES: BEGIN OF lty_group_key,
  group_key        TYPE string,                  " Concatenated group key
  warehouse_code   TYPE /scwm/de_who,            " Warehouse code
  material_number  TYPE matnr,                  " Material number
  plant_code       TYPE werks_d,                " Plant code
  division_code    TYPE spart,                   " Division code
END OF lty_group_key.

TYPES: lty_group_key_tt TYPE STANDARD TABLE OF lty_group_key
       WITH DEFAULT KEY.
```

#### 2.2.7 Internal Structure: LTY_GROUP_INTERNAL

```abap
TYPES: BEGIN OF lty_group_internal,
  group_key        TYPE string,                  " Group key
  group_index      TYPE i,                       " Internal index
  materials        TYPE zewm_ordim_o_tt,         " Materials in group
  product_info     TYPE zewm_db_proci_o1_tt,     " Product info
END OF lty_group_internal.

TYPES: lty_group_internal_tt TYPE STANDARD TABLE OF lty_group_internal
       WITH DEFAULT KEY.
```

---

## 3. Function Module Interface

### 3.1 Import Parameters

| Parameter | Type | Pass Value | Required | Description |
|-----------|------|------------|----------|-------------|
| IV_LGNUM | `/SCWM/LGNUM` | Value | Yes | Warehouse number |
| IV_VEH_NUM | `/SCWM/DE_VEH_NUM` | Value | Yes | Vehicle number |
| IV_REPORT_NO | `YREPORT_NO` | Value | Yes | Report number / Vehicle external number |
| IT_ORDIM_O | `ZEWM_ORDIM_O_TT` | Value | Yes | Order items table |
| IT_DB_PROCI_O | `ZEWM_DB_PROCI_O1_TT` | Value | No | Product information table |
| IV_DISPATCH_DATE | `SYDATUM` | Value | No | Dispatch date (default: SY-DATUM) |
| IV_USE_CONFIG | `ABAP_BOOL` | Value | No | Use configuration flag (default: 'X') |

### 3.2 Export Parameters

| Parameter | Type | Pass Value | Description |
|-----------|------|------------|-------------|
| ET_GROUPED_MATERIALS | `ZSCWM_MPN_GROUP_TT` | Value | Grouped materials table |
| EV_GROUP_COUNT | `I` | Value | Number of groups created |
| EV_CONFIG_ID | `CHAR32` | Value | Configuration ID applied |
| EV_CONFIG_LEVEL | `CHAR10` | Value | Configuration level applied |
| EV_GROUPING_CRITERIA | `CHAR200` | Value | Grouping criteria applied |

### 3.3 Table Parameters

| Parameter | Type | Pass Value | Description |
|-----------|------|------------|-------------|
| ET_RETURN | `BAPIRET2_T` | Value | Return messages table |

### 3.4 Exceptions

| Exception | Description |
|-----------|-------------|
| OTHERS | Catch-all exception for unexpected errors |

---

## 4. Algorithm Design

### 4.1 Main Processing Flow

```
┌─────────────────────────────────────────────────────────────┐
│                    FUNCTION ENTRY                            │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│  STEP 1: Initialize and Validate Inputs                     │
│  - Clear output parameters                                   │
│  - Validate IV_LGNUM in /SCWM/T300                          │
│  - Validate IV_VEH_NUM in /SCWM/VEHICLE                     │
│  - Validate IT_ORDIM_O is not empty                         │
│  - If error: Populate ET_RETURN and EXIT                     │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│  STEP 2: Determine Configuration                            │
│  - Check IV_USE_CONFIG flag                                  │
│  - If ' ': Skip to Step 6 (Default)                         │
│  - Read configuration using hierarchy:                       │
│    Material > Division > Plant > Site > Global              │
│  - Apply date range and active flag filters                  │
│  - Select most recent if multiple matches                    │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│  STEP 3: Read Master Data                                    │
│  - Extract unique material GUIDs from IT_ORDIM_O            │
│  - Convert GUIDs to material numbers                          │
│  - Read MARA, MARC, MVKE using FOR ALL ENTRIES               │
│  - Read warehouse master data                                │
│  - Cache in internal tables                                  │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│  STEP 4: Apply Grouping Logic                                │
│  - Parse GROUPING_CRITERIA (comma-separated)                 │
│  - Loop through IT_ORDIM_O:                                  │
│    * Extract criteria values                                │
│    * Build group key (concatenate values)                   │
│    * Handle missing values (use 'UNKNOWN')                   │
│    * Assign material to group                                │
│  - Validate groups (remove empty groups)                     │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│  STEP 5: Build Output Structure                              │
│  - Sort groups by group key                                  │
│  - Loop through groups:                                       │
│    * Generate GROUP_ID                                       │
│    * Calculate GROUP_SUMMARY                                 │
│    * Filter IT_DB_PROCI_O for group                          │
│    * Append to ET_GROUPED_MATERIALS                          │
│  - Set EV_GROUP_COUNT                                        │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│  STEP 6: Default Behavior (if no config)                    │
│  - Create single group with all materials                    │
│  - Set GROUP_ID = {VEH_NUM}-DEFAULT-001                      │
│  - Set EV_GROUPING_CRITERIA = 'DEFAULT'                      │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│  STEP 7: Finalize and Return                                │
│  - Sort ET_GROUPED_MATERIALS by GROUP_SEQUENCE               │
│  - Populate success message in ET_RETURN                     │
│  - Add warnings if any                                       │
│  - RETURN                                                     │
└─────────────────────────────────────────────────────────────┘
```

### 4.2 Configuration Lookup Algorithm

**Priority Order**: Material > Division > Plant > Site > Global

**Pseudo-code**:
```
FUNCTION get_configuration:
  lv_current_date = SY-DATUM
  lv_config_found = ABAP_FALSE
  
  * Try Material level
  IF material numbers available:
    SELECT config_id, config_level, grouping_criteria
      FROM zscwm_mpn_config
      INTO ls_config
      WHERE config_level = 'MATERIAL'
        AND level_value IN (material_numbers)
        AND is_active = 'X'
        AND effective_from_date <= lv_current_date
        AND (effective_to_date >= lv_current_date OR effective_to_date IS INITIAL)
      ORDER BY created_date DESCENDING
      UP TO 1 ROWS.
    ENDSELECT.
    IF sy-subrc = 0:
      lv_config_found = ABAP_TRUE
      RETURN ls_config
    ENDIF.
  ENDIF.
  
  * Try Division level
  IF divisions available:
    SELECT ... WHERE config_level = 'DIVISION' ...
    (similar logic)
  ENDIF.
  
  * Try Plant level
  IF plants available:
    SELECT ... WHERE config_level = 'PLANT' ...
    (similar logic)
  ENDIF.
  
  * Try Site level
  SELECT ... WHERE config_level = 'SITE' AND level_value = iv_lgnum ...
  (similar logic)
  
  * Try Global level
  SELECT ... WHERE config_level = 'GLOBAL' ...
  (similar logic)
  
  IF lv_config_found = ABAP_FALSE:
    RETURN initial (use default)
  ENDIF.
ENDFUNCTION.
```

### 4.3 Grouping Logic Algorithm

**Pseudo-code**:
```
FUNCTION apply_grouping:
  * Parse grouping criteria
  SPLIT ls_config-grouping_criteria AT ',' INTO TABLE lt_criteria.
  
  * Initialize group table
  CLEAR: lt_groups[].
  
  * Loop through materials
  LOOP AT it_ordim_o INTO ls_ordim_o.
    CLEAR: lv_group_key.
    
    * Extract criteria values
    LOOP AT lt_criteria INTO lv_criteria.
      CASE lv_criteria.
        WHEN 'WH':  " Warehouse
          lv_warehouse = get_warehouse_code(ls_ordim_o).
          CONCATENATE lv_group_key lv_warehouse INTO lv_group_key SEPARATED BY '-'.
          
        WHEN 'M':   " Material
          lv_matnr = get_material_number(ls_ordim_o-matid).
          CONCATENATE lv_group_key lv_matnr INTO lv_group_key SEPARATED BY '-'.
          
        WHEN 'PC':  " Plant Code
          lv_plant = get_plant_code(ls_ordim_o).
          IF lv_plant IS INITIAL:
            lv_plant = 'UNKNOWN'.
            log_warning('Missing plant code').
          ENDIF.
          CONCATENATE lv_group_key lv_plant INTO lv_group_key SEPARATED BY '-'.
          
        WHEN 'D':   " Division
          lv_division = get_division(ls_ordim_o).
          IF lv_division IS INITIAL:
            lv_division = 'UNKNOWN'.
            log_warning('Missing division').
          ENDIF.
          CONCATENATE lv_group_key lv_division INTO lv_group_key SEPARATED BY '-'.
      ENDCASE.
    ENDLOOP.
    
    * Assign material to group
    READ TABLE lt_groups WITH KEY group_key = lv_group_key INTO ls_group.
    IF sy-subrc <> 0:
      ls_group-group_key = lv_group_key.
      APPEND ls_group TO lt_groups.
    ENDIF.
    APPEND ls_ordim_o TO ls_group-materials.
    MODIFY lt_groups FROM ls_group.
  ENDLOOP.
  
  * Remove empty groups
  DELETE lt_groups WHERE materials IS INITIAL.
  
  RETURN lt_groups.
ENDFUNCTION.
```

### 4.4 Group ID Generation Algorithm

**Format**: `{VEH_NUM}-{GROUP_KEY}-{SEQUENCE}`

**Pseudo-code**:
```
FUNCTION generate_group_id:
  lv_veh_num_clean = iv_veh_num.
  REPLACE ALL OCCURRENCES OF '-' IN lv_veh_num_clean WITH ''.
  REPLACE ALL OCCURRENCES OF ' ' IN lv_veh_num_clean WITH ''.
  
  lv_group_key_clean = ls_group-group_key.
  REPLACE ALL OCCURRENCES OF '-' IN lv_group_key_clean WITH ''.
  REPLACE ALL OCCURRENCES OF ' ' IN lv_group_key_clean WITH ''.
  
  lv_sequence = |{ lv_sequence_number WIDTH = 3 PAD = '0' }|.
  
  CONCATENATE lv_veh_num_clean lv_group_key_clean lv_sequence
    INTO lv_group_id SEPARATED BY '-'.
  
  * Limit to 20 characters
  IF strlen(lv_group_id) > 20:
    lv_group_id = lv_group_id(20).
  ENDIF.
  
  RETURN lv_group_id.
ENDFUNCTION.
```

---

## 5. Code Structure

### 5.1 Function Module Skeleton

```abap
FUNCTION z_scwm_mpn_group_materials.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_LGNUM) TYPE /SCWM/LGNUM
*"     VALUE(IV_VEH_NUM) TYPE /SCWM/DE_VEH_NUM
*"     VALUE(IV_REPORT_NO) TYPE YREPORT_NO
*"     VALUE(IT_ORDIM_O) TYPE ZEWM_ORDIM_O_TT
*"     VALUE(IT_DB_PROCI_O) TYPE ZEWM_DB_PROCI_O1_TT OPTIONAL
*"     VALUE(IV_DISPATCH_DATE) TYPE SYDATUM OPTIONAL
*"     VALUE(IV_USE_CONFIG) TYPE ABAP_BOOL DEFAULT 'X'
*"  EXPORTING
*"     VALUE(ET_GROUPED_MATERIALS) TYPE ZSCWM_MPN_GROUP_TT
*"     VALUE(EV_GROUP_COUNT) TYPE I
*"     VALUE(EV_CONFIG_ID) TYPE CHAR32
*"     VALUE(EV_CONFIG_LEVEL) TYPE CHAR10
*"     VALUE(EV_GROUPING_CRITERIA) TYPE CHAR200
*"  TABLES
*"     ET_RETURN TYPE BAPIRET2_T
*"  EXCEPTIONS
*"     OTHERS
*"----------------------------------------------------------------------

  DATA: lv_dispatch_date TYPE dats,
        lv_use_config_flag TYPE abap_bool,
        ls_config TYPE zscwm_mpn_config_st,
        lt_material_master TYPE lty_material_master_tt,
        lt_groups TYPE lty_group_internal_tt,
        lv_group_count TYPE i,
        lv_error_flag TYPE abap_bool.

  *----------------------------------------------------------------------*
  * Initialize
  *----------------------------------------------------------------------*
  CLEAR: et_grouped_materials[],
         ev_group_count,
         ev_config_id,
         ev_config_level,
         ev_grouping_criteria,
         et_return[].

  * Set defaults
  IF iv_dispatch_date IS INITIAL.
    lv_dispatch_date = sy-datum.
  ELSE.
    lv_dispatch_date = iv_dispatch_date.
  ENDIF.

  IF iv_use_config IS INITIAL.
    lv_use_config_flag = abap_false.
  ELSE.
    lv_use_config_flag = iv_use_config.
  ENDIF.

  *----------------------------------------------------------------------*
  * Step 1: Validate Inputs
  *----------------------------------------------------------------------*
  PERFORM validate_inputs USING iv_lgnum
                                 iv_veh_num
                                 it_ordim_o
                        CHANGING lv_error_flag
                                 et_return[].

  IF lv_error_flag = abap_true.
    RETURN.
  ENDIF.

  *----------------------------------------------------------------------*
  * Step 2: Get Configuration
  *----------------------------------------------------------------------*
  IF lv_use_config_flag = abap_true.
    PERFORM get_configuration USING iv_lgnum
                                    it_ordim_o
                                    it_db_proci_o
                                    lv_dispatch_date
                           CHANGING ls_config
                                    ev_config_id
                                    ev_config_level
                                    ev_grouping_criteria
                                    et_return[].
  ENDIF.

  *----------------------------------------------------------------------*
  * Step 3: Read Master Data
  *----------------------------------------------------------------------*
  PERFORM read_master_data USING it_ordim_o
                        CHANGING lt_material_master
                                 et_return[].

  *----------------------------------------------------------------------*
  * Step 4 & 5: Apply Grouping and Build Output
  *----------------------------------------------------------------------*
  IF ls_config IS NOT INITIAL AND lv_use_config_flag = abap_true.
    PERFORM apply_grouping USING it_ordIM_o
                                 it_db_proci_o
                                 ls_config
                                 lt_material_master
                                 iv_veh_num
                        CHANGING lt_groups
                                 ev_group_count
                                 et_return[].
    
    PERFORM build_output USING lt_groups
                               iv_veh_num
                      CHANGING et_grouped_materials[]
                               ev_group_count.
  ELSE.
    * Default behavior: single group
    PERFORM create_default_group USING it_ordim_o
                                      it_db_proci_o
                                      iv_veh_num
                             CHANGING et_grouped_materials[]
                                      ev_group_count
                                      ev_grouping_criteria.
  ENDIF.

  *----------------------------------------------------------------------*
  * Step 7: Finalize
  *----------------------------------------------------------------------*
  PERFORM finalize USING ev_group_count
                CHANGING et_return[].

ENDFUNCTION.
```

### 5.2 Form Routines

#### 5.2.1 VALIDATE_INPUTS

```abap
FORM validate_inputs USING iv_lgnum TYPE /scwm/lgnum
                           iv_veh_num TYPE /scwm/de_veh_num
                           it_ordim_o TYPE zewm_ordim_o_tt
                  CHANGING ev_error_flag TYPE abap_bool
                           et_return TYPE bapiret2_t.

  DATA: lv_return TYPE bapiret2,
        lv_exists TYPE abap_bool.

  CLEAR: ev_error_flag.

  * Validate warehouse
  SELECT SINGLE lgnum FROM /scwm/t300
    INTO lv_lgnum
    WHERE lgnum = iv_lgnum.
  IF sy-subrc <> 0.
    ev_error_flag = abap_true.
    lv_return-type = 'E'.
    lv_return-id = 'ZSCWM_MPN'.
    lv_return-number = '001'.
    lv_return-message_v1 = iv_lgnum.
    MESSAGE ID lv_return-id TYPE lv_return-type NUMBER lv_return-number
      WITH lv_return-message_v1 INTO lv_return-message.
    APPEND lv_return TO et_return.
    RETURN.
  ENDIF.

  * Validate vehicle
  SELECT SINGLE veh_num FROM /scwm/vehicle
    INTO lv_veh_num
    WHERE veh_num = iv_veh_num.
  IF sy-subrc <> 0.
    ev_error_flag = abap_true.
    lv_return-type = 'E'.
    lv_return-id = 'ZSCWM_MPN'.
    lv_return-number = '002'.
    lv_return-message_v1 = iv_veh_num.
    MESSAGE ID lv_return-id TYPE lv_return-type NUMBER lv_return-number
      WITH lv_return-message_v1 INTO lv_return-message.
    APPEND lv_return TO et_return.
    RETURN.
  ENDIF.

  * Validate materials table
  IF it_ordim_o[] IS INITIAL.
    ev_error_flag = abap_true.
    lv_return-type = 'E'.
    lv_return-id = 'ZSCWM_MPN'.
    lv_return-number = '003'.
    MESSAGE ID lv_return-id TYPE lv_return-type NUMBER lv_return-number
      INTO lv_return-message.
    APPEND lv_return TO et_return.
    RETURN.
  ENDIF.

ENDFORM.
```

#### 5.2.2 GET_CONFIGURATION

```abap
FORM get_configuration USING iv_lgnum TYPE /scwm/lgnum
                            it_ordim_o TYPE zewm_ordim_o_tt
                            it_db_proci_o TYPE zewm_db_proci_o1_tt
                            iv_dispatch_date TYPE dats
                   CHANGING es_config TYPE zscwm_mpn_config_st
                            ev_config_id TYPE char32
                            ev_config_level TYPE char10
                            ev_grouping_criteria TYPE char200
                            et_return TYPE bapiret2_t.

  DATA: lt_material_numbers TYPE TABLE OF matnr,
        lt_divisions TYPE TABLE OF spart,
        lt_plants TYPE TABLE OF werks_d,
        ls_config_temp TYPE zscwm_mpn_config_st,
        lv_found TYPE abap_bool,
        lv_current_date TYPE dats.

  CLEAR: es_config, ev_config_id, ev_config_level, ev_grouping_criteria.
  lv_current_date = iv_dispatch_date.

  * Extract material numbers, divisions, plants from input
  PERFORM extract_lookup_values USING it_ordim_o
                                     it_db_proci_o
                            CHANGING lt_material_numbers
                                     lt_divisions
                                     lt_plants.

  * Try Material level
  IF lt_material_numbers[] IS NOT INITIAL.
    SELECT config_id config_level level_value grouping_criteria
           effective_from_date effective_to_date is_active created_date
      FROM zscwm_mpn_config
      INTO CORRESPONDING FIELDS OF ls_config_temp
      FOR ALL ENTRIES IN lt_material_numbers
      WHERE config_level = 'MATERIAL'
        AND level_value = lt_material_numbers-table_line
        AND is_active = 'X'
        AND effective_from_date <= lv_current_date
        AND (effective_to_date >= lv_current_date OR effective_to_date IS INITIAL)
      ORDER BY created_date DESCENDING
      UP TO 1 ROWS.
    ENDSELECT.
    IF sy-subrc = 0.
      es_config = ls_config_temp.
      ev_config_id = ls_config_temp-config_id.
      ev_config_level = ls_config_temp-config_level.
      ev_grouping_criteria = ls_config_temp-grouping_criteria.
      RETURN.
    ENDIF.
  ENDIF.

  * Try Division level
  IF lt_divisions[] IS NOT INITIAL.
    SELECT ... WHERE config_level = 'DIVISION' ...
    (similar logic)
    IF sy-subrc = 0.
      es_config = ls_config_temp.
      ...
      RETURN.
    ENDIF.
  ENDIF.

  * Try Plant level
  IF lt_plants[] IS NOT INITIAL.
    SELECT ... WHERE config_level = 'PLANT' ...
    (similar logic)
    IF sy-subrc = 0.
      es_config = ls_config_temp.
      ...
      RETURN.
    ENDIF.
  ENDIF.

  * Try Site level
  SELECT ... WHERE config_level = 'SITE' AND level_value = iv_lgnum ...
  (similar logic)
  IF sy-subrc = 0.
    es_config = ls_config_temp.
    ...
    RETURN.
  ENDIF.

  * Try Global level
  SELECT ... WHERE config_level = 'GLOBAL' ...
  (similar logic)
  IF sy-subrc = 0.
    es_config = ls_config_temp.
    ...
    RETURN.
  ENDIF.

ENDFORM.
```

#### 5.2.3 READ_MASTER_DATA

```abap
FORM read_master_data USING it_ordim_o TYPE zewm_ordim_o_tt
                   CHANGING et_material_master TYPE lty_material_master_tt
                            et_return TYPE bapiret2_t.

  DATA: lt_matid_22 TYPE TABLE OF /sapapo/matid,
        lt_matid_16 TYPE TABLE OF /scwm/de_matid,
        ls_matid_22 TYPE /sapapo/matid,
        ls_material_master TYPE lty_material_master,
        ls_return TYPE bapiret2.

  CLEAR: et_material_master[].

  * Extract unique material GUIDs
  LOOP AT it_ordim_o INTO DATA(ls_ordim_o).
    APPEND ls_ordim_o-matid TO lt_matid_16.
  ENDLOOP.
  SORT lt_matid_16.
  DELETE ADJACENT DUPLICATES FROM lt_matid_16.

  * Convert 16-byte GUIDs to 22-byte GUIDs
  LOOP AT lt_matid_16 INTO DATA(lv_matid_16).
    CLEAR ls_matid_22.
    CALL FUNCTION '/SAPAPO/INC_CONVERT_GUIDS'
      EXPORTING
        iv_guid16 = lv_matid_16
      IMPORTING
        ev_guid22 = ls_matid_22
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc = 0.
      APPEND ls_matid_22 TO lt_matid_22.
    ENDIF.
  ENDLOOP.

  * Read material master data
  IF lt_matid_22[] IS NOT INITIAL.
    SELECT matid matnr FROM /sapapo/mara
      INTO TABLE @DATA(lt_mara)
      FOR ALL ENTRIES IN @lt_matid_22
      WHERE matid = @lt_matid_22-table_line.

    IF sy-subrc = 0.
      * Read plant and division data
      SELECT matnr werks spart FROM marc
        INTO TABLE @DATA(lt_marc)
        FOR ALL ENTRIES IN @lt_mara
        WHERE matnr = @lt_mara-matnr.

      * Build material master table
      LOOP AT lt_mara INTO DATA(ls_mara).
        CLEAR ls_material_master.
        ls_material_master-matid = ls_mara-matid.
        ls_material_master-matnr = ls_mara-matnr.
        
        READ TABLE lt_marc INTO DATA(ls_marc)
          WITH KEY matnr = ls_mara-matnr.
        IF sy-subrc = 0.
          ls_material_master-werks = ls_marc-werks.
          ls_material_master-spart = ls_marc-spart.
        ENDIF.
        
        APPEND ls_material_master TO et_material_master.
      ENDLOOP.
    ELSE.
      * Log warning
      ls_return-type = 'W'.
      ls_return-id = 'ZSCWM_MPN'.
      ls_return-number = '008'.
      MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
        INTO ls_return-message.
      APPEND ls_return TO et_return.
    ENDIF.
  ENDIF.

ENDFORM.
```

#### 5.2.4 APPLY_GROUPING

```abap
FORM apply_grouping USING it_ordim_o TYPE zewm_ordim_o_tt
                         it_db_proci_o TYPE zewm_db_proci_o1_tt
                         is_config TYPE zscwm_mpn_config_st
                         it_material_master TYPE lty_material_master_tt
                         iv_veh_num TYPE /scwm/de_veh_num
                CHANGING et_groups TYPE lty_group_internal_tt
                         ev_group_count TYPE i
                         et_return TYPE bapiret2_t.

  DATA: lt_criteria TYPE TABLE OF string,
        ls_group TYPE lty_group_internal,
        lv_group_key TYPE string,
        lv_matnr TYPE matnr,
        lv_warehouse TYPE /scwm/de_who,
        lv_plant TYPE werks_d,
        lv_division TYPE spart,
        ls_return TYPE bapiret2,
        lv_index TYPE i.

  CLEAR: et_groups[], ev_group_count.

  * Parse grouping criteria
  SPLIT is_config-grouping_criteria AT ',' INTO TABLE lt_criteria.
  IF lt_criteria[] IS INITIAL.
    * Invalid configuration
    ls_return-type = 'E'.
    ls_return-id = 'ZSCWM_MPN'.
    ls_return-number = '004'.
    ls_return-message_v1 = is_config-config_id.
    MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
      WITH ls_return-message_v1 INTO ls_return-message.
    APPEND ls_return TO et_return.
    RETURN.
  ENDIF.

  * Loop through materials
  LOOP AT it_ordim_o INTO DATA(ls_ordim_o).
    CLEAR: lv_group_key, lv_matnr, lv_warehouse, lv_plant, lv_division.

    * Get material number
    READ TABLE it_material_master INTO DATA(ls_material_master)
      WITH KEY matid = ls_ordim_o-matid.
    IF sy-subrc = 0.
      lv_matnr = ls_material_master-matnr.
      lv_plant = ls_material_master-werks.
      lv_division = ls_material_master-spart.
    ENDIF.

    * Build group key based on criteria
    LOOP AT lt_criteria INTO DATA(lv_criteria).
      CASE lv_criteria.
        WHEN 'WH'.  " Warehouse
          lv_warehouse = get_warehouse_from_storage_bin(ls_ordim_o-vlpla).
          IF lv_warehouse IS INITIAL.
            lv_warehouse = 'UNKNOWN'.
            PERFORM log_warning USING lv_matnr 'Warehouse' CHANGING et_return[].
          ENDIF.
          CONCATENATE lv_group_key lv_warehouse INTO lv_group_key SEPARATED BY '-'.

        WHEN 'M'.   " Material
          IF lv_matnr IS INITIAL.
            lv_matnr = 'UNKNOWN'.
            PERFORM log_warning USING ' ' 'Material' CHANGING et_return[].
          ENDIF.
          CONCATENATE lv_group_key lv_matnr INTO lv_group_key SEPARATED BY '-'.

        WHEN 'PC'.  " Plant Code
          IF lv_plant IS INITIAL.
            * Try to get from IT_DB_PROCI_O
            READ TABLE it_db_proci_o INTO DATA(ls_proci_o)
              WITH KEY docid = ls_ordim_o-rdocid.
            IF sy-subrc = 0.
              lv_plant = ls_proci_o-stock_owner.
            ENDIF.
          ENDIF.
          IF lv_plant IS INITIAL.
            lv_plant = 'UNKNOWN'.
            PERFORM log_warning USING lv_matnr 'Plant Code' CHANGING et_return[].
          ENDIF.
          CONCATENATE lv_group_key lv_plant INTO lv_group_key SEPARATED BY '-'.

        WHEN 'D'.   " Division
          IF lv_division IS INITIAL.
            lv_division = 'UNKNOWN'.
            PERFORM log_warning USING lv_matnr 'Division' CHANGING et_return[].
          ENDIF.
          CONCATENATE lv_group_key lv_division INTO lv_group_key SEPARATED BY '-'.
      ENDCASE.
    ENDLOOP.

    * Assign material to group
    READ TABLE et_groups WITH KEY group_key = lv_group_key INTO ls_group.
    IF sy-subrc <> 0.
      CLEAR ls_group.
      ls_group-group_key = lv_group_key.
      lv_index = lv_index + 1.
      ls_group-group_index = lv_index.
      APPEND ls_group TO et_groups.
    ENDIF.
    APPEND ls_ordim_o TO ls_group-materials.
    MODIFY et_groups FROM ls_group.
  ENDLOOP.

  * Remove empty groups
  DELETE et_groups WHERE materials IS INITIAL.

  * Validate groups created
  IF et_groups[] IS INITIAL.
    ls_return-type = 'E'.
    ls_return-id = 'ZSCWM_MPN'.
    ls_return-number = '006'.
    MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
      INTO ls_return-message.
    APPEND ls_return TO et_return.
    RETURN.
  ENDIF.

  ev_group_count = lines( et_groups ).

ENDFORM.
```

#### 5.2.5 BUILD_OUTPUT

```abap
FORM build_output USING it_groups TYPE lty_group_internal_tt
                       iv_veh_num TYPE /scwm/de_veh_num
              CHANGING et_grouped_materials TYPE zscwm_mpn_group_tt
                       ev_group_count TYPE i.

  DATA: ls_grouped_material TYPE zscwm_mpn_group_st,
        ls_group TYPE lty_group_internal,
        lv_group_id TYPE char20,
        lv_sequence TYPE i,
        ls_summary TYPE zscwm_mpn_group_summary_st.

  CLEAR: et_grouped_materials[], ev_group_count.

  SORT it_groups BY group_key.

  LOOP AT it_groups INTO ls_group.
    CLEAR: ls_grouped_material, ls_summary.
    lv_sequence = lv_sequence + 1.

    * Generate group ID
    PERFORM generate_group_id USING iv_veh_num
                                    ls_group-group_key
                                    lv_sequence
                          CHANGING lv_group_id.

    * Build structure
    ls_grouped_material-group_id = lv_group_id.
    ls_grouped_material-group_sequence = lv_sequence.
    ls_grouped_material-group_criteria_value = ls_group-group_key.
    ls_grouped_material-it_ordim_o = ls_group-materials.
    ls_grouped_material-it_db_proci_o = ls_group-product_info.

    * Calculate summary
    PERFORM calculate_group_summary USING ls_group-materials
                                          ls_group-product_info
                                CHANGING ls_summary.

    ls_grouped_material-group_summary = ls_summary.

    APPEND ls_grouped_material TO et_grouped_materials.
  ENDLOOP.

  ev_group_count = lines( et_grouped_materials ).

ENDFORM.
```

#### 5.2.6 CREATE_DEFAULT_GROUP

```abap
FORM create_default_group USING it_ordim_o TYPE zewm_ordim_o_tt
                               it_db_proci_o TYPE zewm_db_proci_o1_tt
                               iv_veh_num TYPE /scwm/de_veh_num
                      CHANGING et_grouped_materials TYPE zscwm_mpn_group_tt
                               ev_group_count TYPE i
                               ev_grouping_criteria TYPE char200.

  DATA: ls_grouped_material TYPE zscwm_mpn_group_st,
        ls_summary TYPE zscwm_mpn_group_summary_st,
        lv_group_id TYPE char20.

  CLEAR: et_grouped_materials[], ev_group_count.

  * Generate default group ID
  CONCATENATE iv_veh_num 'DEFAULT' '001' INTO lv_group_id SEPARATED BY '-'.

  * Build structure
  ls_grouped_material-group_id = lv_group_id.
  ls_grouped_material-group_sequence = 1.
  ls_grouped_material-group_criteria_value = 'DEFAULT'.
  ls_grouped_material-group_criteria_desc = 'Default grouping (all materials)'.
  ls_grouped_material-it_ordim_o = it_ordim_o.
  ls_grouped_material-it_db_proci_o = it_db_proci_o.

  * Calculate summary
  PERFORM calculate_group_summary USING it_ordim_o
                                        it_db_proci_o
                              CHANGING ls_summary.

  ls_grouped_material-group_summary = ls_summary.

  APPEND ls_grouped_material TO et_grouped_materials.

  ev_group_count = 1.
  ev_grouping_criteria = 'DEFAULT'.

ENDFORM.
```

---

## 6. Database Access Patterns

### 6.1 Configuration Table Access

**Pattern**: Single SELECT with FOR ALL ENTRIES (when applicable)

**Optimization**:
- Use secondary index on (CONFIG_LEVEL, LEVEL_VALUE, IS_ACTIVE, EFFECTIVE_FROM_DATE, EFFECTIVE_TO_DATE)
- Use UP TO 1 ROWS for single record retrieval
- Order by CREATED_DATE DESCENDING to get most recent

**Example**:
```abap
SELECT config_id config_level level_value grouping_criteria
       effective_from_date effective_to_date is_active created_date
  FROM zscwm_mpn_config
  INTO CORRESPONDING FIELDS OF ls_config
  FOR ALL ENTRIES IN lt_material_numbers
  WHERE config_level = 'MATERIAL'
    AND level_value = lt_material_numbers-table_line
    AND is_active = 'X'
    AND effective_from_date <= sy-datum
    AND (effective_to_date >= sy-datum OR effective_to_date IS INITIAL)
  ORDER BY created_date DESCENDING
  UP TO 1 ROWS.
ENDSELECT.
```

### 6.2 Material Master Data Access

**Pattern**: FOR ALL ENTRIES with multiple table joins

**Optimization**:
- Read MARA first to get material numbers
- Then read MARC and MVKE using material numbers
- Cache results in internal tables

**Example**:
```abap
* First: Convert GUIDs and read MARA
SELECT matid matnr FROM /sapapo/mara
  INTO TABLE lt_mara
  FOR ALL ENTRIES IN lt_matid_22
  WHERE matid = lt_matid_22-table_line.

* Then: Read MARC for plant and division
SELECT matnr werks spart FROM marc
  INTO TABLE lt_marc
  FOR ALL ENTRIES IN lt_mara
  WHERE matnr = lt_mara-matnr.
```

### 6.3 Warehouse and Vehicle Validation

**Pattern**: Single SELECT with UP TO 1 ROWS

**Example**:
```abap
SELECT SINGLE lgnum FROM /scwm/t300
  INTO lv_lgnum
  WHERE lgnum = iv_lgnum.
```

---

## 7. Error Handling

### 7.1 Error Handling Strategy

1. **Validation Errors**: Exit immediately, populate ET_RETURN with error
2. **Configuration Errors**: Log error, use default behavior
3. **Data Errors**: Log warning, continue with available data
4. **System Errors**: Log error, use default behavior

### 7.2 Message Class: ZSCWM_MPN

| Number | Type | Text | Variables |
|--------|------|------|-----------|
| 001 | E | Warehouse number &1 is invalid | &1 = Warehouse number |
| 002 | E | Vehicle number &1 is invalid | &1 = Vehicle number |
| 003 | E | No materials provided for grouping | - |
| 004 | E | Invalid configuration &1 found | &1 = Config ID |
| 005 | W | Material &1 missing &2 value for grouping | &1 = Material, &2 = Field |
| 006 | E | No groups created after grouping | - |
| 007 | E | Error reading configuration table | - |
| 008 | W | Error reading material master data | - |
| 009 | S | Materials grouped successfully. &1 MPN(s) to be generated. | &1 = Count |

### 7.3 Error Handling Implementation

```abap
* Error handling pattern
FORM handle_error USING iv_message_id TYPE symsgid
                        iv_message_number TYPE symsgno
                        iv_message_type TYPE symsgty
                        iv_message_v1 TYPE any OPTIONAL
                        iv_message_v2 TYPE any OPTIONAL
                        iv_message_v3 TYPE any OPTIONAL
                        iv_message_v4 TYPE any OPTIONAL
              CHANGING et_return TYPE bapiret2_t.

  DATA: ls_return TYPE bapiret2.

  CLEAR ls_return.
  ls_return-type = iv_message_type.
  ls_return-id = iv_message_id.
  ls_return-number = iv_message_number.

  IF iv_message_v1 IS SUPPLIED.
    ls_return-message_v1 = iv_message_v1.
  ENDIF.
  IF iv_message_v2 IS SUPPLIED.
    ls_return-message_v2 = iv_message_v2.
  ENDIF.
  IF iv_message_v3 IS SUPPLIED.
    ls_return-message_v3 = iv_message_v3.
  ENDIF.
  IF iv_message_v4 IS SUPPLIED.
    ls_return-message_v4 = iv_message_v4.
  ENDIF.

  MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
    WITH ls_return-message_v1
         ls_return-message_v2
         ls_return-message_v3
         ls_return-message_v4
    INTO ls_return-message.

  APPEND ls_return TO et_return.

ENDFORM.
```

---

## 8. Performance Optimization

### 8.1 Database Optimization

1. **Indexes on ZSCWM_MPN_CONFIG**:
   - Primary index: MANDT, CONFIG_ID
   - Secondary index: CONFIG_LEVEL, LEVEL_VALUE, IS_ACTIVE, EFFECTIVE_FROM_DATE, EFFECTIVE_TO_DATE

2. **Query Optimization**:
   - Use FOR ALL ENTRIES instead of nested loops
   - Use UP TO 1 ROWS for single record retrieval
   - Sort internal tables before FOR ALL ENTRIES
   - Delete duplicates before FOR ALL ENTRIES

3. **Master Data Caching**:
   - Read all required master data in single pass
   - Cache in internal tables with sorted keys
   - Use binary search for lookups

### 8.2 Memory Optimization

1. **Table Operations**:
   - Use FREE instead of CLEAR for large tables
   - Avoid unnecessary table copies
   - Use field symbols for large structures

2. **String Operations**:
   - Use CONCATENATE efficiently
   - Limit string lengths where possible

### 8.3 Algorithm Optimization

1. **Grouping Logic**:
   - Build group key once per material
   - Use sorted tables for group lookups
   - Remove empty groups immediately

2. **Loop Optimization**:
   - Minimize nested loops
   - Use binary search for table lookups
   - Process in batches where possible

---

## 9. Testing Strategy

### 9.1 Unit Testing

**Test Cases** (refer to FS Section "Tests"):

1. **UT-001**: Warehouse-wise grouping (happy path)
2. **UT-002**: Material-wise grouping
3. **UT-003**: Combined criteria grouping
4. **UT-004**: No configuration (default behavior)
5. **UT-005**: Missing grouping criteria values
6. **UT-006**: Invalid warehouse number
7. **UT-007**: Empty material list
8. **UT-008**: Configuration priority
9. **UT-009**: All materials in same group
10. **UT-010**: Performance test (100 materials)

### 9.2 Integration Testing

**Test Scenarios**:

1. **IT-001**: Integration with Z_FIORI_SWM_OB_TRK_PRINT_MPN
   - Verify function call at correct location
   - Verify multiple MPN generation
   - Verify filename generation

2. **IT-002**: SmartForm integration
   - Verify SmartForm receives correct grouped data
   - Verify multiple PDF generation

3. **IT-003**: Error handling integration
   - Verify error messages propagate correctly
   - Verify default behavior on errors

### 9.3 Performance Testing

**Test Scenarios**:

1. **PT-001**: 100 materials, 10 warehouses
   - Expected: < 2 seconds

2. **PT-002**: 500 materials, 50 warehouses
   - Expected: < 5 seconds

3. **PT-003**: 1000 materials, 100 warehouses
   - Expected: < 10 seconds (if acceptable)

---

## 10. Implementation Checklist

### 10.1 Pre-Implementation

- [ ] Create message class ZSCWM_MPN with all messages
- [ ] Create custom table ZSCWM_MPN_CONFIG
- [ ] Create secondary index on ZSCWM_MPN_CONFIG
- [ ] Create data elements for all custom fields
- [ ] Create structure types: ZSCWM_MPN_GROUP_ST, ZSCWM_MPN_GROUP_SUMMARY_ST
- [ ] Create table types: ZSCWM_MPN_GROUP_TT
- [ ] Create function group ZSCWM_MPN
- [ ] Create authorization object Z_SCWM_MPN

### 10.2 Implementation

- [ ] Create function module Z_SCWM_MPN_GROUP_MATERIALS
- [ ] Implement form routine: VALIDATE_INPUTS
- [ ] Implement form routine: GET_CONFIGURATION
- [ ] Implement form routine: READ_MASTER_DATA
- [ ] Implement form routine: APPLY_GROUPING
- [ ] Implement form routine: BUILD_OUTPUT
- [ ] Implement form routine: CREATE_DEFAULT_GROUP
- [ ] Implement form routine: GENERATE_GROUP_ID
- [ ] Implement form routine: CALCULATE_GROUP_SUMMARY
- [ ] Implement form routine: FINALIZE
- [ ] Implement helper routines: GET_WAREHOUSE_FROM_STORAGE_BIN, LOG_WARNING, etc.

### 10.3 Integration

- [ ] Modify Z_FIORI_SWM_OB_TRK_PRINT_MPN at line 1812
- [ ] Add data declarations for grouping
- [ ] Add function call with proper error handling
- [ ] Add loop for multiple MPN generation
- [ ] Update filename generation logic
- [ ] Test integration end-to-end

### 10.4 Testing

- [ ] Unit test all form routines
- [ ] Integration test with main program
- [ ] Performance test with large datasets
- [ ] User acceptance testing
- [ ] Regression testing

### 10.5 Documentation

- [ ] Update function module documentation
- [ ] Create user guide for configuration
- [ ] Update technical documentation
- [ ] Create test documentation

---

## 11. Code Review Checklist

### 11.1 Code Quality

- [ ] No hardcoded values (use constants)
- [ ] Proper error handling for all database operations
- [ ] All internal tables properly cleared/freed
- [ ] No memory leaks
- [ ] Proper use of field symbols where appropriate
- [ ] Efficient database queries (FOR ALL ENTRIES, proper indexes)
- [ ] Proper sorting before binary search
- [ ] No unnecessary loops

### 11.2 ABAP Best Practices

- [ ] Use modern ABAP syntax (inline declarations, etc.)
- [ ] Proper use of data types
- [ ] Meaningful variable names
- [ ] Proper comments and documentation
- [ ] No SELECT * statements
- [ ] Proper exception handling
- [ ] No deprecated statements

### 11.3 Performance

- [ ] Database queries optimized
- [ ] Internal table operations efficient
- [ ] No unnecessary table copies
- [ ] Proper use of SORT and DELETE ADJACENT DUPLICATES
- [ ] Memory usage within limits

---

## 12. Deployment Plan

### 12.1 Development System

1. Create all DDIC objects
2. Implement function module
3. Unit testing
4. Code review

### 12.2 Quality Assurance System

1. Transport all objects
2. Integration testing
3. Performance testing
4. User acceptance testing

### 12.3 Production System

1. Transport all objects
2. Create initial configuration data
3. Monitor initial runs
4. User training

---

## 13. Maintenance Considerations

### 13.1 Configuration Management

- Configuration table should be maintained via customizing transaction
- Regular review of active configurations
- Archive old configurations

### 13.2 Performance Monitoring

- Monitor execution times
- Review database access patterns
- Optimize if performance degrades

### 13.3 Error Monitoring

- Monitor error messages in ET_RETURN
- Review warnings for missing data
- Update master data if needed

---

## 14. Appendix

### 14.1 Constants

```abap
CONSTANTS: gc_config_level_material TYPE char10 VALUE 'MATERIAL',
           gc_config_level_division   TYPE char10 VALUE 'DIVISION',
           gc_config_level_plant      TYPE char10 VALUE 'PLANT',
           gc_config_level_site       TYPE char10 VALUE 'SITE',
           gc_config_level_global     TYPE char10 VALUE 'GLOBAL',
           gc_criteria_warehouse      TYPE char2  VALUE 'WH',
           gc_criteria_material       TYPE char1  VALUE 'M',
           gc_criteria_plant          TYPE char2  VALUE 'PC',
           gc_criteria_division       TYPE char1  VALUE 'D',
           gc_active                  TYPE char1  VALUE 'X',
           gc_inactive                TYPE char1  VALUE ' ',
           gc_unknown                 TYPE char7  VALUE 'UNKNOWN',
           gc_default                 TYPE char7  VALUE 'DEFAULT',
           gc_separator               TYPE char1  VALUE '-'.
```

### 14.2 Helper Functions

#### GET_WAREHOUSE_FROM_STORAGE_BIN

```abap
FORM get_warehouse_from_storage_bin USING iv_vlpla TYPE /scwm/ltap_vlpla
                                 CHANGING ev_warehouse TYPE /scwm/de_who.

  * Extract warehouse code from storage bin
  * Logic depends on storage bin format
  * Example: If format is W001-BIN-001, extract W001
  
  DATA: lv_vlpla TYPE string.
  lv_vlpla = iv_vlpla.
  
  * Extract first part before first separator
  SPLIT lv_vlpla AT '-' INTO ev_warehouse DATA(lv_rest).
  
  * If no separator, use first few characters
  IF ev_warehouse IS INITIAL.
    ev_warehouse = iv_vlpla(4).
  ENDIF.

ENDFORM.
```

#### LOG_WARNING

```abap
FORM log_warning USING iv_matnr TYPE matnr
                       iv_field TYPE string
              CHANGING et_return TYPE bapiret2_t.

  DATA: ls_return TYPE bapiret2.

  CLEAR ls_return.
  ls_return-type = 'W'.
  ls_return-id = 'ZSCWM_MPN'.
  ls_return-number = '005'.
  ls_return-message_v1 = iv_matnr.
  ls_return-message_v2 = iv_field.
  
  MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
    WITH ls_return-message_v1 ls_return-message_v2
    INTO ls_return-message.
  
  APPEND ls_return TO et_return.

ENDFORM.
```

#### CALCULATE_GROUP_SUMMARY

```abap
FORM calculate_group_summary USING it_ordim_o TYPE zewm_ordim_o_tt
                                  it_db_proci_o TYPE zewm_db_proci_o1_tt
                        CHANGING es_summary TYPE zscwm_mpn_group_summary_st.

  DATA: lv_total_qty TYPE quan,
        lv_total_qty_alt TYPE quan,
        lv_count TYPE i.

  CLEAR: es_summary.

  * Count materials and calculate totals
  LOOP AT it_ordim_o INTO DATA(ls_ordim_o).
    lv_count = lv_count + 1.
    lv_total_qty = lv_total_qty + ls_ordim_o-vsolm.
    lv_total_qty_alt = lv_total_qty_alt + ls_ordim_o-vsola.
    
    * Extract key values from first material
    IF lv_count = 1.
      * Get warehouse, material, plant, division from first entry
      * (Implementation depends on grouping criteria)
    ENDIF.
  ENDLOOP.

  es_summary-total_materials = lv_count.
  es_summary-total_quantity = lv_total_qty.
  es_summary-total_quantity_alt = lv_total_qty_alt.

ENDFORM.
```

---

## Revision History

| Version | Date | Author | Description |
|---------|------|--------|------------|
| 1.0 | 2025-01-XX | [To be filled] | Initial TS creation based on FS |

---

**Document Status**: Draft for Review

**Next Steps**:
1. Review and approve TS
2. Create DDIC objects
3. Implement function module
4. Unit testing
5. Integration testing

