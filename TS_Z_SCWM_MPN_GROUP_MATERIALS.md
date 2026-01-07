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
- **Target System**: SAP EWM (Extended Warehouse Management) - Native EWM Implementation
- **Calling System**: SAP EWM (Extended Warehouse Management)
- **Integration Point**: Called from `Z_FIORI_SWM_OB_TRK_PRINT_MPN` after material collection, before SmartForm generation
- **Execution Mode**: Synchronous, single dialog step
- **Language**: ABAP
- **EWM Compatibility**: Optimized for EWM 9.0+ and S/4HANA EWM, uses EWM-native data structures and APIs

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
  it_db_proci_o       TYPE zewm_db_proci_o1_tt, " Product info for group (filtered)
  it_db_proch_o       TYPE zewm_db_proch_o_tt,  " Product header info (filtered)
  it_db_refdoc        TYPE zewm_db_ref_doc_tt,  " Reference documents (filtered)
  it_it_waveitm       TYPE zewm_waveitm_tt,     " Wave items (filtered)
  it_but000           TYPE zewm_but000_tt,      " Business partners (filtered)
  it_but020           TYPE zewm_but020_tt,      " Address assignments (filtered)
  it_adrc             TYPE zewm_adrc_tt,        " Address data (filtered)
  it_t005u            TYPE t005u_t,            " Region descriptions (filtered)
  it_plant            TYPE /scwm/t300_md_tt,   " Plant data (filtered)
  it_lnumt            TYPE /scwm/t300t_tt,     " Warehouse descriptions (filtered)
  it_delv_st          TYPE zlog_get_detail_tt, " Delivery details (filtered)
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
  matid    TYPE /scwm/de_matid,    " Material GUID (16-byte, EWM native)
  werks    TYPE werks_d,            " Plant code
  spart    TYPE spart,              " Division
  matkl    TYPE matkl,              " Material group (optional)
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
  docids           TYPE /scwm/tt_rdocid,          " Document IDs for filtering
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
| IT_DB_PROCH_O | `ZEWM_DB_PROCH_O_TT` | Value | No | Product header information table |
| IT_DB_REFDOC | `ZEWM_DB_REF_DOC_TT` | Value | No | Reference document table |
| IT_IT_WAVEITM | `ZEWM_WAVEITM_TT` | Value | No | Wave item table |
| IT_BUT000 | `ZEWM_BUT000_TT` | Value | No | Business partner general data |
| IT_BUT020 | `ZEWM_BUT020_TT` | Value | No | Business partner address assignment |
| IT_ADRC | `ZEWM_ADRC_TT` | Value | No | Address data |
| IT_T005U | `T005U_TT` | Value | No | Region/State descriptions |
| IT_PLANT | `/SCWM/T300_MD_TT` | Value | No | Warehouse plant data |
| IT_LNUMT | `/SCWM/T300T_TT` | Value | No | Warehouse descriptions |
| IT_DELV_ST | `ZLOG_GET_DETAIL_TT` | Value | No | Delivery detail data |
| IV_DISPATCH_DATE | `SYDATUM` | Value | No | Dispatch date (default: SY-DATUM) |
| IV_USE_CONFIG | `ABAP_BOOL` | Value | No | Use configuration flag (default: 'X') |

### 3.2 Export Parameters

| Parameter | Type | Pass Value | Description |
|-----------|------|------------|-------------|
| ET_GROUPED_MATERIALS | `ZSCWM_MPN_GROUP_TT` | Value | Grouped materials table (one entry per MPN group, contains all filtered data for SmartForm) |
| EV_GROUP_COUNT | `I` | Value | Number of groups created (**Multiple Print Indicator**: If > 1, multiple MPNs need to be generated) |
| EV_CONFIG_ID | `CHAR32` | Value | Configuration ID applied |
| EV_CONFIG_LEVEL | `CHAR10` | Value | Configuration level applied (GLOBAL, SITE, PLANT, DIVISION, MATERIAL) |
| EV_GROUPING_CRITERIA | `CHAR200` | Value | Grouping criteria applied (comma-separated: WH, M, PC, D) |

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
│  STEP 3: Read Master Data (EWM Native)                      │
│  - Extract unique material numbers from IT_DB_PROCI_O        │
│  - Extract material GUIDs from IT_ORDIM_O                    │
│  - Read /SCWM/MAT, /SCWM/MATPLANT, MVKE using FOR ALL ENTRIES│
│  - Map GUIDs to material numbers                             │
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
│    * Extract document IDs from group materials               │
│    * Filter all related tables for group:                   │
│      - IT_DB_PROCI_O (by docid)                             │
│      - IT_DB_PROCH_O (by docno from proci_o)                │
│      - IT_DB_REFDOC (by docid)                              │
│      - IT_IT_WAVEITM (by rdocid)                            │
│      - IT_BUT000, IT_BUT020, IT_ADRC, IT_T005U              │
│        (by partner/address from proch_o)                    │
│      - IT_PLANT, IT_LNUMT (by lgnum from materials)         │
│      - IT_DELV_ST (by deliveryno from refdoc)              │
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

### 5.1 Complete Function Module Code (No PERFORM Statements)

**Note**: This code is optimized for SAP EWM (Extended Warehouse Management) landscape. The code uses standard ABAP syntax compatible with:
- SAP EWM 9.0 and later
- SAP S/4HANA EWM
- SAP NetWeaver 7.40 and later

**EWM-Specific Implementation**:
- Uses EWM-native data structures (`/SCWM/*` tables and types)
- Accesses material master data via EWM product information tables
- No APO dependencies (removed `/SAPAPO/*` references)
- Compatible with EWM material GUID handling

```abap
*"----------------------------------------------------------------------
*"* EWM-Adapted Implementation
*"* Key Changes from ECC/APO Version:
*"* - Removed APO dependencies (/SAPAPO/MARA, /SAPAPO/INC_CONVERT_GUIDS)
*"* - Removed ECC dependencies (MARC table)
*"* - Uses EWM-native material master tables:
*"*   * /SCWM/MAT - EWM Material Master
*"*   * /SCWM/MATPLANT - EWM Material Plant data (for plant code)
*"*   * MVKE - Material Sales Data (for division, standard SAP used in EWM)
*"* - Uses EWM-native material data from IT_DB_PROCI_O (product information)
*"* - Material GUIDs handled via EWM-native /SCWM/DE_MATID type
*"* - Optimized for EWM 9.0+ and S/4HANA EWM landscapes
*"----------------------------------------------------------------------

FUNCTION z_scwm_mpn_group_materials.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_LGNUM) TYPE /SCWM/LGNUM
*"     VALUE(IV_VEH_NUM) TYPE /SCWM/DE_VEH_NUM
*"     VALUE(IV_REPORT_NO) TYPE YREPORT_NO
*"     VALUE(IT_ORDIM_O) TYPE ZEWM_ORDIM_O_TT
*"     VALUE(IT_DB_PROCI_O) TYPE ZEWM_DB_PROCI_O1_TT OPTIONAL
*"     VALUE(IT_DB_PROCH_O) TYPE ZEWM_DB_PROCH_O_TT OPTIONAL
*"     VALUE(IT_DB_REFDOC) TYPE ZEWM_DB_REF_DOC_TT OPTIONAL
*"     VALUE(IT_IT_WAVEITM) TYPE ZEWM_WAVEITM_TT OPTIONAL
*"     VALUE(IT_BUT000) TYPE ZEWM_BUT000_TT OPTIONAL
*"     VALUE(IT_BUT020) TYPE ZEWM_BUT020_TT OPTIONAL
*"     VALUE(IT_ADRC) TYPE ZEWM_ADRC_TT OPTIONAL
*"     VALUE(IT_T005U) TYPE T005U_T OPTIONAL
*"     VALUE(IT_PLANT) TYPE /SCWM/T300_MD_TT OPTIONAL
*"     VALUE(IT_LNUMT) TYPE /SCWM/T300T_TT OPTIONAL
*"     VALUE(IT_DELV_ST) TYPE ZLOG_GET_DETAIL_TT OPTIONAL
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

  *----------------------------------------------------------------------*
  * Local Data Declarations
  *----------------------------------------------------------------------*
  DATA: lv_dispatch_date TYPE dats,
        lv_use_config_flag TYPE abap_bool,
        ls_config TYPE zscwm_mpn_config_st,
        lt_material_master TYPE lty_material_master_tt,
        lt_groups TYPE lty_group_internal_tt,
        lv_group_count TYPE i,
        lv_error_flag TYPE abap_bool,
        lv_return TYPE bapiret2,
        lv_lgnum TYPE /scwm/lgnum,
        lv_veh_num TYPE /scwm/de_veh_num,
        lt_material_numbers TYPE TABLE OF matnr,
        lt_divisions TYPE TABLE OF spart,
        lt_plants TYPE TABLE OF werks_d,
        ls_config_temp TYPE zscwm_mpn_config_st,
        lv_current_date TYPE dats,
        lt_matid_16 TYPE TABLE OF /scwm/de_matid,
        ls_material_master TYPE lty_material_master,
        ls_return TYPE bapiret2,
        lt_criteria TYPE TABLE OF string,
        ls_group TYPE lty_group_internal,
        lv_group_key TYPE string,
        lv_matnr TYPE matnr,
        lv_warehouse TYPE /scwm/de_who,
        lv_plant TYPE werks_d,
        lv_division TYPE spart,
        lv_index TYPE i,
        ls_grouped_material TYPE zscwm_mpn_group_st,
        lv_group_id TYPE char20,
        lv_sequence TYPE i,
        ls_summary TYPE zscwm_mpn_group_summary_st,
        lt_docids TYPE /scwm/tt_rdocid,
        lt_docnos TYPE TABLE OF /scdl/dl_docno,
        lt_partners TYPE TABLE OF bu_partner,
        lt_addrnumbers TYPE TABLE OF ad_addrnum,
        lt_deliverynos TYPE TABLE OF vbeln_vl,
        lt_lgnums TYPE TABLE OF /scwm/lgnum,
        lv_veh_num_clean TYPE string,
        lv_group_key_clean TYPE string,
        lv_total_qty TYPE quan,
        lv_total_qty_alt TYPE quan,
        lv_count TYPE i,
        ls_ordim_o TYPE zewm_ordim_o_st,
        ls_proci_o TYPE zewm_db_proci_o1_st,
        ls_proch_o TYPE zewm_db_proch_o_st,
        ls_refdoc TYPE zewm_db_ref_doc_st,
        ls_waveitm TYPE zewm_waveitm_st,
        ls_but000 TYPE zewm_but000_st,
        ls_but020 TYPE zewm_but020_st,
        ls_adrc TYPE zewm_adrc_st,
        ls_t005u TYPE t005u,
        ls_plant TYPE /scwm/t300_md,
        ls_lnumt TYPE /scwm/t300t,
        ls_delv_st TYPE zlog_get_detail_st,
        ls_matplant TYPE /scwm/matplant,
        lt_matplant TYPE TABLE OF /scwm/matplant,
        ls_mat TYPE /scwm/mat,
        lt_mat TYPE TABLE OF /scwm/mat,
        ls_mvke TYPE mvke,
        lt_mvke TYPE TABLE OF mvke,
        lv_matid_16 TYPE /scwm/de_matid,
        lv_criteria TYPE string,
        ls_material_master_temp TYPE lty_material_master,
        lv_vlpla TYPE /scwm/ltap_vlpla,
        lt_country_region TYPE TABLE OF ty_country_region,
        ls_country_region TYPE ty_country_region.

  TYPES: BEGIN OF ty_country_region,
           country TYPE land1,
           region TYPE regio,
         END OF ty_country_region.

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
  CLEAR: lv_error_flag.

  * Validate warehouse
  SELECT SINGLE lgnum FROM /scwm/t300
    INTO lv_lgnum
    WHERE lgnum = iv_lgnum.
  IF sy-subrc <> 0.
    lv_error_flag = abap_true.
    CLEAR lv_return.
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
    lv_error_flag = abap_true.
    CLEAR lv_return.
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
    lv_error_flag = abap_true.
    CLEAR lv_return.
    lv_return-type = 'E'.
    lv_return-id = 'ZSCWM_MPN'.
    lv_return-number = '003'.
    MESSAGE ID lv_return-id TYPE lv_return-type NUMBER lv_return-number
      INTO lv_return-message.
    APPEND lv_return TO et_return.
    RETURN.
  ENDIF.

  IF lv_error_flag = abap_true.
    RETURN.
  ENDIF.

  *----------------------------------------------------------------------*
  * Step 2: Get Configuration
  *----------------------------------------------------------------------*
  CLEAR: ls_config, ev_config_id, ev_config_level, ev_grouping_criteria.
  lv_current_date = lv_dispatch_date.

  IF lv_use_config_flag = abap_true.
    * Extract material numbers, divisions, plants from input
    CLEAR: lt_material_numbers, lt_divisions, lt_plants.

    * Extract material numbers from IT_DB_PROCI_O
    IF it_db_proci_o[] IS NOT INITIAL.
      LOOP AT it_db_proci_o INTO ls_proci_o.
        IF ls_proci_o-productno IS NOT INITIAL.
          APPEND ls_proci_o-productno TO lt_material_numbers.
        ENDIF.
      ENDLOOP.
      SORT lt_material_numbers.
      DELETE ADJACENT DUPLICATES FROM lt_material_numbers.
    ENDIF.

    * Extract divisions and plants from material master (will be read later)
    * For now, we'll extract from IT_DB_PROCI_O if available
    IF it_db_proci_o[] IS NOT INITIAL.
      LOOP AT it_db_proci_o INTO ls_proci_o.
        IF ls_proci_o-stock_owner IS NOT INITIAL.
          APPEND ls_proci_o-stock_owner TO lt_plants.
        ENDIF.
      ENDLOOP.
      SORT lt_plants.
      DELETE ADJACENT DUPLICATES FROM lt_plants.
    ENDIF.

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
        ls_config = ls_config_temp.
        ev_config_id = ls_config_temp-config_id.
        ev_config_level = ls_config_temp-config_level.
        ev_grouping_criteria = ls_config_temp-grouping_criteria.
      ENDIF.
    ENDIF.

    * Try Division level (if material level not found)
    IF ls_config IS INITIAL AND lt_divisions[] IS NOT INITIAL.
      SELECT config_id config_level level_value grouping_criteria
             effective_from_date effective_to_date is_active created_date
        FROM zscwm_mpn_config
        INTO CORRESPONDING FIELDS OF ls_config_temp
        FOR ALL ENTRIES IN lt_divisions
        WHERE config_level = 'DIVISION'
          AND level_value = lt_divisions-table_line
          AND is_active = 'X'
          AND effective_from_date <= lv_current_date
          AND (effective_to_date >= lv_current_date OR effective_to_date IS INITIAL)
        ORDER BY created_date DESCENDING
        UP TO 1 ROWS.
      ENDSELECT.
      IF sy-subrc = 0.
        ls_config = ls_config_temp.
        ev_config_id = ls_config_temp-config_id.
        ev_config_level = ls_config_temp-config_level.
        ev_grouping_criteria = ls_config_temp-grouping_criteria.
      ENDIF.
    ENDIF.

    * Try Plant level (if division level not found)
    IF ls_config IS INITIAL AND lt_plants[] IS NOT INITIAL.
      SELECT config_id config_level level_value grouping_criteria
             effective_from_date effective_to_date is_active created_date
        FROM zscwm_mpn_config
        INTO CORRESPONDING FIELDS OF ls_config_temp
        FOR ALL ENTRIES IN lt_plants
        WHERE config_level = 'PLANT'
          AND level_value = lt_plants-table_line
          AND is_active = 'X'
          AND effective_from_date <= lv_current_date
          AND (effective_to_date >= lv_current_date OR effective_to_date IS INITIAL)
        ORDER BY created_date DESCENDING
        UP TO 1 ROWS.
      ENDSELECT.
      IF sy-subrc = 0.
        ls_config = ls_config_temp.
        ev_config_id = ls_config_temp-config_id.
        ev_config_level = ls_config_temp-config_level.
        ev_grouping_criteria = ls_config_temp-grouping_criteria.
      ENDIF.
    ENDIF.

    * Try Site level (if plant level not found)
    IF ls_config IS INITIAL.
      SELECT config_id config_level level_value grouping_criteria
             effective_from_date effective_to_date is_active created_date
        FROM zscwm_mpn_config
        INTO CORRESPONDING FIELDS OF ls_config_temp
        WHERE config_level = 'SITE'
          AND level_value = iv_lgnum
          AND is_active = 'X'
          AND effective_from_date <= lv_current_date
          AND (effective_to_date >= lv_current_date OR effective_to_date IS INITIAL)
        ORDER BY created_date DESCENDING
        UP TO 1 ROWS.
      ENDSELECT.
      IF sy-subrc = 0.
        ls_config = ls_config_temp.
        ev_config_id = ls_config_temp-config_id.
        ev_config_level = ls_config_temp-config_level.
        ev_grouping_criteria = ls_config_temp-grouping_criteria.
      ENDIF.
    ENDIF.

    * Try Global level (if site level not found)
    IF ls_config IS INITIAL.
      SELECT config_id config_level level_value grouping_criteria
             effective_from_date effective_to_date is_active created_date
        FROM zscwm_mpn_config
        INTO CORRESPONDING FIELDS OF ls_config_temp
        WHERE config_level = 'GLOBAL'
          AND is_active = 'X'
          AND effective_from_date <= lv_current_date
          AND (effective_to_date >= lv_current_date OR effective_to_date IS INITIAL)
        ORDER BY created_date DESCENDING
        UP TO 1 ROWS.
      ENDSELECT.
      IF sy-subrc = 0.
        ls_config = ls_config_temp.
        ev_config_id = ls_config_temp-config_id.
        ev_config_level = ls_config_temp-config_level.
        ev_grouping_criteria = ls_config_temp-grouping_criteria.
      ENDIF.
    ENDIF.
  ENDIF.

  *----------------------------------------------------------------------*
  * Step 3: Read Master Data (EWM Native)
  *----------------------------------------------------------------------*
  CLEAR: lt_material_master[].

  * Extract unique material numbers and GUIDs from input data
  CLEAR: lt_matid_16, lt_material_numbers.
  
  * First, try to get material numbers from IT_DB_PROCI_O (EWM product info)
  IF it_db_proci_o[] IS NOT INITIAL.
    LOOP AT it_db_proci_o INTO ls_proci_o.
      IF ls_proci_o-productno IS NOT INITIAL.
        APPEND ls_proci_o-productno TO lt_material_numbers.
      ENDIF.
    ENDLOOP.
    SORT lt_material_numbers.
    DELETE ADJACENT DUPLICATES FROM lt_material_numbers.
  ENDIF.

  * Also extract material GUIDs from IT_ORDIM_O for mapping
  LOOP AT it_ordim_o INTO ls_ordim_o.
    IF ls_ordim_o-matid IS NOT INITIAL.
      APPEND ls_ordim_o-matid TO lt_matid_16.
    ENDIF.
  ENDLOOP.
  SORT lt_matid_16.
  DELETE ADJACENT DUPLICATES FROM lt_matid_16.

  * Read material master data using EWM-native tables
  IF lt_material_numbers[] IS NOT INITIAL.
    CLEAR: lt_matplant, lt_mat, lt_mvke.
    
    * Read EWM Material Master data from /SCWM/MAT
    SELECT matnr FROM /scwm/mat
      INTO TABLE lt_mat
      FOR ALL ENTRIES IN lt_material_numbers
      WHERE matnr = lt_material_numbers-table_line.

    * Read EWM Material Plant data from /SCWM/MATPLANT (contains plant-specific data)
    SELECT matnr werks FROM /scwm/matplant
      INTO TABLE lt_matplant
      FOR ALL ENTRIES IN lt_material_numbers
      WHERE matnr = lt_material_numbers-table_line.

    * Read Material Sales Data (MVKE) for division information
    * Note: MVKE is standard SAP table but commonly used in EWM for sales/division data
    SELECT matnr vkorg vtweg spart FROM mvke
      INTO TABLE lt_mvke
      FOR ALL ENTRIES IN lt_material_numbers
      WHERE matnr = lt_material_numbers-table_line
        AND vkorg <> space
        AND vtweg <> space.

    IF sy-subrc = 0 OR lt_matplant[] IS NOT INITIAL OR lt_mat[] IS NOT INITIAL.
      * Build material master table with GUID mapping
      LOOP AT lt_material_numbers INTO lv_matnr.
        CLEAR ls_material_master.
        ls_material_master-matnr = lv_matnr.
        
        * Find corresponding GUID from IT_ORDIM_O or IT_DB_PROCI_O
        LOOP AT it_ordim_o INTO ls_ordim_o.
          * Try to match via IT_DB_PROCI_O if available
          IF it_db_proci_o[] IS NOT INITIAL.
            READ TABLE it_db_proci_o INTO ls_proci_o
              WITH KEY productno = lv_matnr.
            IF sy-subrc = 0.
              READ TABLE it_ordim_o INTO ls_ordim_o
                WITH KEY rdocid = ls_proci_o-docid.
              IF sy-subrc = 0 AND ls_ordim_o-matid IS NOT INITIAL.
                ls_material_master-matid = ls_ordim_o-matid.
                EXIT.
              ENDIF.
            ENDIF.
          ELSE.
            * Fallback: use first matching GUID (less precise)
            IF ls_ordim_o-matid IS NOT INITIAL.
              ls_material_master-matid = ls_ordim_o-matid.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.
        
        * Read plant from EWM Material Plant table (/SCWM/MATPLANT)
        READ TABLE lt_matplant INTO ls_matplant
          WITH KEY matnr = lv_matnr.
        IF sy-subrc = 0.
          ls_material_master-werks = ls_matplant-werks.
          IF ls_matplant-werks IS NOT INITIAL.
            APPEND ls_matplant-werks TO lt_plants.
          ENDIF.
        ELSE.
          * Try to get plant from IT_DB_PROCI_O if EWM table read failed
          IF it_db_proci_o[] IS NOT INITIAL.
            READ TABLE it_db_proci_o INTO ls_proci_o
              WITH KEY productno = lv_matnr.
            IF sy-subrc = 0 AND ls_proci_o-stock_owner IS NOT INITIAL.
              ls_material_master-werks = ls_proci_o-stock_owner.
              APPEND ls_proci_o-stock_owner TO lt_plants.
            ENDIF.
          ENDIF.
        ENDIF.
        
        * Read division from Material Sales Data (MVKE)
        * Use first sales organization/division combination found
        READ TABLE lt_mvke INTO ls_mvke
          WITH KEY matnr = lv_matnr.
        IF sy-subrc = 0 AND ls_mvke-spart IS NOT INITIAL.
          ls_material_master-spart = ls_mvke-spart.
          APPEND ls_mvke-spart TO lt_divisions.
        ELSE.
          * Try alternative: Read from IT_DB_PROCI_O if available
          IF it_db_proci_o[] IS NOT INITIAL.
            READ TABLE it_db_proci_o INTO ls_proci_o
              WITH KEY productno = lv_matnr.
            * Note: Division might not be in IT_DB_PROCI_O, so this is optional
          ENDIF.
        ENDIF.
        
        APPEND ls_material_master TO lt_material_master.
      ENDLOOP.
      
      SORT lt_material_master BY matid.
      SORT lt_divisions.
      DELETE ADJACENT DUPLICATES FROM lt_divisions.
      SORT lt_plants.
      DELETE ADJACENT DUPLICATES FROM lt_plants.
    ELSE.
      * If EWM table reads failed, build from IT_DB_PROCI_O data
      IF it_db_proci_o[] IS NOT INITIAL.
        LOOP AT it_db_proci_o INTO ls_proci_o.
          IF ls_proci_o-productno IS NOT INITIAL.
            CLEAR ls_material_master.
            ls_material_master-matnr = ls_proci_o-productno.
            
            * Find corresponding GUID
            LOOP AT it_ordim_o INTO ls_ordim_o.
              READ TABLE it_db_proci_o TRANSPORTING NO FIELDS
                WITH KEY docid = ls_ordim_o-rdocid
                         productno = ls_proci_o-productno.
              IF sy-subrc = 0 AND ls_ordim_o-matid IS NOT INITIAL.
                ls_material_master-matid = ls_ordim_o-matid.
                EXIT.
              ENDIF.
            ENDLOOP.
            
            IF ls_proci_o-stock_owner IS NOT INITIAL.
              ls_material_master-werks = ls_proci_o-stock_owner.
              APPEND ls_proci_o-stock_owner TO lt_plants.
            ENDIF.
            
            APPEND ls_material_master TO lt_material_master.
          ENDIF.
        ENDLOOP.
        SORT lt_material_master BY matid.
        DELETE ADJACENT DUPLICATES FROM lt_material_master.
        SORT lt_plants.
        DELETE ADJACENT DUPLICATES FROM lt_plants.
      ELSE.
        * Log warning
        CLEAR ls_return.
        ls_return-type = 'W'.
        ls_return-id = 'ZSCWM_MPN'.
        ls_return-number = '008'.
        MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
          INTO ls_return-message.
        APPEND ls_return TO et_return.
      ENDIF.
    ENDIF.
  ELSE.
    * Fallback: Build material master from GUIDs only (if material numbers not available)
    IF lt_matid_16[] IS NOT INITIAL.
      LOOP AT lt_matid_16 INTO lv_matid_16.
        CLEAR ls_material_master.
        ls_material_master-matid = lv_matid_16.
        * Material number will be empty, will be handled in grouping logic
        APPEND ls_material_master TO lt_material_master.
      ENDLOOP.
      SORT lt_material_master BY matid.
    ELSE.
      * Log warning
      CLEAR ls_return.
      ls_return-type = 'W'.
      ls_return-id = 'ZSCWM_MPN'.
      ls_return-number = '008'.
      MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
        INTO ls_return-message.
      APPEND ls_return TO et_return.
    ENDIF.
  ENDIF.

  *----------------------------------------------------------------------*
  * Step 4: Apply Grouping Logic
  *----------------------------------------------------------------------*
  CLEAR: lt_groups[], ev_group_count.

  IF ls_config IS NOT INITIAL AND lv_use_config_flag = abap_true.
    * Parse grouping criteria
    CLEAR: lt_criteria.
    SPLIT ls_config-grouping_criteria AT ',' INTO TABLE lt_criteria.
    IF lt_criteria[] IS INITIAL.
      * Invalid configuration
      CLEAR ls_return.
      ls_return-type = 'E'.
      ls_return-id = 'ZSCWM_MPN'.
      ls_return-number = '004'.
      ls_return-message_v1 = ls_config-config_id.
      MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
        WITH ls_return-message_v1 INTO ls_return-message.
      APPEND ls_return TO et_return.
    ELSE.
      * Loop through materials
      CLEAR: lv_index.
      LOOP AT it_ordim_o INTO ls_ordim_o.
        CLEAR: lv_group_key, lv_matnr, lv_warehouse, lv_plant, lv_division.

        * Get material number
        READ TABLE lt_material_master INTO ls_material_master_temp
          WITH KEY matid = ls_ordim_o-matid BINARY SEARCH.
        IF sy-subrc = 0.
          lv_matnr = ls_material_master_temp-matnr.
          lv_plant = ls_material_master_temp-werks.
          lv_division = ls_material_master_temp-spart.
        ENDIF.

        * Build group key based on criteria
        LOOP AT lt_criteria INTO lv_criteria.
          CASE lv_criteria.
            WHEN 'WH'.  " Warehouse
              * Extract warehouse from storage bin
              lv_vlpla = ls_ordim_o-vlpla.
              IF lv_vlpla IS NOT INITIAL.
                * Extract first part before separator
                SPLIT lv_vlpla AT '-' INTO lv_warehouse DATA(lv_rest).
                IF lv_warehouse IS INITIAL.
                  lv_warehouse = lv_vlpla(4).
                ENDIF.
              ENDIF.
              IF lv_warehouse IS INITIAL.
                lv_warehouse = 'UNKNOWN'.
                CLEAR ls_return.
                ls_return-type = 'W'.
                ls_return-id = 'ZSCWM_MPN'.
                ls_return-number = '005'.
                ls_return-message_v1 = lv_matnr.
                ls_return-message_v2 = 'Warehouse'.
                MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
                  WITH ls_return-message_v1 ls_return-message_v2 INTO ls_return-message.
                APPEND ls_return TO et_return.
              ENDIF.
              IF lv_group_key IS INITIAL.
                lv_group_key = lv_warehouse.
              ELSE.
                CONCATENATE lv_group_key lv_warehouse INTO lv_group_key SEPARATED BY '-'.
              ENDIF.

            WHEN 'M'.   " Material
              IF lv_matnr IS INITIAL.
                lv_matnr = 'UNKNOWN'.
                CLEAR ls_return.
                ls_return-type = 'W'.
                ls_return-id = 'ZSCWM_MPN'.
                ls_return-number = '005'.
                ls_return-message_v1 = ' '.
                ls_return-message_v2 = 'Material'.
                MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
                  WITH ls_return-message_v1 ls_return-message_v2 INTO ls_return-message.
                APPEND ls_return TO et_return.
              ENDIF.
              IF lv_group_key IS INITIAL.
                lv_group_key = lv_matnr.
              ELSE.
                CONCATENATE lv_group_key lv_matnr INTO lv_group_key SEPARATED BY '-'.
              ENDIF.

            WHEN 'PC'.  " Plant Code
              IF lv_plant IS INITIAL.
                * Try to get from IT_DB_PROCI_O
                READ TABLE it_db_proci_o INTO ls_proci_o
                  WITH KEY docid = ls_ordim_o-rdocid.
                IF sy-subrc = 0.
                  lv_plant = ls_proci_o-stock_owner.
                ENDIF.
              ENDIF.
              IF lv_plant IS INITIAL.
                lv_plant = 'UNKNOWN'.
                CLEAR ls_return.
                ls_return-type = 'W'.
                ls_return-id = 'ZSCWM_MPN'.
                ls_return-number = '005'.
                ls_return-message_v1 = lv_matnr.
                ls_return-message_v2 = 'Plant Code'.
                MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
                  WITH ls_return-message_v1 ls_return-message_v2 INTO ls_return-message.
                APPEND ls_return TO et_return.
              ENDIF.
              IF lv_group_key IS INITIAL.
                lv_group_key = lv_plant.
              ELSE.
                CONCATENATE lv_group_key lv_plant INTO lv_group_key SEPARATED BY '-'.
              ENDIF.

            WHEN 'D'.   " Division
              IF lv_division IS INITIAL.
                lv_division = 'UNKNOWN'.
                CLEAR ls_return.
                ls_return-type = 'W'.
                ls_return-id = 'ZSCWM_MPN'.
                ls_return-number = '005'.
                ls_return-message_v1 = lv_matnr.
                ls_return-message_v2 = 'Division'.
                MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
                  WITH ls_return-message_v1 ls_return-message_v2 INTO ls_return-message.
                APPEND ls_return TO et_return.
              ENDIF.
              IF lv_group_key IS INITIAL.
                lv_group_key = lv_division.
              ELSE.
                CONCATENATE lv_group_key lv_division INTO lv_group_key SEPARATED BY '-'.
              ENDIF.
          ENDCASE.
        ENDLOOP.

        * Assign material to group
        READ TABLE lt_groups INTO ls_group
          WITH KEY group_key = lv_group_key.
        IF sy-subrc <> 0.
          CLEAR ls_group.
          ls_group-group_key = lv_group_key.
          lv_index = lv_index + 1.
          ls_group-group_index = lv_index.
          APPEND ls_group TO lt_groups.
        ENDIF.
        APPEND ls_ordim_o TO ls_group-materials.
        MODIFY lt_groups FROM ls_group.
      ENDLOOP.

      * Remove empty groups
      DELETE lt_groups WHERE materials IS INITIAL.

      * Validate groups created
      IF lt_groups[] IS INITIAL.
        CLEAR ls_return.
        ls_return-type = 'E'.
        ls_return-id = 'ZSCWM_MPN'.
        ls_return-number = '006'.
        MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
          INTO ls_return-message.
        APPEND ls_return TO et_return.
      ELSE.
        ev_group_count = lines( lt_groups ).
      ENDIF.
    ENDIF.

    *----------------------------------------------------------------------*
    * Step 5: Build Output Structure
    *----------------------------------------------------------------------*
    IF lt_groups[] IS NOT INITIAL.
      CLEAR: et_grouped_materials[], ev_group_count.
      CLEAR: lv_sequence.

      SORT lt_groups BY group_key.

      LOOP AT lt_groups INTO ls_group.
        CLEAR: ls_grouped_material, ls_summary,
               lt_docids, lt_docnos, lt_partners, lt_addrnumbers,
               lt_deliverynos, lt_lgnums.

        lv_sequence = lv_sequence + 1.

        * Generate group ID
        CLEAR: lv_group_id, lv_veh_num_clean, lv_group_key_clean.
        lv_veh_num_clean = iv_veh_num.
        REPLACE ALL OCCURRENCES OF '-' IN lv_veh_num_clean WITH ''.
        REPLACE ALL OCCURRENCES OF ' ' IN lv_veh_num_clean WITH ''.

        lv_group_key_clean = ls_group-group_key.
        REPLACE ALL OCCURRENCES OF '-' IN lv_group_key_clean WITH ''.
        REPLACE ALL OCCURRENCES OF ' ' IN lv_group_key_clean WITH ''.

        CONCATENATE lv_veh_num_clean lv_group_key_clean lv_sequence
          INTO lv_group_id SEPARATED BY '-'.

        * Limit to 20 characters
        IF strlen( lv_group_id ) > 20.
          lv_group_id = lv_group_id(20).
        ENDIF.

        * Extract document IDs from group materials
        CLEAR: ls_ordim_o.
        LOOP AT ls_group-materials INTO ls_ordim_o.
          IF ls_ordim_o-rdocid IS NOT INITIAL.
            APPEND ls_ordim_o-rdocid TO lt_docids.
          ENDIF.
          IF ls_ordim_o-lgnum IS NOT INITIAL.
            APPEND ls_ordim_o-lgnum TO lt_lgnums.
          ENDIF.
        ENDLOOP.
        SORT lt_docids.
        DELETE ADJACENT DUPLICATES FROM lt_docids.
        SORT lt_lgnums.
        DELETE ADJACENT DUPLICATES FROM lt_lgnums.

        * Filter IT_DB_PROCI_O by docid
        IF it_db_proci_o[] IS NOT INITIAL AND lt_docids[] IS NOT INITIAL.
          LOOP AT it_db_proci_o INTO ls_proci_o.
            READ TABLE lt_docids TRANSPORTING NO FIELDS
              WITH KEY table_line = ls_proci_o-docid BINARY SEARCH.
            IF sy-subrc = 0.
              APPEND ls_proci_o TO ls_grouped_material-it_db_proci_o.
              IF ls_proci_o-docno IS NOT INITIAL.
                APPEND ls_proci_o-docno TO lt_docnos.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
        SORT lt_docnos.
        DELETE ADJACENT DUPLICATES FROM lt_docnos.

        * Filter IT_DB_PROCH_O by docno
        IF it_db_proch_o[] IS NOT INITIAL AND lt_docnos[] IS NOT INITIAL.
          LOOP AT it_db_proch_o INTO ls_proch_o.
            READ TABLE lt_docnos TRANSPORTING NO FIELDS
              WITH KEY table_line = ls_proch_o-docno BINARY SEARCH.
            IF sy-subrc = 0.
              APPEND ls_proch_o TO ls_grouped_material-it_db_proch_o.
              IF ls_proch_o-partnerto_id IS NOT INITIAL.
                * Get partner from but000
                IF it_but000[] IS NOT INITIAL.
                  READ TABLE it_but000 INTO ls_but000
                    WITH KEY partner_guid = ls_proch_o-partnerto_id BINARY SEARCH.
                  IF sy-subrc = 0 AND ls_but000-partner IS NOT INITIAL.
                    APPEND ls_but000-partner TO lt_partners.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.

        * Filter IT_DB_REFDOC by docid
        IF it_db_refdoc[] IS NOT INITIAL AND lt_docids[] IS NOT INITIAL.
          LOOP AT it_db_refdoc INTO ls_refdoc.
            READ TABLE lt_docids TRANSPORTING NO FIELDS
              WITH KEY table_line = ls_refdoc-docid BINARY SEARCH.
            IF sy-subrc = 0.
              APPEND ls_refdoc TO ls_grouped_material-it_db_refdoc.
              IF ls_refdoc-refdocno IS NOT INITIAL.
                APPEND ls_refdoc-refdocno TO lt_deliverynos.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.

        * Filter IT_IT_WAVEITM by rdocid
        IF it_it_waveitm[] IS NOT INITIAL AND lt_docids[] IS NOT INITIAL.
          LOOP AT it_it_waveitm INTO ls_waveitm.
            READ TABLE lt_docids TRANSPORTING NO FIELDS
              WITH KEY table_line = ls_waveitm-rdocid BINARY SEARCH.
            IF sy-subrc = 0.
              APPEND ls_waveitm TO ls_grouped_material-it_it_waveitm.
            ENDIF.
          ENDLOOP.
        ENDIF.

        * Filter IT_BUT000, IT_BUT020, IT_ADRC, IT_T005U by partner
        SORT lt_partners.
        DELETE ADJACENT DUPLICATES FROM lt_partners.
        IF lt_partners[] IS NOT INITIAL.
          IF it_but000[] IS NOT INITIAL.
            LOOP AT it_but000 INTO ls_but000.
              READ TABLE lt_partners TRANSPORTING NO FIELDS
                WITH KEY table_line = ls_but000-partner BINARY SEARCH.
              IF sy-subrc = 0.
                APPEND ls_but000 TO ls_grouped_material-it_but000.
              ENDIF.
            ENDLOOP.
          ENDIF.

          IF it_but020[] IS NOT INITIAL.
            LOOP AT it_but020 INTO ls_but020.
              READ TABLE lt_partners TRANSPORTING NO FIELDS
                WITH KEY table_line = ls_but020-partner BINARY SEARCH.
              IF sy-subrc = 0.
                APPEND ls_but020 TO ls_grouped_material-it_but020.
                IF ls_but020-addrnumber IS NOT INITIAL.
                  APPEND ls_but020-addrnumber TO lt_addrnumbers.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.

          SORT lt_addrnumbers.
          DELETE ADJACENT DUPLICATES FROM lt_addrnumbers.
          IF lt_addrnumbers[] IS NOT INITIAL.
            IF it_adrc[] IS NOT INITIAL.
              LOOP AT it_adrc INTO ls_adrc.
                READ TABLE lt_addrnumbers TRANSPORTING NO FIELDS
                  WITH KEY table_line = ls_adrc-addrnumber BINARY SEARCH.
                IF sy-subrc = 0.
                  APPEND ls_adrc TO ls_grouped_material-it_adrc.
                ENDIF.
              ENDLOOP.
            ENDIF.

            * Filter IT_T005U by country/region from adrc
            CLEAR: lt_country_region.
            LOOP AT ls_grouped_material-it_adrc INTO ls_adrc.
              IF ls_adrc-country IS NOT INITIAL AND ls_adrc-region IS NOT INITIAL.
                APPEND VALUE #( country = ls_adrc-country region = ls_adrc-region )
                  TO lt_country_region.
              ENDIF.
            ENDLOOP.
            SORT lt_country_region BY country region.
            DELETE ADJACENT DUPLICATES FROM lt_country_region.
            IF lt_country_region[] IS NOT INITIAL AND it_t005u[] IS NOT INITIAL.
              LOOP AT it_t005u INTO ls_t005u.
                READ TABLE lt_country_region INTO ls_country_region
                  WITH KEY country = ls_t005u-land1 region = ls_t005u-bland BINARY SEARCH.
                IF sy-subrc = 0.
                  APPEND ls_t005u TO ls_grouped_material-it_t005u.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.

        * Filter IT_PLANT, IT_LNUMT by lgnum
        IF lt_lgnums[] IS NOT INITIAL.
          IF it_plant[] IS NOT INITIAL.
            LOOP AT it_plant INTO ls_plant.
              READ TABLE lt_lgnums TRANSPORTING NO FIELDS
                WITH KEY table_line = ls_plant-lgnum BINARY SEARCH.
              IF sy-subrc = 0.
                APPEND ls_plant TO ls_grouped_material-it_plant.
              ENDIF.
            ENDLOOP.
          ENDIF.

          IF it_lnumt[] IS NOT INITIAL.
            LOOP AT it_lnumt INTO ls_lnumt.
              READ TABLE lt_lgnums TRANSPORTING NO FIELDS
                WITH KEY table_line = ls_lnumt-lgnum BINARY SEARCH.
              IF sy-subrc = 0.
                APPEND ls_lnumt TO ls_grouped_material-it_lnumt.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

        * Filter IT_DELV_ST by deliveryno
        SORT lt_deliverynos.
        DELETE ADJACENT DUPLICATES FROM lt_deliverynos.
        IF lt_deliverynos[] IS NOT INITIAL AND it_delv_st[] IS NOT INITIAL.
          LOOP AT it_delv_st INTO ls_delv_st.
            READ TABLE lt_deliverynos TRANSPORTING NO FIELDS
              WITH KEY table_line = ls_delv_st-deliveryno BINARY SEARCH.
            IF sy-subrc = 0.
              APPEND ls_delv_st TO ls_grouped_material-it_delv_st.
            ENDIF.
          ENDLOOP.
        ENDIF.

        * Build structure
        ls_grouped_material-group_id = lv_group_id.
        ls_grouped_material-group_sequence = lv_sequence.
        ls_grouped_material-group_criteria_value = ls_group-group_key.
        ls_grouped_material-it_ordim_o = ls_group-materials.

        * Calculate summary
        CLEAR: ls_summary, lv_total_qty, lv_total_qty_alt, lv_count.
        LOOP AT ls_group-materials INTO ls_ordim_o.
          lv_count = lv_count + 1.
          lv_total_qty = lv_total_qty + ls_ordim_o-vsolm.
          lv_total_qty_alt = lv_total_qty_alt + ls_ordim_o-vsola.
          
          * Extract key values from first material
          IF lv_count = 1.
            ls_summary-warehouse_code = ls_ordim_o-lgnum.
            READ TABLE lt_material_master INTO ls_material_master_temp
              WITH KEY matid = ls_ordim_o-matid BINARY SEARCH.
            IF sy-subrc = 0.
              ls_summary-material_number = ls_material_master_temp-matnr.
              ls_summary-plant_code = ls_material_master_temp-werks.
              ls_summary-division_code = ls_material_master_temp-spart.
            ENDIF.
          ENDIF.
        ENDLOOP.

        ls_summary-total_materials = lv_count.
        ls_summary-total_quantity = lv_total_qty.
        ls_summary-total_quantity_alt = lv_total_qty_alt.

        ls_grouped_material-group_summary = ls_summary.

        APPEND ls_grouped_material TO et_grouped_materials.
      ENDLOOP.

      ev_group_count = lines( et_grouped_materials ).
    ENDIF.

  ELSE.
    *----------------------------------------------------------------------*
    * Step 6: Default Behavior (No Configuration or Configuration Disabled)
    *----------------------------------------------------------------------*
    CLEAR: et_grouped_materials[], ev_group_count.
    CLEAR: ls_grouped_material, ls_summary, lv_group_id.

    * Generate default group ID
    CONCATENATE iv_veh_num 'DEFAULT' '001' INTO lv_group_id SEPARATED BY '-'.

    * Build structure - assign all data as-is (no filtering needed for default)
    ls_grouped_material-group_id = lv_group_id.
    ls_grouped_material-group_sequence = 1.
    ls_grouped_material-group_criteria_value = 'DEFAULT'.
    ls_grouped_material-group_criteria_desc = 'Default grouping (all materials)'.
    ls_grouped_material-it_ordim_o = it_ordim_o.
    IF it_db_proci_o[] IS NOT INITIAL.
      ls_grouped_material-it_db_proci_o = it_db_proci_o.
    ENDIF.
    IF it_db_proch_o[] IS NOT INITIAL.
      ls_grouped_material-it_db_proch_o = it_db_proch_o.
    ENDIF.
    IF it_db_refdoc[] IS NOT INITIAL.
      ls_grouped_material-it_db_refdoc = it_db_refdoc.
    ENDIF.
    IF it_it_waveitm[] IS NOT INITIAL.
      ls_grouped_material-it_it_waveitm = it_it_waveitm.
    ENDIF.
    IF it_but000[] IS NOT INITIAL.
      ls_grouped_material-it_but000 = it_but000.
    ENDIF.
    IF it_but020[] IS NOT INITIAL.
      ls_grouped_material-it_but020 = it_but020.
    ENDIF.
    IF it_adrc[] IS NOT INITIAL.
      ls_grouped_material-it_adrc = it_adrc.
    ENDIF.
    IF it_t005u[] IS NOT INITIAL.
      ls_grouped_material-it_t005u = it_t005u.
    ENDIF.
    IF it_plant[] IS NOT INITIAL.
      ls_grouped_material-it_plant = it_plant.
    ENDIF.
    IF it_lnumt[] IS NOT INITIAL.
      ls_grouped_material-it_lnumt = it_lnumt.
    ENDIF.
    IF it_delv_st[] IS NOT INITIAL.
      ls_grouped_material-it_delv_st = it_delv_st.
    ENDIF.

    * Calculate summary
    CLEAR: ls_summary, lv_total_qty, lv_total_qty_alt, lv_count.
    LOOP AT it_ordim_o INTO ls_ordim_o.
      lv_count = lv_count + 1.
      lv_total_qty = lv_total_qty + ls_ordim_o-vsolm.
      lv_total_qty_alt = lv_total_qty_alt + ls_ordim_o-vsola.
      
      IF lv_count = 1.
        ls_summary-warehouse_code = ls_ordim_o-lgnum.
        READ TABLE lt_material_master INTO ls_material_master_temp
          WITH KEY matid = ls_ordim_o-matid BINARY SEARCH.
        IF sy-subrc = 0.
          ls_summary-material_number = ls_material_master_temp-matnr.
          ls_summary-plant_code = ls_material_master_temp-werks.
          ls_summary-division_code = ls_material_master_temp-spart.
        ENDIF.
      ENDIF.
    ENDLOOP.

    ls_summary-total_materials = lv_count.
    ls_summary-total_quantity = lv_total_qty.
    ls_summary-total_quantity_alt = lv_total_qty_alt.

    ls_grouped_material-group_summary = ls_summary.

    APPEND ls_grouped_material TO et_grouped_materials.

    ev_group_count = 1.
    ev_grouping_criteria = 'DEFAULT'.
  ENDIF.

  *----------------------------------------------------------------------*
  * Step 7: Finalize
  *----------------------------------------------------------------------*
  IF ev_group_count > 0.
    CLEAR ls_return.
    ls_return-type = 'S'.
    ls_return-id = 'ZSCWM_MPN'.
    ls_return-number = '009'.
    ls_return-message_v1 = ev_group_count.
    MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
      WITH ls_return-message_v1 INTO ls_return-message.
    APPEND ls_return TO et_return.
  ENDIF.

ENDFUNCTION.
```

### 5.2 Code Notes

**Note**: All code is implemented inline within the function module. No FORM routines (PERFORM statements) are used, making the code suitable for SAP EWM (Extended Warehouse Management) systems.

**EWM-Specific Adaptations**:
- Uses EWM-native material master access via `IT_DB_PROCI_O` (product information)
- Reads material master data from EWM-native tables:
  - `/SCWM/MAT` - EWM Material Master table
  - `/SCWM/MATPLANT` - EWM Material Plant data (for plant code)
  - `MVKE` - Material Sales Data (for division, standard SAP table used in EWM)
- Removes APO-specific dependencies (`/SAPAPO/MARA`, `/SAPAPO/INC_CONVERT_GUIDS`)
- Removes ECC-specific dependencies (`MARC` table)
- Uses material numbers directly from EWM product information tables
- Handles material GUIDs from EWM order items (`IT_ORDIM_O`)

The function module contains the following logical sections:
1. **Input Validation**: Validates warehouse, vehicle, and materials table
2. **Configuration Lookup**: Reads configuration from ZSCWM_MPN_CONFIG table using hierarchy
3. **Master Data Reading**: Reads material master data using EWM-native tables (/SCWM/MAT, /SCWM/MATPLANT, MVKE)
4. **Grouping Logic**: Applies grouping criteria and creates groups
5. **Output Building**: Filters all related tables per group and builds output structure
6. **Default Behavior**: Creates single group if no configuration found
7. **Finalization**: Adds success message to return table

All helper functions (like generate_group_id, calculate_group_summary, log_warning) are implemented inline where needed.

---

**Note**: The following FORM routine sections are kept for reference only. The actual implementation in Section 5.1 uses inline code without PERFORM statements, optimized for SAP EWM (Extended Warehouse Management) landscape.

#### 5.2.1 VALIDATE_INPUTS (Reference - Now Inline in Section 5.1)

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
                            it_db_proci_o TYPE zewm_db_proci_o1_tt
                   CHANGING et_material_master TYPE lty_material_master_tt
                            et_return TYPE bapiret2_t.

  DATA: lt_matid_16 TYPE TABLE OF /scwm/de_matid,
        ls_material_master TYPE lty_material_master,
        ls_return TYPE bapiret2,
        lt_material_numbers TYPE TABLE OF matnr,
        lt_matplant TYPE TABLE OF /scwm/matplant,
        lt_mat TYPE TABLE OF /scwm/mat,
        lt_mvke TYPE TABLE OF mvke,
        ls_matplant TYPE /scwm/matplant,
        ls_mvke TYPE mvke,
        lv_matnr TYPE matnr,
        ls_proci_o TYPE zewm_db_proci_o1_st,
        ls_ordim_o TYPE zewm_ordim_o_st.

  CLEAR: et_material_master[].

  * Extract unique material numbers from EWM product information
  IF it_db_proci_o[] IS NOT INITIAL.
    LOOP AT it_db_proci_o INTO ls_proci_o.
      IF ls_proci_o-productno IS NOT INITIAL.
        APPEND ls_proci_o-productno TO lt_material_numbers.
      ENDIF.
    ENDLOOP.
    SORT lt_material_numbers.
    DELETE ADJACENT DUPLICATES FROM lt_material_numbers.
  ENDIF.

  * Extract unique material GUIDs
  LOOP AT it_ordim_o INTO ls_ordim_o.
    IF ls_ordim_o-matid IS NOT INITIAL.
      APPEND ls_ordim_o-matid TO lt_matid_16.
    ENDIF.
  ENDLOOP.
  SORT lt_matid_16.
  DELETE ADJACENT DUPLICATES FROM lt_matid_16.

  * Read EWM Material Master data
  IF lt_material_numbers[] IS NOT INITIAL.
    * Read EWM Material Master table
    SELECT matnr FROM /scwm/mat
      INTO TABLE lt_mat
      FOR ALL ENTRIES IN lt_material_numbers
      WHERE matnr = lt_material_numbers-table_line.

    * Read EWM Material Plant data (for plant code)
    SELECT matnr werks FROM /scwm/matplant
      INTO TABLE lt_matplant
      FOR ALL ENTRIES IN lt_material_numbers
      WHERE matnr = lt_material_numbers-table_line.

    * Read Material Sales Data (for division)
    SELECT matnr vkorg vtweg spart FROM mvke
      INTO TABLE lt_mvke
      FOR ALL ENTRIES IN lt_material_numbers
      WHERE matnr = lt_material_numbers-table_line
        AND vkorg <> space
        AND vtweg <> space.

    * Build material master table
    LOOP AT lt_material_numbers INTO lv_matnr.
      CLEAR ls_material_master.
      ls_material_master-matnr = lv_matnr.
      
      * Find corresponding GUID from IT_ORDIM_O
      LOOP AT it_ordim_o INTO ls_ordim_o.
        IF it_db_proci_o[] IS NOT INITIAL.
          READ TABLE it_db_proci_o INTO ls_proci_o
            WITH KEY productno = lv_matnr.
          IF sy-subrc = 0.
            READ TABLE it_ordim_o INTO ls_ordim_o
              WITH KEY rdocid = ls_proci_o-docid.
            IF sy-subrc = 0 AND ls_ordim_o-matid IS NOT INITIAL.
              ls_material_master-matid = ls_ordim_o-matid.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      
      * Read plant from EWM Material Plant table
      READ TABLE lt_matplant INTO ls_matplant
        WITH KEY matnr = lv_matnr.
      IF sy-subrc = 0.
        ls_material_master-werks = ls_matplant-werks.
      ENDIF.
      
      * Read division from Material Sales Data
      READ TABLE lt_mvke INTO ls_mvke
        WITH KEY matnr = lv_matnr.
      IF sy-subrc = 0.
        ls_material_master-spart = ls_mvke-spart.
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
                       it_db_proci_o TYPE zewm_db_proci_o1_tt
                       it_db_proch_o TYPE zewm_db_proch_o_tt
                       it_db_refdoc TYPE zewm_db_ref_doc_tt
                       it_it_waveitm TYPE zewm_waveitm_tt
                       it_but000 TYPE zewm_but000_tt
                       it_but020 TYPE zewm_but020_tt
                       it_adrc TYPE zewm_adrc_tt
                       it_t005u TYPE t005u_t
                       it_plant TYPE /scwm/t300_md_tt
                       it_lnumt TYPE /scwm/t300t_tt
                       it_delv_st TYPE zlog_get_detail_tt
              CHANGING et_grouped_materials TYPE zscwm_mpn_group_tt
                       ev_group_count TYPE i.

  DATA: ls_grouped_material TYPE zscwm_mpn_group_st,
        ls_group TYPE lty_group_internal,
        lv_group_id TYPE char20,
        lv_sequence TYPE i,
        ls_summary TYPE zscwm_mpn_group_summary_st,
        lt_docids TYPE /scwm/tt_rdocid,
        lt_docnos TYPE TABLE OF /scdl/dl_docno,
        lt_partners TYPE TABLE OF bu_partner,
        lt_addrnumbers TYPE TABLE OF ad_addrnum,
        lt_deliverynos TYPE TABLE OF vbeln_vl,
        lt_lgnums TYPE TABLE OF /scwm/lgnum.

  CLEAR: et_grouped_materials[], ev_group_count.

  SORT it_groups BY group_key.

  LOOP AT it_groups INTO ls_group.
    CLEAR: ls_grouped_material, ls_summary,
           lt_docids, lt_docnos, lt_partners, lt_addrnumbers,
           lt_deliverynos, lt_lgnums.

    lv_sequence = lv_sequence + 1.

    * Generate group ID
    PERFORM generate_group_id USING iv_veh_num
                                    ls_group-group_key
                                    lv_sequence
                          CHANGING lv_group_id.

    * Extract document IDs from group materials
    LOOP AT ls_group-materials INTO DATA(ls_ordim_o).
      IF ls_ordim_o-rdocid IS NOT INITIAL.
        APPEND ls_ordim_o-rdocid TO lt_docids.
      ENDIF.
      IF ls_ordim_o-lgnum IS NOT INITIAL.
        APPEND ls_ordim_o-lgnum TO lt_lgnums.
      ENDIF.
    ENDLOOP.
    SORT lt_docids.
    DELETE ADJACENT DUPLICATES FROM lt_docids.
    SORT lt_lgnums.
    DELETE ADJACENT DUPLICATES FROM lt_lgnums.

    * Filter IT_DB_PROCI_O by docid
    LOOP AT it_db_proci_o INTO DATA(ls_proci_o)
      WHERE docid IN lt_docids.
      APPEND ls_proci_o TO ls_grouped_material-it_db_proci_o.
      IF ls_proci_o-docno IS NOT INITIAL.
        APPEND ls_proci_o-docno TO lt_docnos.
      ENDIF.
    ENDLOOP.
    SORT lt_docnos.
    DELETE ADJACENT DUPLICATES FROM lt_docnos.

    * Filter IT_DB_PROCH_O by docno
    LOOP AT it_db_proch_o INTO DATA(ls_proch_o)
      WHERE docno IN lt_docnos.
      APPEND ls_proch_o TO ls_grouped_material-it_db_proch_o.
      IF ls_proch_o-partnerto_id IS NOT INITIAL.
        * Get partner from but000
        READ TABLE it_but000 INTO DATA(ls_but000)
          WITH KEY partner_guid = ls_proch_o-partnerto_id BINARY SEARCH.
        IF sy-subrc = 0 AND ls_but000-partner IS NOT INITIAL.
          APPEND ls_but000-partner TO lt_partners.
        ENDIF.
      ENDIF.
    ENDLOOP.

    * Filter IT_DB_REFDOC by docid
    LOOP AT it_db_refdoc INTO DATA(ls_refdoc)
      WHERE docid IN lt_docids.
      APPEND ls_refdoc TO ls_grouped_material-it_db_refdoc.
      IF ls_refdoc-refdocno IS NOT INITIAL.
        APPEND ls_refdoc-refdocno TO lt_deliverynos.
      ENDIF.
    ENDLOOP.

    * Filter IT_IT_WAVEITM by rdocid
    LOOP AT it_it_waveitm INTO DATA(ls_waveitm)
      WHERE rdocid IN lt_docids.
      APPEND ls_waveitm TO ls_grouped_material-it_it_waveitm.
    ENDLOOP.

    * Filter IT_BUT000, IT_BUT020, IT_ADRC, IT_T005U by partner
    SORT lt_partners.
    DELETE ADJACENT DUPLICATES FROM lt_partners.
    IF lt_partners[] IS NOT INITIAL.
      LOOP AT it_but000 INTO ls_but000
        WHERE partner IN lt_partners.
        APPEND ls_but000 TO ls_grouped_material-it_but000.
      ENDLOOP.

      LOOP AT it_but020 INTO DATA(ls_but020)
        WHERE partner IN lt_partners.
        APPEND ls_but020 TO ls_grouped_material-it_but020.
        IF ls_but020-addrnumber IS NOT INITIAL.
          APPEND ls_but020-addrnumber TO lt_addrnumbers.
        ENDIF.
      ENDLOOP.

      SORT lt_addrnumbers.
      DELETE ADJACENT DUPLICATES FROM lt_addrnumbers.
      IF lt_addrnumbers[] IS NOT INITIAL.
        LOOP AT it_adrc INTO DATA(ls_adrc)
          WHERE addrnumber IN lt_addrnumbers.
          APPEND ls_adrc TO ls_grouped_material-it_adrc.
        ENDLOOP.

        * Filter IT_T005U by country/region from adrc
        DATA: lt_country_region TYPE TABLE OF ty_country_region.
        TYPES: BEGIN OF ty_country_region,
                 country TYPE land1,
                 region TYPE regio,
               END OF ty_country_region.
        LOOP AT ls_grouped_material-it_adrc INTO ls_adrc.
          APPEND VALUE #( country = ls_adrc-country region = ls_adrc-region )
            TO lt_country_region.
        ENDLOOP.
        SORT lt_country_region BY country region.
        DELETE ADJACENT DUPLICATES FROM lt_country_region.
        LOOP AT it_t005u INTO DATA(ls_t005u)
          FOR ALL ENTRIES IN lt_country_region
          WHERE land1 = lt_country_region-country
            AND bland = lt_country_region-region.
          APPEND ls_t005u TO ls_grouped_material-it_t005u.
        ENDLOOP.
      ENDIF.
    ENDIF.

    * Filter IT_PLANT, IT_LNUMT by lgnum
    IF lt_lgnums[] IS NOT INITIAL.
      LOOP AT it_plant INTO DATA(ls_plant)
        WHERE lgnum IN lt_lgnums.
        APPEND ls_plant TO ls_grouped_material-it_plant.
      ENDLOOP.

      LOOP AT it_lnumt INTO DATA(ls_lnumt)
        WHERE lgnum IN lt_lgnums.
        APPEND ls_lnumt TO ls_grouped_material-it_lnumt.
      ENDLOOP.
    ENDIF.

    * Filter IT_DELV_ST by deliveryno
    SORT lt_deliverynos.
    DELETE ADJACENT DUPLICATES FROM lt_deliverynos.
    IF lt_deliverynos[] IS NOT INITIAL.
      LOOP AT it_delv_st INTO DATA(ls_delv_st)
        WHERE deliveryno IN lt_deliverynos.
        APPEND ls_delv_st TO ls_grouped_material-it_delv_st.
      ENDLOOP.
    ENDIF.

    * Build structure
    ls_grouped_material-group_id = lv_group_id.
    ls_grouped_material-group_sequence = lv_sequence.
    ls_grouped_material-group_criteria_value = ls_group-group_key.
    ls_grouped_material-it_ordim_o = ls_group-materials.

    * Calculate summary
    PERFORM calculate_group_summary USING ls_group-materials
                                          ls_grouped_material-it_db_proci_o
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
                               it_db_proch_o TYPE zewm_db_proch_o_tt
                               it_db_refdoc TYPE zewm_db_ref_doc_tt
                               it_it_waveitm TYPE zewm_waveitm_tt
                               it_but000 TYPE zewm_but000_tt
                               it_but020 TYPE zewm_but020_tt
                               it_adrc TYPE zewm_adrc_tt
                               it_t005u TYPE t005u_t
                               it_plant TYPE /scwm/t300_md_tt
                               it_lnumt TYPE /scwm/t300t_tt
                               it_delv_st TYPE zlog_get_detail_tt
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

  * Build structure - assign all data as-is (no filtering needed for default)
  ls_grouped_material-group_id = lv_group_id.
  ls_grouped_material-group_sequence = 1.
  ls_grouped_material-group_criteria_value = 'DEFAULT'.
  ls_grouped_material-group_criteria_desc = 'Default grouping (all materials)'.
  ls_grouped_material-it_ordim_o = it_ordim_o.
  ls_grouped_material-it_db_proci_o = it_db_proci_o.
  ls_grouped_material-it_db_proch_o = it_db_proch_o.
  ls_grouped_material-it_db_refdoc = it_db_refdoc.
  ls_grouped_material-it_it_waveitm = it_it_waveitm.
  ls_grouped_material-it_but000 = it_but000.
  ls_grouped_material-it_but020 = it_but020.
  ls_grouped_material-it_adrc = it_adrc.
  ls_grouped_material-it_t005u = it_t005u.
  ls_grouped_material-it_plant = it_plant.
  ls_grouped_material-it_lnumt = it_lnumt.
  ls_grouped_material-it_delv_st = it_delv_st.

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

### 6.2 Material Master Data Access (EWM Native)

**Pattern**: Extract material numbers from EWM product information, then read EWM-native tables

**Optimization**:
- Extract material numbers directly from `IT_DB_PROCI_O-productno` (EWM product information)
- Read `/SCWM/MAT` (EWM Material Master) to validate material existence
- Read `/SCWM/MATPLANT` (EWM Material Plant) for plant code
- Read `MVKE` (Material Sales Data) for division information
- Map material GUIDs from `IT_ORDIM_O` to material numbers for grouping
- Cache results in internal tables

**Example**:
```abap
* Extract material numbers from EWM product information
LOOP AT it_db_proci_o INTO ls_proci_o.
  IF ls_proci_o-productno IS NOT INITIAL.
    APPEND ls_proci_o-productno TO lt_material_numbers.
  ENDIF.
ENDLOOP.

* Read EWM Material Master table
SELECT matnr FROM /scwm/mat
  INTO TABLE lt_mat
  FOR ALL ENTRIES IN lt_material_numbers
  WHERE matnr = lt_material_numbers-table_line.

* Read EWM Material Plant data (for plant code)
SELECT matnr werks FROM /scwm/matplant
  INTO TABLE lt_matplant
  FOR ALL ENTRIES IN lt_material_numbers
  WHERE matnr = lt_material_numbers-table_line.

* Read Material Sales Data (for division)
SELECT matnr vkorg vtweg spart FROM mvke
  INTO TABLE lt_mvke
  FOR ALL ENTRIES IN lt_material_numbers
  WHERE matnr = lt_material_numbers-table_line
    AND vkorg <> space
    AND vtweg <> space.

* Map material GUIDs from order items
LOOP AT it_ordim_o INTO ls_ordim_o.
  * Match via IT_DB_PROCI_O to get material number
  READ TABLE it_db_proci_o INTO ls_proci_o
    WITH KEY docid = ls_ordim_o-rdocid.
  IF sy-subrc = 0.
    * Use productno for material master lookup
  ENDIF.
ENDLOOP.
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

## 10. Integration with Z_FIORI_SWM_OB_TRK_PRINT_MPN

### 10.1 Integration Point

**Location**: After line 1826 (after `gt_ordim_o_tt` is populated), before SmartForm call at line 1868

**Insert Code**:

```abap
*----------------------------------------------------------------------*
* Call grouping function module for configurable MPN grouping
*----------------------------------------------------------------------*
DATA: lt_grouped_materials TYPE zscwm_mpn_group_tt,
      lv_group_count TYPE i,
      lv_config_id TYPE char32,
      lv_config_level TYPE char10,
      lv_grouping_criteria TYPE char200,
      lt_return_group TYPE bapiret2_t,
      lw_return_group TYPE bapiret2,
      lv_use_config TYPE abap_bool VALUE 'X',
      lv_mpn_sequence TYPE i,
      lw_group TYPE zscwm_mpn_group_st,
      lt_otf_data_final TYPE tsfotf,
      lv_xstring_temp TYPE xstring,
      lv_filename_temp TYPE /bofu/field_name.

* Check if configurable grouping is enabled
* This can be controlled via customizing table or parameter
IF lv_use_config = abap_true AND gt_ordim_o_tt IS NOT INITIAL.
  
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
      et_return            = lt_return_group
    EXCEPTIONS
      OTHERS               = 99.

  IF sy-subrc = 0.
    * Check for errors in grouping
    READ TABLE lt_return_group INTO lw_return_group 
      WITH KEY type = 'E'.
    IF sy-subrc = 0.
      * Error in grouping, use default behavior (single MPN)
      APPEND LINES OF lt_return_group TO et_return.
      CLEAR: lt_grouped_materials, lv_group_count.
    ELSE.
      * Grouping successful - check if multiple MPNs needed
      APPEND LINES OF lt_return_group TO et_return.
      
      * If multiple groups, generate MPN for each group
      IF lv_group_count > 1.
        CLEAR lv_mpn_sequence.
        CLEAR: e_xstring, e_filename.
        
        LOOP AT lt_grouped_materials INTO lw_group.
          lv_mpn_sequence = lv_mpn_sequence + 1.
          
          * Prepare data for SmartForm from grouped data
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
          CONCATENATE i_report_no lw_group-group_id 'pdf' 
            INTO lv_filename_temp SEPARATED BY '.'.
          
          * Call SmartForm for this group
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
            CALL FUNCTION 'CONVERT_OTF'
              EXPORTING
                format = 'PDF'
              IMPORTING
                bin_file = lv_xstring_temp
              TABLES
                otf = gt_otf_data_temp-otfdata
                lines = gt_lines
              EXCEPTIONS
                OTHERS = 5.

            IF sy-subrc = 0.
              * Combine PDFs or handle multiple files
              * For now, use last group's PDF (or implement PDF merge)
              e_xstring = lv_xstring_temp.
              e_filename = lv_filename_temp.
              
              * Option: Append to combined PDF or return multiple files
              * This depends on business requirement
            ENDIF.
          ENDIF.
        ENDLOOP.
        
        * Set final output
        e_mimetype = 'application/pdf'.
        
        * Exit after processing all groups
        RETURN.
      ELSE.
        * Single group - continue with existing logic below
        * Data already filtered in lw_group, use it
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
  ENDIF.
ENDIF.

* If no configuration or grouping failed, continue with existing logic
* (Default: single MPN per vehicle - existing code continues from line 1828)
```

### 10.2 Multiple MPN Handling Options

**Option 1: Return Last Group's PDF** (Current implementation)
- Simple approach
- Returns PDF for last group only
- Suitable if only one MPN is needed at a time

**Option 2: Combine Multiple PDFs**
- Merge all group PDFs into single document
- Requires PDF merge functionality
- Returns one combined PDF

**Option 3: Return Multiple Files**
- Modify function to return table of PDFs
- Each group has separate PDF
- Requires calling program to handle multiple files

**Recommendation**: Start with Option 1, enhance to Option 2 if business requires combined PDF.

### 10.3 Data Flow

```
Z_FIORI_SWM_OB_TRK_PRINT_MPN
    │
    ├─> Collects materials (gt_ordim_o_tt)
    ├─> Collects related data (gt_db_proci_o, gt_db_proch_o, etc.)
    │
    ├─> Calls Z_SCWM_MPN_GROUP_MATERIALS
    │   │
    │   ├─> Groups materials based on configuration
    │   ├─> Filters all related tables per group
    │   └─> Returns ET_GROUPED_MATERIALS (one entry per group)
    │
    └─> Loop through groups
        │
        ├─> For each group:
        │   ├─> Assign filtered data to SmartForm tables
        │   ├─> Call SmartForm
        │   └─> Generate PDF
        │
        └─> Return final PDF(s)
```

### 10.4 Key Points

1. **Multiple Print Indicator**: `EV_GROUP_COUNT > 1` indicates multiple MPNs needed
2. **Complete Data per Group**: Each group contains all filtered data needed for SmartForm
3. **Backward Compatibility**: If grouping fails or returns single group, existing logic continues
4. **Error Handling**: All errors from grouping function are appended to `ET_RETURN`

---

## 11. Implementation Checklist

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

## 12. Code Review Checklist

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

## 13. Deployment Plan

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

## 14. Maintenance Considerations

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

## 15. Appendix

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

### 14.2 Helper Functions (All Implemented Inline)

**Note**: All helper functions are implemented inline within the main function module code (Section 5.1). No separate FORM routines are used.

#### GET_WAREHOUSE_FROM_STORAGE_BIN (Inline Implementation)

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

#### LOG_WARNING (Inline Implementation)

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

#### CALCULATE_GROUP_SUMMARY (Inline Implementation)

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
| 1.1 | 2025-01-XX | [To be filled] | EWM Adaptation: Removed APO dependencies, optimized for EWM-native material master access |

---

**Document Status**: Draft for Review

**Next Steps**:
1. Review and approve TS
2. Create DDIC objects
3. Implement function module
4. Unit testing
5. Integration testing

