# Function Specification: Z_SCWM_MPN_GROUP_MATERIALS

## Description

This function module groups dispatch materials based on configurable grouping criteria (warehouse-wise, material-wise, plant code-wise, division-wise, or combinations thereof) to enable generation of multiple Material Pickup Notes (MPNs) per vehicle. The function reads active configuration from the custom configuration table, applies grouping logic based on the configuration hierarchy, and returns grouped materials ready for MPN generation.

**Business Context**: Currently, one MPN is generated per vehicle regardless of source warehouse, material, plant, or division. This function enables configurable grouping to generate separate MPNs for efficient warehouse picking operations.

**Technical Context**: This function is called from `Z_FIORI_SWM_OB_TRK_PRINT_MPN` after materials are collected from SAP EWM tables but before SmartForm generation. It replaces or enhances the existing grouping logic (currently based on storage bin and batch).

---

## Function Module Details

**Function Module Name**: `Z_SCWM_MPN_GROUP_MATERIALS`

**Function Group**: `ZSCWM_MPN` (to be created)

**Function Type**: Remote-Enabled Function Module (RFC)

**Exception Handling**: Yes

---

## Inputs

| Field Name | Type | Required | Constraints | Validation Rules | Description |
|------------|------|----------|-------------|------------------|-------------|
| IV_LGNUM | `/SCWM/LGNUM` | Yes | Not initial | Must exist in `/SCWM/T300` | Warehouse number (Logistics Warehouse Number) |
| IV_VEH_NUM | `/SCWM/DE_VEH_NUM` | Yes | Not initial | Must exist in `/SCWM/VEHICLE` | Vehicle number for which MPN is being generated |
| IV_REPORT_NO | `YREPORT_NO` | Yes | Not initial | Alphanumeric, max 20 chars | Report number / Vehicle external number |
| IT_ORDIM_O | `ZEWM_ORDIM_O_TT` | Yes | Not initial | Table must contain at least one entry | Internal table of order items (materials to dispatch) from `/SCWM/ORDIM_O` |
| IT_DB_PROCI_O | `ZEWM_DB_PROCI_O1_TT` | No | - | - | Product information from `/SCDL/DB_PROCI_O` (for plant/division data) |
| IV_DISPATCH_DATE | `SYDATUM` | No | Valid date | Date format validation | Dispatch date (defaults to current date if not provided) |
| IV_USE_CONFIG | `ABAP_BOOL` | No | 'X' or ' ' | - | Flag to use configuration (default 'X'). If ' ', uses default grouping (one MPN per vehicle) |

**Input Structure Details**:

### IT_ORDIM_O Structure (ZEWM_ORDIM_O_ST):
- `LGNUM`: Warehouse number
- `TANUM`: Transfer order number
- `TRART`: Transfer order type
- `TOSTAT`: Transfer order status
- `MATID`: Material GUID (16-byte)
- `STOCK_ITMNO`: Stock item number
- `CHARG`: Batch number
- `MEINS`: Base unit of measure
- `VSOLM`: Source quantity in base unit
- `VSOLA`: Source quantity in alternative unit
- `IDPLATE`: ID plate
- `VLPLA`: Source storage bin
- `SGUID_HU`: Source handling unit GUID
- `VLENR`: Source storage unit
- `NLBER`: Destination storage bin type
- `DGUID_HU`: Destination handling unit GUID
- `NLENR`: Destination storage unit
- `RDOCCAT`: Reference document category
- `RDOCID`: Reference document ID (PDO document)
- `RITMID`: Reference item ID
- `TCODE`: Transaction code
- `ZZEXP_SRNO`: Serial number (custom field)

### IT_DB_PROCI_O Structure (ZEWM_DB_PROCI_O1_ST):
- `DOCID`: Document ID
- `DOCNO`: Document number
- `ITEMNO`: Item number
- `PRODUCTNO`: Product number (material number)
- `QTY`: Quantity
- `STOCK_OWNER`: Stock owner (plant code)
- `REFDOCNO_SO`: Reference document number for sales order

---

## Output

| Field Name | Type | Description |
|------------|------|-------------|
| ET_GROUPED_MATERIALS | `ZSCWM_MPN_GROUP_TT` | Internal table containing grouped materials, one entry per MPN group |
| EV_GROUP_COUNT | `I` | Number of groups created (number of MPNs to be generated) |
| EV_CONFIG_ID | `CHAR32` | Configuration ID that was applied |
| EV_CONFIG_LEVEL | `CHAR10` | Configuration level applied (GLOBAL, SITE, PLANT, DIVISION, MATERIAL) |
| EV_GROUPING_CRITERIA | `CHAR200` | Grouping criteria applied (comma-separated list) |
| ET_RETURN | `BAPIRET2_T` | Return messages (success, warnings, errors) |

**Output Structure Details**:

### ET_GROUPED_MATERIALS Structure (ZSCWM_MPN_GROUP_ST):
- `GROUP_ID`: `CHAR20` - Unique group identifier (e.g., W001, M001, P001-D001)
- `GROUP_SEQUENCE`: `I` - Sequence number for groups of same vehicle
- `GROUP_CRITERIA_VALUE`: `CHAR200` - Value of grouping criteria (e.g., warehouse code, material number)
- `GROUP_CRITERIA_DESC`: `CHAR200` - Description of grouping criteria value
- `IT_ORDIM_O`: `ZEWM_ORDIM_O_TT` - Materials belonging to this group
- `IT_DB_PROCI_O`: `ZEWM_DB_PROCI_O1_TT` - Product information for this group
- `GROUP_SUMMARY`: `ZSCWM_MPN_GROUP_SUMMARY_ST` - Summary information for the group
  - `TOTAL_MATERIALS`: `I` - Number of materials in group
  - `TOTAL_QUANTITY`: `QUAN` - Total quantity in base unit
  - `TOTAL_QUANTITY_ALT`: `QUAN` - Total quantity in alternative unit
  - `WAREHOUSE_CODE`: `/SCWM/DE_WHO` - Warehouse code (if grouping by warehouse)
  - `MATERIAL_NUMBER`: `MATNR` - Material number (if grouping by material)
  - `PLANT_CODE`: `WERKS_D` - Plant code (if grouping by plant)
  - `DIVISION_CODE`: `SPART` - Division code (if grouping by division)

---

## Behavior

### Step 1: Initialize and Validate Inputs
1. Clear all output parameters
2. Validate `IV_LGNUM` exists in `/SCWM/T300`
3. Validate `IV_VEH_NUM` exists in `/SCWM/VEHICLE`
4. Validate `IT_ORDIM_O` is not empty
5. If validation fails, populate `ET_RETURN` with error message and exit

### Step 2: Determine Configuration to Apply
1. If `IV_USE_CONFIG` is initial or ' ', skip to Step 6 (default behavior - single MPN)
2. Read active configuration from custom table `ZSCWM_MPN_CONFIG` using hierarchy:
   - First check Material-specific configuration (if material data available)
   - Then check Division-specific configuration
   - Then check Plant-specific configuration
   - Then check Site-specific configuration (based on warehouse)
   - Finally check Global configuration
3. Configuration lookup criteria:
   - For Material level: Match material number from `IT_DB_PROCI_O`
   - For Division level: Match division from material master
   - For Plant level: Match plant code from `IT_DB_PROCI_O` or material master
   - For Site level: Match warehouse number (`IV_LGNUM`)
   - For Global level: No matching criteria needed
4. Select configuration where:
   - `IS_ACTIVE = 'X'`
   - `EFFECTIVE_FROM_DATE <= Current Date`
   - `EFFECTIVE_TO_DATE >= Current Date` OR `EFFECTIVE_TO_DATE IS INITIAL`
5. If multiple configurations match at same level, select most recent (by `CREATED_DATE`)
6. Store selected configuration in `EV_CONFIG_ID` and `EV_CONFIG_LEVEL`
7. If no configuration found, set default behavior flag and proceed to Step 6

### Step 3: Read Master Data for Grouping
1. Extract unique material GUIDs from `IT_ORDIM_O`
2. Convert material GUIDs to material numbers using `/SAPAPO/INC_CONVERT_GUIDS`
3. Read material master data (`MARA`, `MARC`, `MVKE`) for:
   - Material number
   - Plant code (`WERKS`)
   - Division (`SPART`)
4. Read warehouse master data (`/SCWM/T300`) for warehouse descriptions
5. Store master data in internal tables for grouping logic

### Step 4: Apply Grouping Logic Based on Configuration
1. Parse `GROUPING_CRITERIA` from configuration (comma-separated or JSON format)
2. For each material in `IT_ORDIM_O`:
   a. Extract grouping criteria values:
      - Warehouse: Use `VLPLA` (storage bin) or derive from `LGNUM`
      - Material: Use material number from master data
      - Plant: Use plant code from `IT_DB_PROCI_O` or material master
      - Division: Use division from material master
   b. Create group key by concatenating selected criteria values
   c. Handle missing values:
      - If required criteria value is missing, use default value 'UNKNOWN' and log warning
      - If optional criteria value is missing, exclude from group key
   d. Assign material to group based on group key
3. Group materials:
   - Create internal table `LT_GROUPS` with group key and materials
   - For each unique group key, create one entry in `LT_GROUPS`
   - Append materials to corresponding group
4. Validate groups:
   - Ensure no empty groups (groups with zero materials)
   - Log warning if expected groups have no materials
   - Remove empty groups from `LT_GROUPS`

### Step 5: Build Output Structure
1. Sort `LT_GROUPS` by group key
2. Initialize `EV_GROUP_COUNT = 0`
3. Loop through `LT_GROUPS`:
   a. Increment `EV_GROUP_COUNT`
   b. Create `ZSCWM_MPN_GROUP_ST` structure:
      - `GROUP_ID`: Generate unique ID (format: `{VEH_NUM}-{GROUP_KEY}-{SEQUENCE}`)
      - `GROUP_SEQUENCE`: `EV_GROUP_COUNT`
      - `GROUP_CRITERIA_VALUE`: Group key value
      - `GROUP_CRITERIA_DESC`: Description from master data
      - `IT_ORDIM_O`: Materials in this group
      - `IT_DB_PROCI_O`: Filtered product info for materials in this group
      - `GROUP_SUMMARY`: Calculate totals and key values
   c. Append to `ET_GROUPED_MATERIALS`
4. Set `EV_GROUPING_CRITERIA` to comma-separated list of criteria codes used

### Step 6: Default Behavior (No Configuration or Configuration Disabled)
1. If no configuration found or `IV_USE_CONFIG` is initial:
   a. Create single group with all materials
   b. Set `EV_GROUP_COUNT = 1`
   c. Set `GROUP_ID = {VEH_NUM}-DEFAULT-001`
   d. Set `EV_GROUPING_CRITERIA = 'DEFAULT'`
   e. Append all materials to single group in `ET_GROUPED_MATERIALS`
2. This maintains backward compatibility with existing single MPN per vehicle behavior

### Step 7: Finalize and Return
1. Sort `ET_GROUPED_MATERIALS` by `GROUP_SEQUENCE`
2. Populate success message in `ET_RETURN`:
   - Type: 'S' (Success)
   - Message: "Materials grouped successfully. {EV_GROUP_COUNT} MPN(s) to be generated."
3. If warnings occurred (missing criteria values), add warning messages to `ET_RETURN`
4. Return to calling function

---

## Errors

| Error Code | Error Message | Trigger Condition | Handling |
|------------|---------------|-------------------|----------|
| CONFIG-001 | "Warehouse number &1 is invalid" | `IV_LGNUM` not found in `/SCWM/T300` | Set return type 'E', populate `ET_RETURN`, exit function |
| CONFIG-002 | "Vehicle number &1 is invalid" | `IV_VEH_NUM` not found in `/SCWM/VEHICLE` | Set return type 'E', populate `ET_RETURN`, exit function |
| CONFIG-003 | "No materials provided for grouping" | `IT_ORDIM_O` is empty | Set return type 'E', populate `ET_RETURN`, exit function |
| CONFIG-004 | "Invalid configuration &1 found" | Configuration exists but has invalid `GROUPING_CRITERIA` format | Set return type 'E', populate `ET_RETURN`, use default behavior |
| DATA-001 | "Material &1 missing &2 value for grouping" | Material does not have required grouping criteria value | Set return type 'W', use default value 'UNKNOWN', continue processing |
| DATA-002 | "No groups created after grouping" | All materials filtered out or no valid groups | Set return type 'E', populate `ET_RETURN`, exit function |
| SYS-001 | "Error reading configuration table" | Database error when reading `ZSCWM_MPN_CONFIG` | Set return type 'E', populate `ET_RETURN`, use default behavior |
| SYS-002 | "Error reading material master data" | Error reading `MARA`/`MARC`/`MVKE` | Set return type 'W', continue with available data, log warning |

**Error Message Format**: Use SAP standard message class or custom message class `ZSCWM_MPN`

---

## Success

### Success Conditions
1. ✅ Materials successfully grouped based on configuration
2. ✅ `ET_GROUPED_MATERIALS` contains at least one group
3. ✅ `EV_GROUP_COUNT` > 0
4. ✅ All materials from input assigned to exactly one group
5. ✅ Return message type 'S' in `ET_RETURN`

### Success Response Structure
- `ET_RETURN` contains:
  - At least one message with type 'S'
  - Message text: "Materials grouped successfully. {count} MPN(s) to be generated."
- `ET_GROUPED_MATERIALS` contains grouped materials ready for MPN generation
- `EV_GROUP_COUNT` indicates number of MPNs to generate
- `EV_CONFIG_ID` and `EV_CONFIG_LEVEL` indicate which configuration was applied

---

## Edge Cases

### Edge Case 1: Single Material, Multiple Warehouses
**Scenario**: Vehicle has same material from multiple warehouses when grouping is warehouse-wise.

**Handling**:
- Generate separate group for each warehouse (even if same material)
- Each group will have one material entry
- Group ID will include warehouse code: `{VEH_NUM}-W001-001`, `{VEH_NUM}-W002-001`

### Edge Case 2: Material with Missing Grouping Criteria Value
**Scenario**: Material does not have value for selected grouping criteria (e.g., missing plant code when grouping by plant).

**Handling**:
- Use default value 'UNKNOWN' for missing criteria
- Create group with key 'UNKNOWN'
- Log warning message in `ET_RETURN` with type 'W'
- Continue processing other materials

### Edge Case 3: All Materials in Same Group
**Scenario**: All materials belong to same group based on criteria (e.g., all from same warehouse).

**Handling**:
- Generate single group (same as default behavior)
- Set `EV_GROUP_COUNT = 1`
- This is valid scenario, no error

### Edge Case 4: Zero Materials in a Group
**Scenario**: After grouping, one or more groups have zero materials.

**Handling**:
- Remove empty groups from `LT_GROUPS`
- Do not create entry in `ET_GROUPED_MATERIALS` for empty groups
- Log information message (not error)
- Continue with non-empty groups

### Edge Case 5: Multiple Configurations Applicable
**Scenario**: Multiple configurations match (e.g., material-specific and plant-specific both exist).

**Handling**:
- Apply configuration priority: Material > Division > Plant > Site > Global
- Use most specific configuration found
- Log which configuration was applied in `EV_CONFIG_ID` and `EV_CONFIG_LEVEL`

### Edge Case 6: Configuration with Future Effective Date
**Scenario**: Configuration exists but `EFFECTIVE_FROM_DATE` is in future.

**Handling**:
- Skip this configuration
- Check next level in hierarchy
- If no active configuration found, use default behavior

### Edge Case 7: Configuration Overlap in Date Ranges
**Scenario**: Multiple configurations at same level with overlapping date ranges.

**Handling**:
- Select configuration with most recent `CREATED_DATE`
- Log warning if multiple active configurations found
- Use selected configuration

### Edge Case 8: Combined Criteria with Partial Data
**Scenario**: Grouping by Warehouse + Material, but some materials missing warehouse or material data.

**Handling**:
- Use available criteria values
- For missing values, use 'UNKNOWN'
- Group key will be: `{WAREHOUSE}-{MATERIAL}` or `UNKNOWN-{MATERIAL}` or `{WAREHOUSE}-UNKNOWN`
- Log warnings for missing data

---

## Dependencies

### External Services
- **SAP EWM Tables**:
  - `/SCWM/T300` - Warehouse master
  - `/SCWM/VEHICLE` - Vehicle master
  - `/SCWM/ORDIM_O` - Order items (input)
  - `/SCDL/DB_PROCI_O` - Product information (input)

### Databases
- **Custom Configuration Table**: `ZSCWM_MPN_CONFIG` (to be created)
  - Structure:
    - `CONFIG_ID`: `CHAR32` (Primary Key)
    - `CONFIG_LEVEL`: `CHAR10` (GLOBAL, SITE, PLANT, DIVISION, MATERIAL)
    - `LEVEL_VALUE`: `CHAR40` (Site code, Plant code, Division code, Material number)
    - `GROUPING_CRITERIA`: `CHAR200` (Comma-separated: WH, M, PC, D)
    - `EFFECTIVE_FROM_DATE`: `DATS`
    - `EFFECTIVE_TO_DATE`: `DATS`
    - `IS_ACTIVE`: `CHAR1`
    - `CREATED_BY`: `SYUNAME`
    - `CREATED_DATE`: `DATS`
    - `CREATED_TIME`: `TIMS`
    - `MODIFIED_BY`: `SYUNAME`
    - `MODIFIED_DATE`: `DATS`
    - `MODIFIED_TIME`: `TIMS`

- **SAP Standard Tables**:
  - `MARA` - Material master (general data)
  - `MARC` - Material master (plant data)
  - `MVKE` - Material master (sales data)
  - `/SAPAPO/MARM` - Unit of measure conversion

### Function Modules
- `/SAPAPO/INC_CONVERT_GUIDS` - Convert material GUID to material number
- `CONVERSION_EXIT_ALPHA_INPUT` - Format alphanumeric fields
- `CONVERSION_EXIT_ALPHA_OUTPUT` - Format alphanumeric fields

### Events
- None (synchronous function call)

### Other
- **Custom Types**: 
  - `ZSCWM_MPN_GROUP_TT` - Table type for grouped materials
  - `ZSCWM_MPN_GROUP_ST` - Structure for group data
  - `ZSCWM_MPN_GROUP_SUMMARY_ST` - Structure for group summary
  - `ZEWM_ORDIM_O_TT` - Existing table type for order items
  - `ZEWM_DB_PROCI_O1_TT` - Existing table type for product info

---

## Performance

### Performance Requirements
- **Max Execution Time**: < 2 seconds for 100 materials
- **Max Execution Time**: < 5 seconds for 500 materials
- **Database Queries**: Optimize with proper indexes on `ZSCWM_MPN_CONFIG`
- **Memory Usage**: Efficient use of internal tables, avoid unnecessary copies

### Optimization Strategies
1. Use `FOR ALL ENTRIES` for reading master data (avoid nested loops)
2. Create database indexes on `ZSCWM_MPN_CONFIG`:
   - Primary index on `CONFIG_ID`
   - Secondary index on (`CONFIG_LEVEL`, `LEVEL_VALUE`, `IS_ACTIVE`, `EFFECTIVE_FROM_DATE`, `EFFECTIVE_TO_DATE`)
3. Cache material master data in internal tables
4. Use binary search for table lookups where possible
5. Minimize database roundtrips by batching reads

### Resource Constraints
- Function should complete within single SAP dialog step
- No long-running database operations
- No external system calls

---

## Security

### Authentication
- Function module is called from authorized SAP EWM function
- No additional authentication required (inherits caller's authorization)

### Authorization
- Read access to configuration table `ZSCWM_MPN_CONFIG` required
- Read access to material master (`MARA`, `MARC`, `MVKE`) required
- Read access to warehouse master (`/SCWM/T300`) required
- Authorization object: `Z_SCWM_MPN` (to be created) with activity '03' (Display)

### Data Protection
- No PII (Personally Identifiable Information) processed
- No sensitive data encryption required
- Standard SAP data protection applies

### Audit Trail
- Configuration ID applied is logged in `EV_CONFIG_ID`
- Configuration level applied is logged in `EV_CONFIG_LEVEL`
- All errors and warnings logged in `ET_RETURN`

---

## Integration with Existing Function Module

### Call Location in Z_FIORI_SWM_OB_TRK_PRINT_MPN

**Insert Point**: After line 1812 (after `gt_ordim_o_tt` is populated, before SmartForm call at line 1868)

**Code Insertion**:

```abap
*----------------------------------------------------------------------*
* Call new function module for configurable MPN grouping
*----------------------------------------------------------------------*
DATA: lt_grouped_materials TYPE zscwm_mpn_group_tt,
      lv_group_count TYPE i,
      lv_config_id TYPE char32,
      lv_config_level TYPE char10,
      lv_grouping_criteria TYPE char200,
      lt_return_group TYPE bapiret2_t,
      lw_return_group TYPE bapiret2.

* Check if configurable grouping is enabled
DATA: lv_use_config TYPE abap_bool VALUE 'X'.
* This can be controlled via customizing table or parameter

IF lv_use_config = abap_true.
  
  CALL FUNCTION 'Z_SCWM_MPN_GROUP_MATERIALS'
    EXPORTING
      iv_lgnum          = i_lgnum
      iv_veh_num        = gw_veh_num
      iv_report_no      = i_report_no
      it_ordim_o        = gt_ordim_o_tt
      it_db_proci_o     = gt_db_proci_o
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
    READ TABLE lt_return_group INTO lw_return_group 
      WITH KEY type = 'E'.
    IF sy-subrc = 0.
      * Error in grouping, use default behavior
      APPEND LINES OF lt_return_group TO et_return.
      CLEAR: lt_grouped_materials, lv_group_count.
    ELSE.
      * Grouping successful, process multiple MPNs
      * Append return messages
      APPEND LINES OF lt_return_group TO et_return.
      
      * Loop through groups and generate MPN for each
      DATA: lw_group TYPE zscwm_mpn_group_st,
            lv_mpn_sequence TYPE i.
      
      CLEAR lv_mpn_sequence.
      LOOP AT lt_grouped_materials INTO lw_group.
        lv_mpn_sequence = lv_mpn_sequence + 1.
        
        * Prepare data for SmartForm
        gt_ordim_o_tt = lw_group-it_ordim_o.
        gt_db_proci_o = lw_group-it_db_proci_o.
        
        * Update output filename with group identifier
        CONCATENATE i_report_no lw_group-group_id 'pdf' 
          INTO e_filename SEPARATED BY '.'.
        
        * Call SmartForm for this group
        * (Existing SmartForm call logic here)
        * ...
        
      ENDLOOP.
      
      * Exit after processing all groups
      RETURN.
    ENDIF.
  ENDIF.
ENDIF.

* If no configuration or grouping failed, continue with existing logic
* (Default: single MPN per vehicle)
```

### Modification Strategy
1. **Option 1 (Recommended)**: Wrap existing SmartForm call in loop for multiple groups
2. **Option 2**: Create separate SmartForm call for each group
3. **Option 3**: Modify SmartForm to handle grouped data structure

**Recommendation**: Use Option 1 - Loop through groups and call SmartForm for each group, updating filename and data for each iteration.

---

## Tests

### AT-001: Warehouse-wise Grouping (Happy Path)
**Test Scenario**: 
- Input: 10 materials from 3 different warehouses
- Configuration: Warehouse-wise grouping for site
- Expected: 3 groups created, each containing materials from one warehouse

**Test Data**:
- `IV_LGNUM`: '001'
- `IV_VEH_NUM`: 'V001'
- `IT_ORDIM_O`: 10 entries with different `VLPLA` values
- Configuration: Site level, `GROUPING_CRITERIA = 'WH'`

**Expected Result**:
- `EV_GROUP_COUNT = 3`
- `ET_GROUPED_MATERIALS` contains 3 entries
- Each group has materials from single warehouse
- Return type 'S'

### AT-002: Material-wise Grouping
**Test Scenario**:
- Input: 5 different materials
- Configuration: Material-wise grouping
- Expected: 5 groups created, one per material

**Test Data**:
- `IT_ORDIM_O`: 5 entries with different `MATID` values
- Configuration: Global level, `GROUPING_CRITERIA = 'M'`

**Expected Result**:
- `EV_GROUP_COUNT = 5`
- Each group contains one material type
- Return type 'S'

### AT-003: Combined Criteria (Warehouse + Material)
**Test Scenario**:
- Input: Materials from 2 warehouses, 3 materials each
- Configuration: Warehouse + Material grouping
- Expected: 6 groups (2 warehouses × 3 materials)

**Test Data**:
- `IT_ORDIM_O`: 6 entries (2 warehouses, 3 materials)
- Configuration: `GROUPING_CRITERIA = 'WH,M'`

**Expected Result**:
- `EV_GROUP_COUNT = 6`
- Each group has materials from one warehouse and one material type
- Return type 'S'

### AT-004: No Configuration (Default Behavior)
**Test Scenario**:
- Input: Materials from multiple warehouses
- Configuration: No active configuration found
- Expected: Single group with all materials

**Test Data**:
- `IT_ORDIM_O`: 10 entries
- No configuration in table or `IV_USE_CONFIG = ' '`

**Expected Result**:
- `EV_GROUP_COUNT = 1`
- Single group contains all materials
- `EV_GROUPING_CRITERIA = 'DEFAULT'`
- Return type 'S'

### AT-005: Missing Grouping Criteria Value
**Test Scenario**:
- Input: Materials, some missing plant code
- Configuration: Plant code-wise grouping
- Expected: Materials with plant code grouped, missing ones in 'UNKNOWN' group

**Test Data**:
- `IT_ORDIM_O`: 5 entries, 3 with plant code, 2 without
- Configuration: `GROUPING_CRITERIA = 'PC'`

**Expected Result**:
- `EV_GROUP_COUNT >= 2` (at least one group for plant codes, one for UNKNOWN)
- Warning messages in `ET_RETURN` for missing plant codes
- Return type 'S' with warnings

### AT-006: Invalid Warehouse Number
**Test Scenario**:
- Input: Invalid warehouse number
- Expected: Error returned, function exits

**Test Data**:
- `IV_LGNUM`: '999' (non-existent)

**Expected Result**:
- Error message in `ET_RETURN`: "Warehouse number 999 is invalid"
- Return type 'E'
- `ET_GROUPED_MATERIALS` empty

### AT-007: Empty Material List
**Test Scenario**:
- Input: Empty material table
- Expected: Error returned

**Test Data**:
- `IT_ORDIM_O`: Initial (empty)

**Expected Result**:
- Error message: "No materials provided for grouping"
- Return type 'E'
- Function exits

### AT-008: Configuration Priority (Material > Plant)
**Test Scenario**:
- Input: Materials with both material-specific and plant-specific configurations
- Expected: Material-specific configuration applied

**Test Data**:
- Material 'M001' has material-specific config: `GROUPING_CRITERIA = 'M'`
- Plant 'P001' has plant-specific config: `GROUPING_CRITERIA = 'PC'`
- Material 'M001' belongs to plant 'P001'

**Expected Result**:
- Material-specific configuration applied
- `EV_CONFIG_LEVEL = 'MATERIAL'`
- `EV_CONFIG_ID` = Material-specific config ID

### AT-009: All Materials in Same Group
**Test Scenario**:
- Input: All materials from same warehouse
- Configuration: Warehouse-wise grouping
- Expected: Single group (valid scenario)

**Test Data**:
- `IT_ORDIM_O`: 5 entries, all with same `VLPLA`
- Configuration: `GROUPING_CRITERIA = 'WH'`

**Expected Result**:
- `EV_GROUP_COUNT = 1`
- Single group contains all materials
- Return type 'S'

### AT-010: Performance Test (100 Materials)
**Test Scenario**:
- Input: 100 materials from 10 warehouses
- Configuration: Warehouse-wise grouping
- Expected: Function completes in < 2 seconds

**Test Data**:
- `IT_ORDIM_O`: 100 entries
- Configuration: `GROUPING_CRITERIA = 'WH'`

**Expected Result**:
- `EV_GROUP_COUNT = 10`
- Execution time < 2 seconds
- Return type 'S'

---

## Configuration Table Structure

### ZSCWM_MPN_CONFIG Table Definition

```abap
* Table: ZSCWM_MPN_CONFIG
* Description: Configuration table for MPN grouping criteria

FIELD-SYMBOLS: <fs_config> TYPE zscwm_mpn_config.

DATA: BEGIN OF ls_config,
  config_id          TYPE char32,           " Primary Key
  config_level       TYPE char10,           " GLOBAL, SITE, PLANT, DIVISION, MATERIAL
  level_value        TYPE char40,           " Site code, Plant code, etc.
  grouping_criteria  TYPE char200,          " Comma-separated: WH,M,PC,D
  effective_from_date TYPE dats,
  effective_to_date   TYPE dats,
  is_active          TYPE char1,            " X = Active, ' ' = Inactive
  created_by         TYPE syuname,
  created_date       TYPE dats,
  created_time       TYPE tims,
  modified_by        TYPE syuname,
  modified_date      TYPE dats,
  modified_time      TYPE tims,
END OF ls_config.
```

### Sample Configuration Data

| CONFIG_ID | CONFIG_LEVEL | LEVEL_VALUE | GROUPING_CRITERIA | EFFECTIVE_FROM | EFFECTIVE_TO | IS_ACTIVE |
|-----------|--------------|-------------|-------------------|----------------|--------------|-----------|
| CONFIG-001 | GLOBAL | - | WH | 2025-01-01 | - | X |
| CONFIG-002 | SITE | RPL-DAH | WH,M | 2025-01-01 | - | X |
| CONFIG-003 | PLANT | P001 | PC | 2025-01-01 | 2025-12-31 | X |
| CONFIG-004 | DIVISION | D001 | D | 2025-01-01 | - | X |
| CONFIG-005 | MATERIAL | M001 | M | 2025-01-01 | - | X |

---

## Appendix

### Custom Type Definitions

```abap
* Type: ZSCWM_MPN_GROUP_ST
TYPES: BEGIN OF zscwm_mpn_group_st,
  group_id            TYPE char20,
  group_sequence      TYPE i,
  group_criteria_value TYPE char200,
  group_criteria_desc  TYPE char200,
  it_ordim_o          TYPE zewm_ordim_o_tt,
  it_db_proci_o       TYPE zewm_db_proci_o1_tt,
  group_summary       TYPE zscwm_mpn_group_summary_st,
END OF zscwm_mpn_group_st.

* Type: ZSCWM_MPN_GROUP_TT
TYPES: zscwm_mpn_group_tt TYPE STANDARD TABLE OF zscwm_mpn_group_st.

* Type: ZSCWM_MPN_GROUP_SUMMARY_ST
TYPES: BEGIN OF zscwm_mpn_group_summary_st,
  total_materials     TYPE i,
  total_quantity     TYPE quan,
  total_quantity_alt TYPE quan,
  warehouse_code     TYPE /scwm/de_who,
  material_number    TYPE matnr,
  plant_code         TYPE werks_d,
  division_code      TYPE spart,
END OF zscwm_mpn_group_summary_st.
```

### Message Class: ZSCWM_MPN

| Message Number | Type | Text |
|---------------|------|------|
| 001 | E | Warehouse number &1 is invalid |
| 002 | E | Vehicle number &1 is invalid |
| 003 | E | No materials provided for grouping |
| 004 | E | Invalid configuration &1 found |
| 005 | W | Material &1 missing &2 value for grouping |
| 006 | E | No groups created after grouping |
| 007 | E | Error reading configuration table |
| 008 | W | Error reading material master data |
| 009 | S | Materials grouped successfully. &1 MPN(s) to be generated. |

---

## Revision History

| Version | Date | Author | Description |
|---------|------|--------|-------------|
| 1.0 | 2025-11-06 | [To be filled] | Initial FS creation based on BRD |

---

**Document Status**: Draft for Review

**Next Steps**:
1. Review and approve FS
2. Create custom table `ZSCWM_MPN_CONFIG`
3. Create custom types in DDIC
4. Implement function module
5. Unit testing
6. Integration testing with existing FM

