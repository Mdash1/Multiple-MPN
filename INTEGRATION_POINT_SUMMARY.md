# Integration Point Summary: Z_SCWM_MPN_GROUP_MATERIALS

## Exact Insertion Location

**File**: `Z_FIORI_SWM_OB_TRK_PRINT_MPN.md` (Function Module)

**Insert After**: Line 1866 (after product code logic block ends)

**Insert Before**: Line 1868 (before SmartForm call `CALL FUNCTION gw_fm_name`)

---

## Context Around Insertion Point

### Before Insertion (Line 1866):
```abap
    ENDIF."IF sy-subrc = 0."FUNCTION_EXISTS
    ENDIF."IF sy-subrc = 0."RFC_PING
    ********** ENDIF.

-   Added by SN Das CD:8079871(End) CALL FUNCTION gw_fm_name EXPORTING
```

### After Insertion (Line 1868):
```abap
    ********** ENDIF.

*----------------------------------------------------------------------*
* Call grouping function module for configurable MPN grouping
* [INSERT INTEGRATION CODE HERE]
*----------------------------------------------------------------------

-   Added by SN Das CD:8079871(End) CALL FUNCTION gw_fm_name EXPORTING
```

---

## Step-by-Step Integration Instructions

### Step 1: Add Data Declarations

**Location**: Around line 320, in the DATA declaration section

**Find this section**:
```abap
DATA: gw_rfc_dest TYPE tvarv_val, gw_open_qty_so TYPE kwmeng,
gw_open_qty_sto TYPE lfimg, gw_unit TYPE meins, gw_vbeln TYPE vbeln.
```

**Add after it**:
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

### Step 2: Insert Main Integration Code

**Location**: After line 1866, before line 1868

**Find this exact location**:
```abap
    ********** ENDIF.

-   Added by SN Das CD:8079871(End) CALL FUNCTION gw_fm_name EXPORTING
```

**Insert the complete integration code block** (see `INTEGRATION_CODE_ECC.md` for full code)

---

## Visual Flow Diagram

```
Line 1812-1826: Build gt_ordim_o_tt
    │
    ├─ LOOP AT gt_ordim_o INTO gw_ordim_o
    ├─ MOVE-CORRESPONDING gw_ordim_o TO gw_ordim_o_tt
    ├─ [Storage bin logic]
    ├─ [Polyster flag logic]
    └─ APPEND gw_ordim_o_tt TO gt_ordim_o_tt
    │
Line 1828-1831: Set control parameters
    │
Line 1835-1866: Product code logic
    │
    └─ ENDIF.  ← Line 1866
    │
    ╔══════════════════════════════════════════════════════╗
    ║  INSERT INTEGRATION CODE HERE (After line 1866)     ║
    ║                                                      ║
    ║  1. Call Z_SCWM_MPN_GROUP_MATERIALS                  ║
    ║  2. Check EV_GROUP_COUNT                             ║
    ║  3. If > 1: Loop through groups, generate PDFs      ║
    ║  4. If = 1: Use filtered data, continue below        ║
    ║  5. If error: Fallback to default behavior           ║
    ╚══════════════════════════════════════════════════════╝
    │
Line 1868: CALL FUNCTION gw_fm_name (SmartForm)
    │
    └─ [Existing SmartForm call continues]
```

---

## Key Integration Logic

### Scenario 1: Multiple Groups (EV_GROUP_COUNT > 1)
```
1. Loop through each group in lt_grouped_materials
2. Assign filtered data from group to SmartForm tables
3. Call SmartForm for each group
4. Convert OTF to PDF for each group
5. Return last group's PDF (or merge all PDFs)
6. RETURN (exit function)
```

### Scenario 2: Single Group (EV_GROUP_COUNT = 1)
```
1. Read first group from lt_grouped_materials
2. Assign filtered data to SmartForm tables
3. Continue with existing SmartForm call at line 1868
4. Existing logic handles PDF generation
```

### Scenario 3: Error or No Configuration
```
1. Log warning/error
2. Continue with existing logic (default behavior)
3. Existing SmartForm call at line 1868 uses original data
```

---

## Data Flow

### Input to Grouping Function:
- `gt_ordim_o_tt` - All materials
- `gt_db_proci_o` - Product information
- `gt_db_proch_o` - Product header
- `gt_db_refdoc` - Reference documents
- `gt_it_waveitm` - Wave items
- `gt_but000`, `gt_but020`, `gt_adrc`, `gt_t005u` - Business partner data
- `gt_plant`, `gt_lnumt` - Warehouse data
- `gt_delv_st` - Delivery details

### Output from Grouping Function:
- `lt_grouped_materials` - Grouped data (one entry per MPN)
- `lv_group_count` - Number of groups (multiple print indicator)
- `lv_config_id`, `lv_config_level`, `lv_grouping_criteria` - Configuration info

### Each Group Contains:
- `it_ordim_o` - Filtered materials for the group
- `it_db_proci_o` - Filtered product info
- `it_db_proch_o` - Filtered product header
- `it_db_refdoc` - Filtered reference docs
- `it_it_waveitm` - Filtered wave items
- `it_but000`, `it_but020`, `it_adrc`, `it_t005u` - Filtered BP data
- `it_plant`, `it_lnumt` - Filtered warehouse data
- `it_delv_st` - Filtered delivery details
- `group_id` - Unique group identifier
- `group_sequence` - Sequence number
- `group_summary` - Group summary information

---

## Critical Points

1. **Data Preparation**: All data must be collected before calling grouping function
2. **Error Handling**: Always fallback to default behavior on errors
3. **Backward Compatibility**: Single group scenario uses existing SmartForm call
4. **Multiple PDFs**: Currently returns last PDF; can be enhanced for merging
5. **ECC Compatibility**: All code uses traditional ABAP syntax (no inline declarations)

---

## Testing Scenarios

### Test Case 1: Single Group
- **Input**: Materials that group into single MPN
- **Expected**: `EV_GROUP_COUNT = 1`, continues with existing logic
- **Verify**: PDF generated with original filename

### Test Case 2: Multiple Groups
- **Input**: Materials that group into multiple MPNs
- **Expected**: `EV_GROUP_COUNT > 1`, loop generates PDFs
- **Verify**: PDF generated with group identifier in filename

### Test Case 3: Grouping Error
- **Input**: Invalid configuration or data error
- **Expected**: Error logged, fallback to default behavior
- **Verify**: Single MPN generated with original logic

### Test Case 4: No Configuration
- **Input**: No active configuration found
- **Expected**: Default behavior, single MPN
- **Verify**: Original filename and single PDF

---

## Files to Reference

1. **INTEGRATION_CODE_ECC.md** - Complete integration code
2. **TS_Z_SCWM_MPN_GROUP_MATERIALS.md** - Technical specification
3. **INTEGRATION_SUMMARY.md** - Integration overview

---

**End of Summary**

