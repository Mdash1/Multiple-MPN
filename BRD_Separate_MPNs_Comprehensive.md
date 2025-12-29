# Business Requirement Document (BRD)

## Enable Configurable Warehouse-Wise MPN Generation for Multi-Warehouse, Multi-Product Dispatches Under a Single Vehicle at RPL Dahej

---

**Classification:** Reliance Industries Limited – Internal  
**Applicability:** RPL Dahej Manufacturing Site  
**Issue Date:** 06-Nov-2025  
**Issuing Authority:** Head Central G&I  
**Content Owner:** Head BPM  

---

## Use and Interpretation of this Document

This document is classified as **Reliance Industries Limited Internal**. Distribution is intended for authorized recipients only. The content is proprietary and governed by Reliance Industries Limited's Code of Conduct.

---

## Table of Contents

1. Document Versioning  
2. Stakeholders and Ownership  
3. Business Requirement Details  
4. Current State Analysis  
5. Business Requirements Specification  
6. Functional Requirements  
7. Business Rules  
8. Data Requirements  
9. User Stories and Use Cases  
10. Process Flow  
11. Configuration Requirements  
12. Validation Rules  
13. Error Scenarios and Handling  
14. Success Criteria  
15. Edge Cases and Special Scenarios  
16. Integration Requirements  
17. Non-Functional Requirements  
18. Risks & Controls  
19. Dependencies  
20. Testing Requirements  
21. Instructions  

---

## 1. Document Versioning

| Version | Created By | Reviewed By | Date Created | Change Description |
|------|-----------|------------|--------------|-------------------|
| 1 | Preethika Manoj | Chandrakant Shinde | 06-11-2025 | Enable Separate MPNs for Multi-Warehouse, Multi-Product Dispatches Under a Single Vehicle at RPL Dahej |
| 2 | [To be filled] | [To be filled] | [To be filled] | Enhanced with configuration requirements, detailed business rules, and comprehensive specifications |

---

## 2. Stakeholders and Ownership

| Role | Name / Team |
|----|------------|
| Business Requirement Sponsor | PETCHEM |
| Project Lead | Amrut Urkude |
| Process Owner | Amit Damani |
| IT Process SPOC | Lalit Modi |
| Project Team Member | Satish Bhagwatkar |
| SAP EWM Technical Lead | [To be filled] |
| Configuration Administrator | [To be filled] |

---

## 3. Business Requirement Details

### Business Requirement Summary

At **RPL Dahej**, the current dispatch planning system generates **one MPN (Material Pickup Note) per vehicle** based on manufacturing site and vehicle combination. This creates operational challenges when a single vehicle needs to dispatch materials from multiple warehouses with different products, as warehouse staff cannot efficiently organize picking operations.

The requirement is to enable **configurable MPN generation** that supports **multiple MPNs per vehicle** based on configurable grouping criteria (warehouse-wise, material-wise, plant code-wise, division-wise, or combinations thereof). This will enable efficient picking, better tracking, and improved retrieval operations at warehouses.

### Business Objective

Enable flexible, configurable MPN generation from SAP EWM that allows creation of separate MPNs for different warehouses, materials, plants, divisions, or combinations thereof, under a single vehicle dispatch, while maintaining data integrity and operational efficiency.

### Impacted Value Scenario

- **O2C (Order to Cash)**: Dispatch Process

### Impacted Function

- **Dispatch Planning**
- **Warehouse Operations (Picking)**
- **Material Tracking**
- **Vehicle Loading**

### Expected Benefits

- **Ease of Picking**: Warehouse staff can organize picking operations warehouse-wise or material-wise
- **Better Tracking and Retrieval**: Separate MPNs enable better tracking of materials by warehouse/product
- **Operational Efficiency**: Reduced picking time and errors
- **Flexibility**: Configurable grouping allows adaptation to different operational scenarios
- **Improved Audit Trail**: Better traceability of materials by warehouse/product combination

### Business Impact

- **High**: Improves warehouse operations efficiency
- **Medium**: Reduces picking errors and material mismatches
- **Medium**: Enhances tracking and audit capabilities

---

## 4. Current State Analysis

### Current Behavior

1. **MPN Generation**: System generates **one MPN per vehicle** based on:
   - Manufacturing Site (e.g., RPL Dahej)
   - Vehicle Number/ID

2. **Limitations**:
   - All materials for a vehicle are grouped into a single MPN regardless of:
     - Source warehouse
     - Material/product type
     - Plant code
     - Division
   - Warehouse staff must manually segregate materials during picking
   - Difficult to track materials by warehouse or product
   - Higher risk of material mismatch during picking

3. **Current System**: SAP EWM (Extended Warehouse Management)

### Current Process Flow

1. Dispatch planning is created for a vehicle
2. Materials from multiple warehouses/products are assigned to the vehicle
3. System generates **one MPN** for the entire vehicle
4. Warehouse staff receives single MPN and must manually organize picking

---

## 5. Business Requirements Specification

| Sl. No | Requirement Description | AOP Goal | Requirement Type | Priority | Impacted Process |
|-----|------------------------|---------|------------------|----------|-----------------|
| 1 | Enable Configurable MPN Generation for Multi-Warehouse Dispatches | Process Improvement | IT Application | High | Dispatch Planning |
| 2 | Support Warehouse-wise MPN Grouping | Process Improvement | IT Application | High | Dispatch Planning |
| 3 | Support Material-wise MPN Grouping | Process Improvement | IT Application | Medium | Dispatch Planning |
| 4 | Support Plant Code-wise MPN Grouping | Process Improvement | IT Application | Medium | Dispatch Planning |
| 5 | Support Division-wise MPN Grouping | Process Improvement | IT Application | Medium | Dispatch Planning |
| 6 | Support Combined Criteria MPN Grouping (e.g., Warehouse + Material, Plant + Division) | Process Improvement | IT Application | Medium | Dispatch Planning |
| 7 | Provide Configuration Management Interface | Process Improvement | IT Application | High | System Administration |
| 8 | Maintain Backward Compatibility with Existing Single MPN per Vehicle | Process Improvement | IT Application | High | Dispatch Planning |
| 9 | Enable MPN Tracking and Reporting by Grouping Criteria | Process Improvement | IT Application | Medium | Reporting |

---

## 6. Functional Requirements

### FR-1: Configurable MPN Grouping

**Description**: System shall support configurable grouping criteria for MPN generation.

**Grouping Criteria Options**:
1. **Warehouse-wise**: One MPN per warehouse per vehicle
2. **Material-wise**: One MPN per material per vehicle
3. **Plant Code-wise**: One MPN per plant code per vehicle
4. **Division-wise**: One MPN per division per vehicle
5. **Combined Criteria**:
   - Warehouse + Material
   - Warehouse + Plant Code
   - Warehouse + Division
   - Plant Code + Division
   - Plant Code + Material
   - Division + Material
   - Warehouse + Plant Code + Division
   - Warehouse + Plant Code + Material
   - Warehouse + Division + Material
   - Plant Code + Division + Material
   - Warehouse + Plant Code + Division + Material

**Priority**: High

### FR-2: Configuration Management

**Description**: System shall provide ability to configure MPN grouping rules.

**Requirements**:
- Configuration can be set at multiple levels:
  - **Global Level**: Default configuration for all dispatches
  - **Site Level**: Configuration specific to manufacturing site (e.g., RPL Dahej)
  - **Plant Level**: Configuration specific to plant code
  - **Division Level**: Configuration specific to division
  - **Material Level**: Configuration specific to material (optional)
- Configuration hierarchy: Material > Division > Plant > Site > Global
- Configuration changes should be effective immediately or from a specified effective date
- Configuration history/audit trail should be maintained

**Priority**: High

### FR-3: MPN Generation Logic

**Description**: System shall generate MPNs based on configured grouping criteria.

**Requirements**:
- When dispatch planning is created for a vehicle with materials from multiple warehouses/products:
  - System shall read applicable configuration
  - System shall group materials based on configured criteria
  - System shall generate separate MPN for each group
  - Each MPN shall contain:
    - Vehicle information
    - Grouping criteria values (e.g., warehouse code, material code)
    - List of materials in that group
    - Quantities
    - Source warehouse details
- All MPNs for a vehicle shall be linked to the same vehicle dispatch

**Priority**: High

### FR-4: Data Integration with SAP EWM

**Description**: System shall integrate with SAP EWM for MPN generation.

**Requirements**:
- Read dispatch planning data from SAP EWM
- Read warehouse, material, plant, division master data
- Generate MPNs in SAP EWM format
- Update dispatch status in SAP EWM
- Maintain synchronization between systems

**Priority**: High

### FR-5: Backward Compatibility

**Description**: System shall maintain backward compatibility.

**Requirements**:
- If no configuration is set, system shall default to current behavior (one MPN per vehicle)
- Existing single MPN per vehicle functionality shall continue to work
- Migration path for existing data/processes

**Priority**: High

### FR-6: MPN Tracking and Reporting

**Description**: System shall enable tracking and reporting of MPNs by grouping criteria.

**Requirements**:
- Track MPNs by warehouse, material, plant, division
- Generate reports showing MPN distribution by grouping criteria
- Enable search/filter by grouping criteria values

**Priority**: Medium

---

## 7. Business Rules

### BR-1: MPN Grouping Rule

**Rule**: Materials shall be grouped into separate MPNs based on the configured grouping criteria.

**Example**:
- If configuration is "Warehouse-wise":
  - Vehicle V001 has materials from Warehouse W001 (Material M001, M002) and Warehouse W002 (Material M003)
  - System generates: MPN-001 for W001 (M001, M002) and MPN-002 for W002 (M003)

### BR-2: Configuration Priority Rule

**Rule**: More specific configuration overrides less specific configuration.

**Priority Order**:
1. Material-specific configuration (highest priority)
2. Division-specific configuration
3. Plant-specific configuration
4. Site-specific configuration
5. Global configuration (lowest priority)

### BR-3: Minimum Group Size Rule

**Rule**: If a group has zero materials after grouping, no MPN shall be generated for that group.

**Exception**: System shall log a warning if expected groups have no materials.

### BR-4: MPN Naming Convention

**Rule**: MPN numbers/identifiers shall be unique and may include grouping criteria identifiers for traceability.

**Format Options**:
- [Vehicle-ID]-[Grouping-Criteria-Value]-[Sequence]
- Example: V001-W001-001, V001-W002-001

### BR-5: Vehicle-MPN Relationship

**Rule**: All MPNs generated for a single vehicle dispatch shall be linked to that vehicle.

**Requirement**: System shall maintain parent-child relationship between vehicle dispatch and MPNs.

### BR-6: Material Assignment Rule

**Rule**: Each material line item shall be assigned to exactly one MPN based on grouping criteria.

**Exception**: If material has multiple grouping criteria values (e.g., stored in multiple warehouses), system shall use primary/default value or prompt for selection.

### BR-7: Configuration Effective Date Rule

**Rule**: Configuration changes shall be effective from the specified effective date or immediately if no date specified.

**Requirement**: System shall not apply future-dated configurations to current dispatches unless explicitly allowed.

---

## 8. Data Requirements

### Input Data

| Data Element | Description | Source | Required | Example |
|-------------|-------------|--------|----------|---------|
| Vehicle ID/Number | Unique identifier for vehicle | Dispatch Planning | Yes | V001 |
| Manufacturing Site | Site code (e.g., RPL Dahej) | Dispatch Planning | Yes | RPL-DAH |
| Material Number | Material/product code | Dispatch Planning | Yes | M001 |
| Material Description | Material description | Material Master | No | Product A |
| Warehouse Code | Source warehouse identifier | Dispatch Planning | Yes | W001 |
| Warehouse Name | Warehouse name | Warehouse Master | No | Warehouse North |
| Plant Code | Plant identifier | Material Master / Dispatch Planning | Yes | P001 |
| Division Code | Division identifier | Material Master / Dispatch Planning | Yes | D001 |
| Quantity | Material quantity to dispatch | Dispatch Planning | Yes | 100 |
| Unit of Measure | Unit of measure | Material Master | Yes | KG |
| Dispatch Date | Planned dispatch date | Dispatch Planning | Yes | 2025-11-10 |
| Configuration ID | Active configuration identifier | Configuration Table | Yes | CONFIG-001 |
| Grouping Criteria | Selected grouping criteria | Configuration | Yes | WAREHOUSE |

### Output Data

| Data Element | Description | Format | Required |
|-------------|-------------|--------|----------|
| MPN Number | Unique MPN identifier | Alphanumeric | Yes |
| MPN Date | MPN creation date | Date | Yes |
| Vehicle ID | Associated vehicle | Alphanumeric | Yes |
| Grouping Criteria Value | Value of grouping criteria (e.g., warehouse code) | Alphanumeric | Yes |
| Material List | List of materials in MPN | Table/Array | Yes |
| Material Details | Material number, description, quantity | Table/Array | Yes |
| Warehouse Code | Source warehouse | Alphanumeric | Yes |
| MPN Status | Status of MPN (Created, Picked, Dispatched) | Enum | Yes |
| MPN Sequence | Sequence number for MPNs of same vehicle | Numeric | Yes |

### Configuration Data

| Data Element | Description | Format | Required |
|-------------|-------------|--------|----------|
| Configuration ID | Unique configuration identifier | Alphanumeric | Yes |
| Configuration Level | Global, Site, Plant, Division, Material | Enum | Yes |
| Level Value | Value at configuration level (e.g., site code, plant code) | Alphanumeric | Conditional |
| Grouping Criteria | Selected grouping criteria | Enum/Array | Yes |
| Effective From Date | Date from which configuration is effective | Date | Yes |
| Effective To Date | Date until which configuration is effective | Date | No |
| Is Active | Whether configuration is active | Boolean | Yes |
| Created By | User who created configuration | Alphanumeric | Yes |
| Created Date | Configuration creation date | Date | Yes |
| Modified By | User who last modified configuration | Alphanumeric | No |
| Modified Date | Last modification date | Date | No |

---

## 9. User Stories and Use Cases

### User Story 1: Warehouse Manager

**As a** Warehouse Manager  
**I want to** receive separate MPNs for each warehouse when a vehicle has materials from multiple warehouses  
**So that** I can organize picking operations warehouse-wise and reduce picking errors

**Acceptance Criteria**:
- When vehicle has materials from 3 warehouses, system generates 3 separate MPNs
- Each MPN clearly identifies the warehouse
- Materials are correctly grouped by warehouse

### User Story 2: Dispatch Planner

**As a** Dispatch Planner  
**I want to** configure MPN grouping rules based on operational needs  
**So that** I can adapt the system to different dispatch scenarios

**Acceptance Criteria**:
- Can set configuration at site, plant, or division level
- Can select from available grouping criteria
- Configuration changes are effective immediately or from specified date
- Can view configuration history

### User Story 3: Warehouse Staff

**As a** Warehouse Staff (Picker)  
**I want to** receive MPNs grouped by material or warehouse  
**So that** I can efficiently pick materials without confusion

**Acceptance Criteria**:
- MPN clearly shows grouping criteria (warehouse/material)
- Materials in MPN belong to same group
- Can easily identify which materials to pick together

### User Story 4: System Administrator

**As a** System Administrator  
**I want to** manage MPN grouping configurations  
**So that** I can maintain system flexibility and support different business scenarios

**Acceptance Criteria**:
- Can create, update, deactivate configurations
- Can set configuration hierarchy
- Can view audit trail of configuration changes
- Can test configuration before applying

### Use Case 1: Multi-Warehouse Dispatch with Warehouse-wise Grouping

**Actor**: Dispatch Planning System, Warehouse Manager

**Preconditions**:
- Configuration is set to "Warehouse-wise" grouping for RPL Dahej
- Dispatch planning created for Vehicle V001
- Vehicle has materials from Warehouse W001, W002, W003

**Main Flow**:
1. System reads dispatch planning for Vehicle V001
2. System reads active configuration (Warehouse-wise)
3. System groups materials by warehouse:
   - Group 1: Materials from W001
   - Group 2: Materials from W002
   - Group 3: Materials from W003
4. System generates MPN-001 for W001, MPN-002 for W002, MPN-003 for W003
5. System links all MPNs to Vehicle V001
6. System sends MPNs to respective warehouses
7. Warehouse managers receive separate MPNs

**Postconditions**:
- Three MPNs created and linked to Vehicle V001
- Each warehouse receives its MPN
- Materials correctly grouped

### Use Case 2: Configuration Management

**Actor**: System Administrator

**Preconditions**:
- User has configuration management access
- Current configuration exists

**Main Flow**:
1. Administrator accesses configuration management interface
2. Administrator selects configuration level (e.g., Site: RPL Dahej)
3. Administrator selects grouping criteria (e.g., Warehouse + Material)
4. Administrator sets effective date
5. Administrator saves configuration
6. System validates configuration
7. System saves configuration with audit trail
8. System applies configuration to new dispatches

**Postconditions**:
- Configuration saved and active
- Audit trail created
- Configuration available for MPN generation

---

## 10. Process Flow

### Process 1: MPN Generation Flow

```
1. START: Dispatch Planning Created for Vehicle
   │
   ├─> 2. Read Vehicle Details (Vehicle ID, Site, Materials List)
   │
   ├─> 3. Read Active Configuration
   │     │
   │     ├─> 3.1 Check Material-specific configuration
   │     ├─> 3.2 Check Division-specific configuration
   │     ├─> 3.3 Check Plant-specific configuration
   │     ├─> 3.4 Check Site-specific configuration
   │     └─> 3.5 Check Global configuration
   │
   ├─> 4. Determine Grouping Criteria (from configuration)
   │
   ├─> 5. Group Materials Based on Criteria
   │     │
   │     ├─> 5.1 For each material, extract grouping criteria values
   │     ├─> 5.2 Group materials with same criteria values
   │     └─> 5.3 Validate groups (ensure no empty groups)
   │
   ├─> 6. Generate MPNs
   │     │
   │     ├─> 6.1 For each group, create MPN
   │     ├─> 6.2 Assign MPN number (unique)
   │     ├─> 6.3 Populate MPN with materials in group
   │     ├─> 6.4 Link MPN to vehicle
   │     └─> 6.5 Set MPN status to "Created"
   │
   ├─> 7. Send MPNs to SAP EWM / Warehouse Systems
   │
   ├─> 8. Update Dispatch Status
   │
   └─> 9. END: MPNs Generated and Distributed
```

### Process 2: Configuration Management Flow

```
1. START: Administrator Accesses Configuration Management
   │
   ├─> 2. Select Configuration Level (Global/Site/Plant/Division/Material)
   │
   ├─> 3. If Level requires Value, Enter Level Value (e.g., Site Code)
   │
   ├─> 4. Select Grouping Criteria
   │     │
   │     ├─> 4.1 Single Criteria (Warehouse/Material/Plant/Division)
   │     └─> 4.2 Combined Criteria (select multiple)
   │
   ├─> 5. Set Effective Dates (From Date, To Date - optional)
   │
   ├─> 6. Validate Configuration
   │     │
   │     ├─> 6.1 Check for conflicts with existing configurations
   │     ├─> 6.2 Validate date ranges
   │     └─> 6.3 Validate grouping criteria combination
   │
   ├─> 7. Save Configuration
   │     │
   │     ├─> 7.1 Create configuration record
   │     ├─> 7.2 Set IsActive = true
   │     ├─> 7.3 Record Created By, Created Date
   │     └─> 7.4 Create audit trail entry
   │
   └─> 8. END: Configuration Saved and Active
```

---

## 11. Configuration Requirements

### Configuration Levels

| Level | Description | Level Value Required | Example |
|-------|-------------|---------------------|---------|
| Global | Applies to all dispatches | No | Default configuration |
| Site | Applies to specific manufacturing site | Yes (Site Code) | RPL-DAH |
| Plant | Applies to specific plant | Yes (Plant Code) | P001 |
| Division | Applies to specific division | Yes (Division Code) | D001 |
| Material | Applies to specific material | Yes (Material Number) | M001 |

### Grouping Criteria Options

| Criteria Code | Criteria Name | Description | Applicable Combinations |
|--------------|---------------|-------------|-------------------------|
| WH | Warehouse | Group by warehouse code | Can combine with M, PC, D |
| M | Material | Group by material number | Can combine with WH, PC, D |
| PC | Plant Code | Group by plant code | Can combine with WH, M, D |
| D | Division | Group by division code | Can combine with WH, M, PC |

### Configuration Storage

- Configuration shall be stored in a database table with following structure:
  - Configuration ID (Primary Key)
  - Configuration Level
  - Level Value (nullable, based on level)
  - Grouping Criteria (JSON or comma-separated)
  - Effective From Date
  - Effective To Date (nullable)
  - Is Active (boolean)
  - Audit fields (Created By, Created Date, Modified By, Modified Date)

### Configuration Access Control

- Only authorized users (System Administrators, Configuration Administrators) can create/modify configurations
- Read access for Dispatch Planners and Warehouse Managers
- Audit trail for all configuration changes

### Configuration Validation Rules

1. **Date Range Validation**: Effective To Date must be after Effective From Date
2. **Overlap Validation**: For same level and level value, configurations should not overlap in date ranges (unless explicitly allowed for testing)
3. **Criteria Validation**: Combined criteria should be valid (e.g., cannot combine same criteria twice)
4. **Level Value Validation**: Level value must exist in master data (e.g., site code, plant code must be valid)

---

## 12. Validation Rules

### Input Validation

| Field | Validation Rule | Error Message |
|-------|----------------|---------------|
| Vehicle ID | Must not be null/empty | "Vehicle ID is required" |
| Manufacturing Site | Must be valid site code | "Invalid manufacturing site" |
| Material Number | Must exist in material master | "Material not found in master data" |
| Warehouse Code | Must exist in warehouse master | "Invalid warehouse code" |
| Plant Code | Must exist in plant master | "Invalid plant code" |
| Division Code | Must exist in division master | "Invalid division code" |
| Quantity | Must be greater than zero | "Quantity must be greater than zero" |
| Configuration ID | Must exist and be active | "Configuration not found or inactive" |

### Business Validation

| Validation | Rule | Error Message |
|-----------|------|---------------|
| Material-Warehouse Mapping | Material must be available in specified warehouse | "Material not available in warehouse" |
| Grouping Criteria Completeness | All materials must have values for selected grouping criteria | "Missing grouping criteria value for material" |
| MPN Generation Feasibility | At least one group must have materials | "No materials to group for MPN generation" |
| Configuration Applicability | Active configuration must exist for dispatch context | "No applicable configuration found" |

### Data Integrity Validation

| Validation | Rule | Error Message |
|-----------|------|---------------|
| MPN Uniqueness | MPN number must be unique | "MPN number already exists" |
| Vehicle-MPN Linkage | All MPNs must be linked to valid vehicle | "Invalid vehicle reference" |
| Material Assignment | Each material must be assigned to exactly one MPN | "Material assignment error" |

---

## 13. Error Scenarios and Handling

| Error Scenario | Error Code | Error Message | Handling Approach |
|---------------|------------|---------------|-------------------|
| Configuration Not Found | CONFIG-001 | "No active configuration found for dispatch context" | Use default configuration (one MPN per vehicle) or reject dispatch |
| Invalid Grouping Criteria | CONFIG-002 | "Invalid grouping criteria specified in configuration" | Reject configuration, log error, notify administrator |
| Missing Grouping Criteria Value | DATA-001 | "Material {MaterialNumber} missing {Criteria} value" | Skip material, log warning, or reject dispatch |
| No Materials to Group | DATA-002 | "No materials available for grouping" | Reject MPN generation, notify dispatch planner |
| SAP EWM Integration Failure | INT-001 | "Failed to generate MPN in SAP EWM" | Retry mechanism, log error, notify support team |
| Configuration Conflict | CONFIG-003 | "Configuration conflict detected for {Level}: {Value}" | Reject configuration save, show conflicting configurations |
| Invalid Date Range | CONFIG-004 | "Effective To Date must be after Effective From Date" | Reject configuration, show validation error |
| Material Not in Warehouse | DATA-003 | "Material {MaterialNumber} not available in warehouse {WarehouseCode}" | Reject material assignment, notify dispatch planner |
| MPN Generation Timeout | SYS-001 | "MPN generation timed out for vehicle {VehicleID}" | Retry, escalate if persistent, log for analysis |
| Database Connection Failure | SYS-002 | "Database connection failed" | Retry with exponential backoff, notify system administrator |

### Error Handling Strategy

1. **Validation Errors**: Reject operation, return error message to user
2. **Integration Errors**: Retry with exponential backoff (3 attempts), then escalate
3. **Data Errors**: Log warning, skip problematic data if possible, or reject operation
4. **System Errors**: Log error, notify support team, maintain system stability

---

## 14. Success Criteria

### Functional Success Criteria

1. ✅ System generates separate MPNs based on configured grouping criteria
2. ✅ Each MPN contains correct materials grouped by criteria
3. ✅ All MPNs are correctly linked to parent vehicle dispatch
4. ✅ MPNs are successfully created in SAP EWM
5. ✅ Configuration management interface is functional
6. ✅ System maintains backward compatibility (defaults to one MPN per vehicle if no configuration)

### Business Success Criteria

1. ✅ Warehouse staff can efficiently pick materials using warehouse-wise/material-wise MPNs
2. ✅ Picking errors reduced by at least 20% (to be measured post-implementation)
3. ✅ Picking time reduced by at least 15% (to be measured post-implementation)
4. ✅ Material tracking and retrieval improved (qualitative feedback from warehouse staff)
5. ✅ Configuration changes can be made without system downtime

### Technical Success Criteria

1. ✅ MPN generation completes within 5 seconds for dispatches with up to 100 materials
2. ✅ System handles up to 1000 dispatches per day
3. ✅ 99.5% success rate for MPN generation
4. ✅ Zero data loss during MPN generation
5. ✅ All MPNs are traceable and auditable

---

## 15. Edge Cases and Special Scenarios

### Edge Case 1: Single Material, Multiple Warehouses

**Scenario**: Vehicle has same material from multiple warehouses.

**Handling**:
- If grouping is warehouse-wise: Generate separate MPN for each warehouse (even if same material)
- If grouping is material-wise: Generate one MPN with material from all warehouses (with warehouse details)

### Edge Case 2: Material with Missing Grouping Criteria Value

**Scenario**: Material does not have value for selected grouping criteria (e.g., material missing plant code when grouping by plant).

**Handling**:
- Option 1: Use default/null value and group separately
- Option 2: Skip material and log warning
- Option 3: Reject dispatch (based on configuration)

**Recommendation**: Use Option 1 with default grouping, log warning for review.

### Edge Case 3: Configuration Change During Active Dispatch

**Scenario**: Configuration is changed while dispatch is being processed.

**Handling**:
- Use configuration that was active at dispatch creation time
- Do not apply new configuration to in-progress dispatches
- Apply new configuration only to new dispatches

### Edge Case 4: All Materials in Same Group

**Scenario**: All materials belong to same group based on criteria (e.g., all from same warehouse).

**Handling**:
- Generate single MPN (same as current behavior)
- No error, this is valid scenario

### Edge Case 5: Zero Materials in a Group

**Scenario**: After grouping, one or more groups have zero materials.

**Handling**:
- Do not generate MPN for empty groups
- Log information message
- Proceed with MPN generation for non-empty groups

### Edge Case 6: Partial Dispatch

**Scenario**: Some materials are partially dispatched, remaining materials need new MPN.

**Handling**:
- System should support regeneration of MPNs for remaining materials
- Maintain link to original dispatch
- Track partial dispatch status

### Edge Case 7: Multiple Configurations Applicable

**Scenario**: Multiple configurations match (e.g., material-specific and plant-specific both exist).

**Handling**:
- Apply most specific configuration (Material > Division > Plant > Site > Global)
- Log which configuration was applied for audit

### Edge Case 8: Configuration with Future Effective Date

**Scenario**: Configuration is created with future effective date.

**Handling**:
- Do not apply until effective date
- Continue using current/previous configuration
- Apply new configuration on effective date

---

## 16. Integration Requirements

### SAP EWM Integration

**Integration Type**: Real-time or Batch (to be confirmed)

**Data Flow**:
1. Read dispatch planning data from SAP EWM
2. Read master data (warehouse, material, plant, division) from SAP EWM
3. Generate MPNs in SAP EWM format
4. Create MPN documents in SAP EWM
5. Update dispatch status in SAP EWM
6. Send notifications to warehouse systems

**Integration Points**:
- **Input**: Dispatch Planning API/Interface
- **Output**: MPN Creation API/Interface
- **Master Data**: Material Master, Warehouse Master, Plant Master, Division Master

**Data Format**: SAP EWM standard format (to be confirmed with SAP team)

**Error Handling**: 
- Retry mechanism for transient failures
- Error logging and notification
- Fallback to manual MPN creation if integration fails

### Other System Integrations

| System | Integration Type | Purpose | Data Exchange |
|--------|-----------------|---------|---------------|
| Warehouse Management System | Real-time | Send MPNs to warehouses | MPN data, status updates |
| Reporting System | Batch/Real-time | MPN tracking and reporting | MPN data, grouping statistics |
| Configuration Management System | Real-time | Manage configurations | Configuration data |

---

## 17. Non-Functional Requirements

### Performance Requirements

| Requirement | Target | Measurement |
|-------------|--------|-------------|
| MPN Generation Time | < 5 seconds for 100 materials | 95th percentile |
| Configuration Lookup Time | < 1 second | Average response time |
| System Throughput | 1000 dispatches per day | Peak load capacity |
| Database Query Performance | < 2 seconds for complex queries | 95th percentile |

### Scalability Requirements

- System shall handle 50% growth in dispatch volume without performance degradation
- Database shall support 5 years of historical MPN data
- Configuration table shall support 10,000+ configurations

### Availability Requirements

- System availability: 99.5% (excluding planned maintenance)
- Scheduled maintenance window: Non-business hours
- Maximum downtime: 4 hours per month

### Security Requirements

- Authentication required for all configuration management operations
- Role-based access control:
  - Configuration Administrator: Full access
  - Dispatch Planner: Read access
  - Warehouse Manager: Read access (MPNs only)
- Audit trail for all configuration changes
- Data encryption for sensitive information
- Secure communication with SAP EWM (encrypted channels)

### Usability Requirements

- Configuration management interface should be intuitive
- Error messages should be clear and actionable
- MPN documents should be easily readable by warehouse staff
- System should provide confirmation messages for successful operations

### Maintainability Requirements

- Code should be modular and well-documented
- Configuration should be easily modifiable without code changes
- System should support logging and monitoring
- Error logs should be easily accessible for troubleshooting

---

## 18. Risks & Controls

| Risk | Impact | Probability | Mitigation | Control |
|-----|--------|------------|-----------|---------|
| Material mismatch during picking | High | Medium | Warehouse-wise and product-wise MPN creation with clear labeling | MPN validation, material-warehouse mapping check |
| Configuration errors leading to incorrect grouping | High | Low | Configuration validation, testing, audit trail | Configuration testing before activation, review process |
| SAP EWM integration failures | High | Medium | Retry mechanism, fallback to manual process | Monitoring, alerting, manual override capability |
| Performance degradation with high volume | Medium | Low | Performance testing, optimization, scaling | Load testing, performance monitoring |
| Data inconsistency between systems | Medium | Medium | Data synchronization, validation checks | Regular data reconciliation, validation rules |
| Unauthorized configuration changes | Medium | Low | Access control, audit trail | Role-based access, configuration change approval process |
| Backward compatibility issues | Medium | Low | Thorough testing, migration plan | Regression testing, phased rollout |
| Missing grouping criteria values | Medium | Medium | Data validation, default value handling | Master data validation, data quality checks |

---

## 19. Dependencies

### System Dependencies

| Dependency | Description | Impact if Unavailable |
|-----------|-------------|---------------------|
| SAP EWM | Source system for dispatch planning and MPN creation | High - System cannot function |
| Material Master | Source for material, plant, division data | High - Cannot group materials |
| Warehouse Master | Source for warehouse data | High - Cannot group by warehouse |
| Database | Storage for configurations and MPN data | High - System cannot function |
| Network Connectivity | Communication with SAP EWM | Medium - Temporary disruption acceptable with retry |

### Business Dependencies

| Dependency | Description | Impact |
|-----------|-------------|--------|
| Master Data Quality | Accurate and complete master data | High - Incorrect grouping if data is wrong |
| User Training | Training for configuration administrators and users | Medium - Incorrect usage if not trained |
| Change Management | Business process changes and communication | Medium - User adoption issues |

### Technical Dependencies

| Dependency | Description | Impact |
|-----------|-------------|--------|
| SAP EWM API Availability | APIs for reading dispatch data and creating MPNs | High - Core functionality depends on this |
| Configuration Management Framework | Framework for storing and retrieving configurations | High - Cannot manage configurations |
| Logging and Monitoring Tools | Tools for system monitoring and troubleshooting | Medium - Difficult to troubleshoot issues |

---

## 20. Testing Requirements

### Unit Testing

- Test MPN grouping logic for each grouping criteria
- Test configuration priority logic
- Test data validation rules
- Test error handling scenarios

### Integration Testing

- Test SAP EWM integration (read dispatch data, create MPNs)
- Test configuration management interface
- Test end-to-end MPN generation flow
- Test error scenarios and retry mechanisms

### System Testing

- Test MPN generation with various grouping criteria combinations
- Test configuration management (create, update, deactivate)
- Test backward compatibility (no configuration scenario)
- Test performance with high volume of materials
- Test edge cases (missing data, single group, etc.)

### User Acceptance Testing (UAT)

**Test Scenarios**:

1. **UAT-001**: Warehouse-wise MPN Generation
   - Create dispatch with materials from 3 warehouses
   - Verify 3 separate MPNs are generated
   - Verify each MPN contains correct materials

2. **UAT-002**: Material-wise MPN Generation
   - Create dispatch with 5 different materials
   - Verify 5 separate MPNs are generated
   - Verify each MPN contains single material

3. **UAT-003**: Combined Criteria (Warehouse + Material)
   - Create dispatch with materials from 2 warehouses, 3 materials each
   - Verify 6 MPNs are generated (2 warehouses × 3 materials)
   - Verify correct grouping

4. **UAT-004**: Configuration Management
   - Create site-level configuration
   - Update configuration
   - Deactivate configuration
   - Verify configuration priority (material > division > plant > site > global)

5. **UAT-005**: Backward Compatibility
   - Create dispatch without any configuration
   - Verify system generates one MPN per vehicle (current behavior)

6. **UAT-006**: Error Handling
   - Test with missing warehouse code
   - Test with invalid configuration
   - Test with SAP EWM integration failure
   - Verify appropriate error messages and handling

7. **UAT-007**: Edge Cases
   - Test with all materials in same warehouse
   - Test with material missing grouping criteria value
   - Test with configuration change during active dispatch

### Performance Testing

- Load test with 1000 dispatches per day
- Stress test with 200 materials per vehicle
- Test MPN generation time under various loads
- Test configuration lookup performance

### Regression Testing

- Verify existing single MPN per vehicle functionality still works
- Verify no impact on other dispatch processes
- Verify SAP EWM integration continues to work

---

## 21. Instructions

Refer to RIL BRD governance template for detailed field definitions.

### Next Steps

1. **Review and Approval**: This BRD should be reviewed by:
   - Business Requirement Sponsor (PETCHEM)
   - Process Owner (Amit Damani)
   - IT Process SPOC (Lalit Modi)
   - SAP EWM Technical Lead

2. **Functional Specification**: After BRD approval, prepare detailed Functional Specification (FS) document covering:
   - Technical architecture
   - Database design
   - API specifications
   - User interface design
   - Detailed technical implementation

3. **Clarifications Required**:
   - Confirm SAP EWM integration approach (real-time vs batch)
   - Confirm MPN naming convention format
   - Confirm configuration storage location (database, SAP custom table, etc.)
   - Confirm user interface requirements for configuration management
   - Confirm reporting requirements

---

**Reliance – Confidential**
