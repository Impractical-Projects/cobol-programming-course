# COBOL Programming Course - Interface Boundaries & Data Contracts

**Document Version:** 1.0  
**Date:** January 13, 2026  
**Purpose:** Surface clear interfaces and boundaries by identifying what data and behavior actually cross them

---

## Table of Contents

1. [Interface Boundaries Overview](#interface-boundaries-overview)
2. [File System Interfaces](#file-system-interfaces)
3. [Database Interfaces](#database-interfaces)
4. [JCL to Program Interfaces](#jcl-to-program-interfaces)
5. [Program to Program Interfaces](#program-to-program-interfaces)
6. [Test Framework Interfaces](#test-framework-interfaces)
7. [Data Type Boundaries](#data-type-boundaries)
8. [Encoding Boundaries](#encoding-boundaries)

---

## Interface Boundaries Overview

This document identifies all boundaries where data crosses between different components, systems, or representations in the COBOL Programming Course codebase.

### Boundary Classification

| Boundary Type | Count | Examples |
|---------------|-------|----------|
| File I/O | 15 | ACCT-REC, PRINT-LINE, REPOUT |
| Database | 3 | DB2 Z#####T table access |
| JCL to COBOL | 30 | All programs receive files via JCL |
| Data Type Conversion | 8 | COMP-3 ↔ DISPLAY |
| Encoding | 30 | EBCDIC (z/OS) ↔ Application logic |
| Test Framework | 4 | COBOL Unit Testing framework |

---

## File System Interfaces

### Interface FS-1: Account Records Input (ACCT-REC)

**Direction:** File System → COBOL Program  
**Used By:** CBL0001, CBL0002, CBL0004-CBL0012, SRCHBIN, SRCHSER, CBL0106/C

#### Physical Format
```
Format: Fixed-length sequential
Record Length: 170 bytes
Encoding: EBCDIC
Organization: Sequential
Block Size: Not specified (typically 170 or 850)
```

#### Logical Structure
```cobol
01  ACCT-FIELDS.
    05  ACCT-NO              PIC X(8).           
    05  ACCT-LIMIT           PIC S9(7)V99 COMP-3.
    05  ACCT-BALANCE         PIC S9(7)V99 COMP-3.
    05  LAST-NAME            PIC X(20).          
    05  FIRST-NAME           PIC X(15).          
    05  CLIENT-ADDR.
        10  STREET-ADDR      PIC X(25).          
        10  CITY-COUNTY      PIC X(20).          
        10  USA-STATE        PIC X(15).          
    05  RESERVED             PIC X(7).           
    05  COMMENTS             PIC X(50).          
```

#### Byte Layout
| Bytes | Field | Type | Size | Notes |
|-------|-------|------|------|-------|
| 1-8 | ACCT-NO | CHAR | 8 | Left-justified, space-filled |
| 9-13 | ACCT-LIMIT | COMP-3 | 5 | Signed, 2 decimals, packed |
| 14-18 | ACCT-BALANCE | COMP-3 | 5 | Signed, 2 decimals, packed |
| 19-38 | LAST-NAME | CHAR | 20 | Left-justified, space-filled |
| 39-53 | FIRST-NAME | CHAR | 15 | Left-justified, space-filled |
| 54-78 | STREET-ADDR | CHAR | 25 | Left-justified, space-filled |
| 79-98 | CITY-COUNTY | CHAR | 20 | Left-justified, space-filled |
| 99-113 | USA-STATE | CHAR | 15 | **Case-sensitive** |
| 114-120 | RESERVED | CHAR | 7 | Unused padding |
| 121-170 | COMMENTS | CHAR | 50 | Optional text |

#### Boundary Contracts

**Reading Programs MUST:**
- Read exactly 170 bytes per record
- Handle COMP-3 fields using COBOL unpacking
- Treat character fields as space-padded (not null-terminated)
- Compare USA-STATE values case-sensitively

**Reading Programs MUST NOT:**
- Assume NULL terminators exist
- Attempt to read beyond 170 bytes
- Modify input file (OPEN INPUT only)
- Make assumptions about field content (can be spaces)

**Data Invariants:**
- ACCT-NO is always 8 characters (may contain leading/trailing spaces)
- ACCT-LIMIT can be negative (credit accounts)
- ACCT-BALANCE can be negative (overdrawn accounts)
- USA-STATE uses Title Case (e.g., "Virginia", not "VIRGINIA" or "virginia")

#### Sample Data Crossing Boundary
```
Hex View (first 20 bytes):
00000000: 3138 3031 3138 3030 0005 0000 0000 0000  18011800........
00000010: 0000 0000                                ....

Interpreted:
ACCT-NO: "18011800"
ACCT-LIMIT: X'0005000000' = 5000.00 (COMP-3)
ACCT-BALANCE: X'0000000000' = 0.00 (COMP-3)
```

#### Known Issues at This Boundary
1. **Case Sensitivity:** CBL006A.cobol compares 'new York' (incorrect case) causing logic failure
2. **Field Interpretation:** Programs assume standard formatting but data may vary
3. **No Validation:** No boundary validation of COMP-3 field integrity

---

### Interface FS-2: Print Line Output (PRINT-LINE/PRTLINE)

**Direction:** COBOL Program → File System  
**Used By:** CBL0001-CBL0012

#### Physical Format
```
Format: Variable or Fixed-length sequential
Record Length: 80-133 bytes (varies by program)
Encoding: EBCDIC
Organization: Sequential
Control: RECFM=FBA (Fixed, Blocked, ASA carriage control)
```

#### Logical Structure
```cobol
01  PRINT-REC.
    05  DETAIL-LINE          PIC X(120).
    
01  DETAIL-LINE.
    05  FILLER               PIC X(02) VALUE SPACES.
    05  ACCT-NO-O            PIC X(8).
    05  FILLER               PIC X(02) VALUE SPACES.
    05  ACCT-LIMIT-O         PIC ZZ,ZZZ,ZZ9.99.
    05  FILLER               PIC X(02) VALUE SPACES.
    05  ACCT-BALANCE-O       PIC ZZ,ZZZ,ZZ9.99.
    [Additional fields...]
```

#### Boundary Contracts

**Writing Programs MUST:**
- Format numeric fields with PIC clauses (zero suppression, editing)
- Add FILLER spacing for readability (best practice)
- Use WRITE with AFTER ADVANCING for proper line control
- Close output file before termination

**Writing Programs MUST NOT:**
- Write records exceeding declared LRECL
- Mix WRITE and WRITE AFTER without consistency
- Assume output is human-readable (may go to printer/spool)

**Data Transformations at Boundary:**
- COMP-3 → Edited numeric (ZZ,ZZZ,ZZ9.99)
- Internal spaces → Explicit FILLER
- Logical grouping → Physical line format

#### Known Issues at This Boundary
1. **CBL0002 Typo:** `WRITE PRINT-REX` should be `WRITE PRINT-REC` (compilation failure)
2. **Inconsistent Formatting:** Early programs lack FILLER spacing for readability
3. **No Validation:** Output records not validated before writing

---

### Interface FS-3: Report Output (REPOUT)

**Direction:** COBOL Program → File System  
**Used By:** CBLDB21, CBLDB22, CBLDB23

#### Physical Format
```
Format: Fixed-length sequential
Record Length: 120 bytes
Encoding: EBCDIC
Organization: Sequential
Control: AFTER ADVANCING 2 LINES (double-spacing)
```

#### Logical Structure
```cobol
FD  REPOUT
    RECORD CONTAINS 120 CHARACTERS
    LABEL RECORDS ARE OMITTED
    DATA RECORD IS REPREC.

01  REPREC.
    05  ACCT-NO-O      PIC X(8).
    05  ACCT-LIMIT-O   PIC $$,$$$,$$9.99.
    05  ACCT-BALANCE-O PIC $$,$$$,$$9.99.
    05  ACCT-LASTN-O   PIC X(20).
    05  ACCT-FIRSTN-O  PIC X(15).
    05  ACCT-COMMENT-O PIC X(50).
```

#### Boundary Contracts

**Writing Programs MUST:**
- Transform DB2 DECIMAL fields to edited COBOL format
- Apply currency editing ($$ leading suppression)
- Write exactly 120 bytes per record
- Use AFTER ADVANCING 2 LINES for spacing

**Data Transformations at Boundary:**
- DB2 DECIMAL(9,2) → COBOL $$,$$$,$$9.99
- CHAR fields trimmed/padded to exact lengths
- Database NULL → COBOL spaces (if applicable)

---

### Interface FS-4: State Filter Input (STATEIN)

**Direction:** File System → COBOL Program  
**Used By:** CBLDB23

#### Physical Format
```
Format: Fixed-length sequential
Record Length: Variable (typically 15 bytes)
Encoding: EBCDIC
Organization: Sequential
```

#### Logical Structure
```cobol
01  STATE-REC.
    05  STATE-FILTER     PIC X(15).
```

#### Boundary Contracts

**Reading Programs MUST:**
- Read single record with state filter value
- Handle special value '*' for wildcard
- Trim trailing spaces before SQL binding

**Special Values:**
- `'*'` = Wildcard (fetch all records)
- Specific state name = Filter to that state only

**Data Transformations at Boundary:**
- File content → COBOL variable
- COBOL variable → SQL host variable (via :STATE-FILTER)

---

## Database Interfaces

### Interface DB-1: DB2 Customer Table (Z#####T)

**Direction:** Bidirectional (COBOL ↔ DB2)  
**Used By:** CBLDB21, CBLDB22, CBLDB23

#### Physical Schema
```sql
CREATE TABLE Z#####T (
    ACCTNO     CHAR(8)       NOT NULL,
    LIMIT      DECIMAL(9,2),               -- Nullable
    BALANCE    DECIMAL(9,2),               -- Nullable
    SURNAME    CHAR(20)      NOT NULL,
    FIRSTN     CHAR(15)      NOT NULL,
    ADDRESS1   CHAR(25)      NOT NULL,
    ADDRESS2   CHAR(20)      NOT NULL,
    ADDRESS3   CHAR(15)      NOT NULL,     -- Actually USA-STATE
    RESERVED   CHAR(7)       NOT NULL,
    COMMENTS   CHAR(50)      NOT NULL
);
```

#### COBOL Host Structure
```cobol
01 CUSTOMER-RECORD.
   02 ACCT-NO            PIC X(8).
   02 ACCT-LIMIT         PIC S9(7)V99 COMP-3.
   02 ACCT-BALANCE       PIC S9(7)V99 COMP-3.
   02 ACCT-LASTN         PIC X(20).
   02 ACCT-FIRSTN        PIC X(15).
   02 ACCT-ADDR1         PIC X(25).
   02 ACCT-ADDR2         PIC X(20).
   02 ACCT-ADDR3         PIC X(15).
   02 ACCT-RSRVD         PIC X(7).
   02 ACCT-COMMENT       PIC X(50).
```

#### Field Mapping & Transformations

| DB2 Column | COBOL Field | Type Transformation | Notes |
|------------|-------------|---------------------|-------|
| ACCTNO | ACCT-NO | CHAR(8) → PIC X(8) | Direct copy |
| LIMIT | ACCT-LIMIT | DECIMAL(9,2) → COMP-3 | Packed decimal conversion |
| BALANCE | ACCT-BALANCE | DECIMAL(9,2) → COMP-3 | Packed decimal conversion |
| SURNAME | ACCT-LASTN | CHAR(20) → PIC X(20) | Direct copy |
| FIRSTN | ACCT-FIRSTN | CHAR(15) → PIC X(15) | Direct copy |
| ADDRESS1 | ACCT-ADDR1 | CHAR(25) → PIC X(25) | Direct copy |
| ADDRESS2 | ACCT-ADDR2 | CHAR(20) → PIC X(20) | Direct copy |
| ADDRESS3 | ACCT-ADDR3 | CHAR(15) → PIC X(15) | ⚠️ **Semantic mismatch** |
| RESERVED | ACCT-RSRVD | CHAR(7) → PIC X(7) | Direct copy |
| COMMENTS | ACCT-COMMENT | CHAR(50) → PIC X(50) | Singular vs Plural |

#### Boundary Contracts

**COBOL Programs MUST:**
- DECLARE cursor before using
- OPEN cursor before FETCH
- Check SQLCODE after each SQL operation
- CLOSE cursor after processing
- Handle SQLCODE = 100 (EOF) gracefully
- Handle SQLCODE < 0 (errors) with error handling

**COBOL Programs MUST NOT:**
- Fetch without OPEN
- Ignore SQLCODE values
- Assume cursor position persists
- Modify table structure

**Data Invariants:**
- SQLCODE = 0 means success
- SQLCODE = 100 means no more rows (EOF)
- SQLCODE < 0 means error
- NULL values in LIMIT/BALANCE return as zeros in COBOL

#### SQL Operations Crossing Boundary

**DECLARE CURSOR:**
```cobol
EXEC SQL DECLARE CUR1 CURSOR FOR
    SELECT * FROM Z#####T
END-EXEC.
```
- Compile-time: Creates cursor definition in DBRM
- No data crosses boundary at this point

**OPEN CURSOR:**
```cobol
EXEC SQL OPEN CUR1 END-EXEC.
```
- Runtime: Establishes connection to DB2
- Executes query and positions cursor
- Data: None yet, only metadata

**FETCH:**
```cobol
EXEC SQL FETCH CUR1 INTO :CUSTOMER-RECORD END-EXEC.
```
- Runtime: Retrieves one row from result set
- Data: Full row (170+ bytes) crosses from DB2 to COBOL
- Transformations: DECIMAL → COMP-3, CHAR → PIC X

**CLOSE CURSOR:**
```cobol
EXEC SQL CLOSE CUR1 END-EXEC.
```
- Runtime: Releases cursor resources
- Data: None, only control information

#### Known Issues at This Boundary

1. **Semantic Name Mismatch:** ADDRESS3 ↔ USA-STATE is not obvious from names
2. **NULL Handling:** Programs don't explicitly check for NULL in LIMIT/BALANCE
3. **Error Recovery:** Programs STOP RUN on error rather than attempting recovery
4. **No Parameterization:** CBLDB21 hardcodes table name (Z#####T requires manual substitution)

---

### Interface DB-2: DB2 Error Handling (SQLCA)

**Direction:** DB2 → COBOL  
**Used By:** All DB2 programs

#### Physical Structure
```cobol
EXEC SQL INCLUDE SQLCA END-EXEC.

* Expands to:
01  SQLCA.
    05  SQLCAID          PIC X(8).
    05  SQLCABC          PIC S9(9) COMP.
    05  SQLCODE          PIC S9(9) COMP.     * Key field
    05  SQLERRM.
        10  SQLERRML     PIC S9(4) COMP.
        10  SQLERRMC     PIC X(70).
    05  SQLERRP          PIC X(8).
    05  SQLERRD          OCCURS 6 TIMES PIC S9(9) COMP.
    05  SQLWARN.
        10  SQLWARN0     PIC X.
        10  SQLWARN1     PIC X.
        [... more warnings ...]
    05  SQLSTATE         PIC X(5).
```

#### Boundary Contracts

**After Each SQL Operation:**
- DB2 updates SQLCA structure
- SQLCODE is primary status indicator
- Programs check SQLCODE before continuing

**Standard Error Handling Pattern:**
```cobol
EXEC SQL [operation] END-EXEC.
IF SQLCODE NOT = 0 THEN
   PERFORM SQL-ERROR-HANDLING
END-IF
```

**Error Handling Routine:**
```cobol
SQL-ERROR-HANDLING.
    DISPLAY 'ERROR AT ' FUNCTION TRIM(UD-ERROR-MESSAGE, TRAILING)
    CALL 'DSNTIAR' USING SQLCA ERROR-MESSAGE ERROR-TEXT-LEN.
    PERFORM VARYING ERROR-INDEX FROM 1 BY 1
              UNTIL ERROR-INDEX > ERROR-TEXT-HBOUND
                 OR ERROR-TEXT(ERROR-INDEX) = SPACES
       DISPLAY FUNCTION TRIM(ERROR-TEXT(ERROR-INDEX), TRAILING)
    END-PERFORM
    IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
       MOVE 1000 TO RETURN-CODE
       STOP RUN
    END-IF.
```

#### Data Crossing Boundary
- **Input:** SQL operation request
- **Output:** SQLCODE value
- **Output:** Detailed error text via DSNTIAR

---

## JCL to Program Interfaces

### Interface JCL-1: File Allocation (DD Statements)

**Direction:** JCL → COBOL via z/OS  
**Used By:** All programs

#### JCL Side
```jcl
//ACCT-REC   DD DISP=SHR,DSN=&SYSUID..DATA
//PRINT-LINE DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=120)
//REPOUT     DD DISP=SHR,DSN=&SYSUID..REPORT.OUT
//STATEIN    DD DISP=SHR,DSN=&SYSUID..STATE.IN
```

#### COBOL Side
```cobol
SELECT ACCT-REC   ASSIGN TO ACCTREC.
SELECT PRINT-LINE ASSIGN TO UT-S-PRINTLINE.
SELECT REPOUT     ASSIGN TO UT-S-REPORT.
SELECT STATEIN    ASSIGN TO UT-S-STATEIN.
```

#### Mapping Rules
- DDNAME in JCL ↔ ASSIGN TO name in COBOL (with conventions)
- z/OS applies naming conventions (e.g., ACCTREC → ACCT-REC)
- DCB parameters in JCL must match COBOL FD declarations

#### Boundary Contracts

**JCL MUST Provide:**
- DD statement for each SELECT in COBOL
- Correct LRECL matching RECORD CONTAINS
- Appropriate DISP for input/output
- Valid DSN or SYSOUT destination

**COBOL MUST:**
- OPEN files before use
- CLOSE files after use
- Match FD record length to JCL LRECL

**Mismatch Consequences:**
- Missing DD statement → S013 ABEND (file not found)
- Wrong LRECL → Data truncation or S001 ABEND
- Wrong DISP → Authority error or S213 ABEND

---

### Interface JCL-2: Program Execution Parameters

**Direction:** JCL → COBOL Program  
**Used By:** DB2 programs (via IKJEFT01)

#### JCL Invocation
```jcl
//GO       EXEC PGM=IKJEFT01,DYNAMNBR=20
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
  DSN SYSTEM(DBBG)
  RUN PROGRAM(CBLDB21) PLAN(########) -
      LIB('&SYSUID..LOAD')
//
```

#### Boundary Contracts
- Program name passed via SYSTSIN
- Plan name must be bound before execution
- Library must contain load module
- DB2 subsystem name (DBBG) must be active

---

## Program to Program Interfaces

### Interface P2P-1: COPYBOOK Inclusion (Implicit)

**Direction:** Source Code → Compiled Program  
**Used By:** All programs (potentially)

#### Current Status
- **No COPYBOOK files exist** in this codebase
- All data structures are defined in-line
- Duplication of structures across programs

#### Potential Interface
```cobol
COPY ACCT-RECORD.
```

Would expand to shared record definition.

#### Boundary Impact
- **Consistency:** Same structure across programs
- **Maintenance:** Change once, apply everywhere
- **Recommendation:** Extract common structures to COPYBOOKs

---

## Test Framework Interfaces

### Interface TF-1: COBOL Unit Testing Framework

**Direction:** Test Framework ↔ COBOL Program  
**Used By:** EMPPAY, DEPTPAY, PAYROL00 (proposed)

#### Test File Format (.cut)
```cobol
TestSuite 'Suite Name'

TestCase 'Test Case Description'
    MOVE value TO variable
    PERFORM paragraph-name
    EXPECT variable TO BE expected-value
    EXPECT variable >= expected-value
```

#### Boundary Contracts

**Test Framework Provides:**
- Test execution environment
- Fresh WORKING-STORAGE for each test
- Assertion evaluation
- Pass/fail reporting

**Test Cases Provide:**
- Setup (MOVE statements)
- Execution (PERFORM statements)
- Assertions (EXPECT statements)

**Programs Must Provide:**
- Public paragraphs that can be PERFORMED
- WORKING-STORAGE variables accessible to tests
- Deterministic behavior (no random I/O)

#### Data Crossing Boundary

**Input to Program:**
- Test data via MOVE statements
- Paragraph invocation via PERFORM

**Output from Program:**
- Variable values checked by EXPECT
- Side effects on WORKING-STORAGE

**Assertions:**
- `TO BE` = exact equality
- `>=`, `<=`, `>`, `<` = comparisons
- `NOT =` = inequality

#### Known Limitations
1. **No File I/O Mocking:** Tests can't easily mock file operations
2. **No DB2 Mocking:** Tests can't mock database access
3. **Paragraph-Level:** Must test individual paragraphs, not entire program flow

---

## Data Type Boundaries

### Interface DT-1: COMP-3 (Packed Decimal) ↔ DISPLAY

**Direction:** Internal representation ↔ Human-readable  
**Used By:** All programs with numeric calculations

#### COMP-3 Representation
```cobol
05  ACCT-LIMIT           PIC S9(7)V99 COMP-3.
```
- Internal: 5 bytes, packed decimal format
- Range: -9,999,999.99 to +9,999,999.99
- Storage: Each digit = 4 bits, sign = 4 bits

#### DISPLAY Representation
```cobol
05  ACCT-LIMIT-O         PIC ZZ,ZZZ,ZZ9.99.
```
- External: 14 bytes, edited character format
- Features: Zero suppression, comma insertion, decimal point
- Storage: One byte per character

#### Transformations

**COMP-3 → DISPLAY:**
```cobol
MOVE ACCT-LIMIT TO ACCT-LIMIT-O.
```
- Automatic unpacking
- Editing applied (zero suppression)
- Example: 0005000.00 → "   5,000.00"

**DISPLAY → COMP-3:**
```cobol
MOVE "1234.56" TO ACCT-LIMIT.
```
- Automatic packing
- Validation (must be numeric)
- Example: "1234.56" → 5-byte packed format

#### Boundary Issues
1. **Precision Loss:** Decimal truncation if target smaller than source
2. **Invalid Data:** Non-numeric data causes S0C7 ABEND
3. **Sign Handling:** Unsigned DISPLAY may lose sign during conversion

---

### Interface DT-2: BINARY (COMP) ↔ DISPLAY

**Direction:** Internal representation ↔ Human-readable  
**Used By:** Index variables, counters (TABLE-VAR, TABLE-MAX)

#### COMP Representation
```cobol
05  TABLE-VAR          PIC S9(4) COMP.
```
- Internal: 2 bytes (halfword), 4 bytes (fullword), or 8 bytes (doubleword)
- Range: -32,768 to +32,767 (halfword)
- Storage: Binary format

#### DISPLAY Representation
```cobol
05  COUNTER-O          PIC ZZ9.
```
- External: 3 bytes, edited character format

#### Boundary Contracts
- Fastest arithmetic operations (binary)
- Efficient loop counters and indexes
- Less space than DISPLAY but no editing

---

## Encoding Boundaries

### Interface ENC-1: EBCDIC ↔ Application Logic

**Direction:** Physical storage ↔ Logical processing  
**Used By:** All programs (implicit)

#### EBCDIC Encoding
- All character data stored in EBCDIC on z/OS
- Space = X'40', 'A' = X'C1', '0' = X'F0'
- Collating sequence different from ASCII

#### Boundary Contracts

**Implications:**
- String comparisons use EBCDIC collating sequence
- Numbers stored as characters (e.g., '123') are EBCDIC
- File transfers to/from ASCII systems require conversion

**Example:**
```cobol
IF USA-STATE = 'Virginia'
```
- Compares EBCDIC bytes: 'V' = X'E5', 'i' = X'89', etc.
- Case-sensitive: 'V' ≠ 'v' (X'E5' ≠ X'A5')

---

## Summary: Critical Boundary Contracts

### Top 5 Boundary Issues

1. **Case-Sensitive State Comparisons**
   - Boundary: File → COBOL
   - Issue: CBL006A uses 'new York' instead of 'New York'
   - Impact: Logic failure, no records match

2. **COMP-3 Field Interpretation**
   - Boundary: File → COBOL
   - Issue: Packed decimal must be handled correctly
   - Impact: Data corruption if misinterpreted

3. **Variable Name Typo in CBL0002**
   - Boundary: COBOL → File
   - Issue: PRINT-REX doesn't exist (should be PRINT-REC)
   - Impact: Compilation failure

4. **ADDRESS3 ↔ USA-STATE Semantic Mismatch**
   - Boundary: DB2 → COBOL
   - Issue: Column name doesn't reflect content
   - Impact: Maintainability, confusion

5. **SQLCODE Error Checking**
   - Boundary: DB2 → COBOL
   - Issue: Must check after every SQL operation
   - Impact: Silent failures if not checked

### Boundary Testing Recommendations

1. **Test at Boundaries:** Focus tests on data crossing interfaces
2. **Validate Transformations:** Verify COMP-3 ↔ DISPLAY, DB2 ↔ COBOL
3. **Check Edge Cases:** Empty files, NULL values, maximum sizes
4. **Verify Contracts:** Ensure all boundary contracts are honored
5. **Document Mismatches:** Flag semantic mismatches like ADDRESS3

---

**End of Interface Boundaries Document**

**Status:** ✅ All interfaces documented, contracts defined, issues identified  
**Next Steps:** Use for test planning, code reviews, and refactoring decisions
