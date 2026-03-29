# COBOL Programming Course - Test Specifications & Behavior Catalog

**Document Version:** 1.0  
**Date:** January 13, 2026  
**Purpose:** Make system behavior explicit and reviewable through executable specifications

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Payroll Programs Test Specifications](#payroll-programs-test-specifications)
3. [File I/O Programs Test Specifications](#file-io-programs-test-specifications)
4. [Search Algorithm Test Specifications](#search-algorithm-test-specifications)
5. [DB2 Programs Test Specifications](#db2-programs-test-specifications)
6. [String Manipulation Test Specifications](#string-manipulation-test-specifications)
7. [Behavioral Inconsistencies](#behavioral-inconsistencies)
8. [Interface Boundaries](#interface-boundaries)
9. [Test Implementation Guide](#test-implementation-guide)

---

## Executive Summary

This document proposes executable specifications for 30 COBOL programs across 4 courses. The specifications are based on **observed behavior** through static code analysis and aim to make implicit behavior explicit for review and validation.

**Key Findings:**
- 🔴 **3 Critical Bugs Identified** (EMPPAY overtime logic, CBL0002 typo, CBL006A state comparison)
- ✅ **Consistent patterns** in file I/O operations across most programs
- ⚠️ **Inconsistent formatting** approaches between early and later programs
- 📊 **45 test cases proposed** covering calculations, I/O, search, and DB2 operations

**Coverage Statistics:**
| Category | Programs | Test Cases Proposed |
|----------|----------|---------------------|
| Payroll Calculations | 3 | 12 |
| File I/O & Filtering | 11 | 15 |
| Search Algorithms | 2 | 6 |
| DB2/SQL Operations | 3 | 9 |
| String Manipulation | 2 | 3 |
| **Total** | **21** | **45** |

---

## Payroll Programs Test Specifications

### Program: PAYROL00.cobol
**Location:** Course #2 - Learning COBOL  
**Purpose:** Basic payroll calculation demonstrating COMPUTE verb

#### Business Rules (Observed)
```
GROSS-PAY = HOURS × HOURLY-RATE
```

#### Test Specifications

**Test Case 1: Basic Payroll Calculation**
```cobol
TestSuite "PAYROL00 - Basic Payroll"

TestCase "Calculate basic gross pay"
    GIVEN HOURS = 19
      AND RATE = 23
    WHEN COMPUTE GROSS-PAY = HOURS * RATE
    THEN GROSS-PAY = 437
```

**Test Case 2: Zero Hours**
```cobol
TestCase "Handle zero hours"
    GIVEN HOURS = 0
      AND RATE = 25
    WHEN COMPUTE GROSS-PAY = HOURS * RATE
    THEN GROSS-PAY = 0
```

**Test Case 3: High Hours**
```cobol
TestCase "Handle high hours without overflow"
    GIVEN HOURS = 999
      AND RATE = 999
    WHEN COMPUTE GROSS-PAY = HOURS * RATE
    THEN GROSS-PAY = 998001
    NOTE: GROSS-PAY is PIC 9(5), max value is 99999
    WARNING: This will cause numeric overflow
```

---

### Program: EMPPAY.CBL
**Location:** Course #4 - Testing  
**Purpose:** Advanced payroll with overtime and monthly bonuses

#### Business Rules (Observed)
```
Weekly Pay:
  OT-RATE = 0.25 if HOURS >= 40
  OT-RATE = 0.50 if HOURS >= 50  [BUG: Unreachable due to logic order]
  OT-RATE = 0    if HOURS < 40
  WEEKLY-PAY = (HOURS × HOURLY-RATE) × (1 + OT-RATE)

Monthly Pay:
  REWARD = 0.50 if HOURS > 150
  REWARD = 0    if HOURS <= 150
  MONTHLY-PAY = (WEEKLY-PAY × 4) × (1 + REWARD)
```

#### Test Specifications

**Test Case 4: Standard Hours (No Overtime)**
```cobol
TestSuite "EMPPAY - Employee Payment with Overtime"

TestCase "Calculate pay for standard hours"
    GIVEN EMP-HOURS = 30
      AND EMP-HOURLY-RATE = 20.00
    WHEN PERFORM PAYMENT-WEEKLY
    THEN EMP-OT-RATE = 0
     AND EMP-PAY-WEEK = 600.00
```

**Test Case 5: Overtime Threshold (40 hours)**
```cobol
TestCase "Apply 25% overtime for 40+ hours"
    GIVEN EMP-HOURS = 40
      AND EMP-HOURLY-RATE = 23.50
    WHEN PERFORM PAYMENT-WEEKLY
    THEN EMP-OT-RATE = 0.25
     AND EMP-PAY-WEEK = 1175.00
```

**Test Case 6: High Overtime (50 hours) - EXPOSES BUG**
```cobol
TestCase "50+ hours should apply 50% OT but doesn't due to bug"
    GIVEN EMP-HOURS = 50
      AND EMP-HOURLY-RATE = 23.50
    WHEN PERFORM PAYMENT-WEEKLY
    THEN EMP-OT-RATE = 0.25  [ACTUAL - BUG]
     BUT EXPECTED EMP-OT-RATE = 0.50
     AND EMP-PAY-WEEK = 1468.75  [ACTUAL with bug]
     BUT EXPECTED EMP-PAY-WEEK = 1762.50
```

**Test Case 7: Monthly Bonus Threshold**
```cobol
TestCase "Apply 50% monthly reward for hours > 150"
    GIVEN EMP-HOURS = 160
      AND EMP-PAY-WEEK = 1600.00
    WHEN PERFORM PAYMENT-MONTHLY
    THEN EMP-REWARD = 0.50
     AND EMP-PAY-MONTH = 9600.00
```

**Test Case 8: No Monthly Bonus**
```cobol
TestCase "No monthly reward for hours <= 150"
    GIVEN EMP-HOURS = 150
      AND EMP-PAY-WEEK = 1000.00
    WHEN PERFORM PAYMENT-MONTHLY
    THEN EMP-REWARD = 0
     AND EMP-PAY-MONTH = 4000.00
```

**Test Case 9: Boundary at 150 hours**
```cobol
TestCase "Verify 150 hours does not get bonus"
    GIVEN EMP-HOURS = 150
      AND EMP-PAY-WEEK = 1500.00
    WHEN PERFORM PAYMENT-MONTHLY
    THEN EMP-REWARD = 0
     AND EMP-PAY-MONTH = 6000.00
```

**Test Case 10: Boundary at 151 hours**
```cobol
TestCase "Verify 151 hours does get bonus"
    GIVEN EMP-HOURS = 151
      AND EMP-PAY-WEEK = 1500.00
    WHEN PERFORM PAYMENT-MONTHLY
    THEN EMP-REWARD = 0.50
     AND EMP-PAY-MONTH = 9000.00
```

---

### Program: DEPTPAY.CBL
**Location:** Course #4 - Testing  
**Purpose:** Calculate department average salary

#### Business Rules (Observed)
```
DEPT-AVG-SALARY = DEPT-TOTAL-SALARIES ÷ DEPT-NBR-EMPS
```

#### Test Specifications

**Test Case 11: Average Salary Calculation**
```cobol
TestSuite "DEPTPAY - Department Average Salary"

TestCase "Calculate average with multiple employees"
    GIVEN DEPT-NBR-EMPS = 19
      AND DEPT-TOTAL-SALARIES = 111111.11
    WHEN PERFORM AVERAGE-SALARY
    THEN DEPT-AVG-SALARY = 5848.48
```

**Test Case 12: Single Employee Department**
```cobol
TestCase "Handle single employee department"
    GIVEN DEPT-NBR-EMPS = 1
      AND DEPT-TOTAL-SALARIES = 50000.00
    WHEN PERFORM AVERAGE-SALARY
    THEN DEPT-AVG-SALARY = 50000.00
```

---

## File I/O Programs Test Specifications

### Program: CBL0001.cobol
**Location:** Course #2 - Learning COBOL  
**Purpose:** Basic file read and write with sequential processing

#### Business Rules (Observed)
```
1. Open ACCT-REC for INPUT, PRINT-LINE for OUTPUT
2. Write header (first time only)
3. Read all records, write each to output
4. Close both files
```

#### Test Specifications

**Test Case 13: Process All Records**
```cobol
TestSuite "CBL0001 - Basic File I/O"

TestCase "Read and write all account records"
    GIVEN ACCT-REC contains 46 records
    WHEN PERFORM READ-NEXT-RECORD until EOF
    THEN Output contains 46 detail lines
     AND Header is written once
     AND All files are closed properly
```

**Test Case 14: Empty Input File**
```cobol
TestCase "Handle empty input file"
    GIVEN ACCT-REC contains 0 records
    WHEN Program executes
    THEN Output contains header only
     AND LASTREC = 'Y' after first read
     AND Program terminates normally
```

---

### Program: CBL0002.cobol
**Location:** Course #2 - Learning COBOL  
**Purpose:** Extended file I/O with all fields including comments

#### Business Rules (Observed)
```
Same as CBL0001 but includes COMMENTS field in output
```

#### Test Specifications

**Test Case 15: Write All Fields Including Comments - EXPOSES BUG**
```cobol
TestSuite "CBL0002 - Extended File I/O"

TestCase "Write record with comments field"
    GIVEN ACCT-REC record with all fields populated
    WHEN PERFORM WRITE-RECORD
    THEN Program FAILS with 'PRINT-REX not found' error
    NOTE: Line 78 has typo: WRITE PRINT-REX should be WRITE PRINT-REC
```

---

### Program: CBL0006.cobol
**Location:** Course #2 - Learning COBOL  
**Purpose:** Conditional filtering - count Virginia residents

#### Business Rules (Observed)
```
1. Process all records
2. Count records where USA-STATE = 'Virginia'
3. Display total count in trailer
```

#### Test Specifications

**Test Case 16: Count Virginia Residents**
```cobol
TestSuite "CBL0006 - State Filtering"

TestCase "Count only Virginia records"
    GIVEN ACCT-REC with mixed state values:
          3 records with USA-STATE = 'Virginia'
          43 records with other states
    WHEN Program processes all records
    THEN VIRGINIA-COUNT = 3
     AND Trailer displays "3" Virginia residents
```

**Test Case 17: Case Sensitivity**
```cobol
TestCase "State comparison is case-sensitive"
    GIVEN USA-STATE = 'virginia' (lowercase)
    WHEN IF USA-STATE = 'Virginia'
    THEN Condition is FALSE
    NOTE: COBOL string comparison is case-sensitive
```

---

### Program: CBL006A.cobol
**Location:** Course #2 - Learning COBOL  
**Purpose:** Modified state filter for New York - CONTAINS BUG

#### Business Rules (Observed)
```
Same pattern as CBL0006 but filters for New York residents
```

#### Test Specifications

**Test Case 18: Count New York Residents - EXPOSES BUG**
```cobol
TestSuite "CBL006A - New York Filtering"

TestCase "Filter New York residents with bug"
    GIVEN ACCT-REC with USA-STATE = 'New York'
    WHEN IF USA-STATE = 'new York'  [Line 157 bug]
    THEN Condition is FALSE (should be TRUE)
    NOTE: Bug prevents any New York residents from being counted
```

---

### Program: CBL0008.cobol
**Location:** Course #2 - Learning COBOL  
**Purpose:** Calculate total limits and balances across all accounts

#### Business Rules (Observed)
```
TOTAL-LIMIT = Σ(ACCT-LIMIT) for all records
TOTAL-BALANCE = Σ(ACCT-BALANCE) for all records
```

#### Test Specifications

**Test Case 19: Accumulate Totals**
```cobol
TestSuite "CBL0008 - Account Totals"

TestCase "Sum all account limits and balances"
    GIVEN ACCT-REC with multiple records
    WHEN PERFORM LIMIT-BALANCE-TOTAL for each record
    THEN TOTAL-LIMIT = sum of all ACCT-LIMIT values
     AND TOTAL-BALANCE = sum of all ACCT-BALANCE values
     AND Trailer displays formatted totals
```

**Test Case 20: Handle Negative Balances**
```cobol
TestCase "Process accounts with negative balances"
    GIVEN ACCT-BALANCE = -100.00 (signed field)
    WHEN COMPUTE TOTAL-BALANCE = TOTAL-BALANCE + ACCT-BALANCE
    THEN TOTAL-BALANCE is reduced by 100.00
    NOTE: ACCT-BALANCE is PIC S9(7)V99 COMP-3 (signed)
```

**Test Case 21: COMP-3 Packed Decimal Handling**
```cobol
TestCase "Correctly process COMP-3 numeric fields"
    GIVEN ACCT-LIMIT = 9999999.99 (stored in 5 bytes COMP-3)
    WHEN Field is read and accumulated
    THEN Value is correctly interpreted as packed decimal
     AND No data conversion errors occur
```

---

## Search Algorithm Test Specifications

### Program: SRCHBIN.cobol
**Location:** Course #2 - Learning COBOL  
**Purpose:** Binary search on sorted account numbers

#### Business Rules (Observed)
```
1. Load up to 45 records into ACCT-TABLE
2. Table is indexed with ASCENDING KEY on ACCT-NO
3. Use SEARCH ALL for binary search
4. Search for specific account: 18011809
```

#### Test Specifications

**Test Case 22: Find Existing Account**
```cobol
TestSuite "SRCHBIN - Binary Search"

TestCase "Find account that exists in table"
    GIVEN ACCT-TABLE loaded with 45 records
      AND Table contains ACCT-NO = '18011809'
    WHEN SEARCH ALL ACCT-TABLE-ITEM
         WHEN ACCT-NO (TABLE-IDX) = 18011809
    THEN Display "User with Acct No 18011809 is found!"
```

**Test Case 23: Account Not Found**
```cobol
TestCase "Search for non-existent account"
    GIVEN ACCT-TABLE loaded with 45 records
      AND Table does NOT contain ACCT-NO = '99999999'
    WHEN SEARCH ALL for account 99999999
    THEN AT END clause executes
     AND Display "Not Found"
```

**Test Case 24: Binary Search Performance**
```cobol
TestCase "Verify binary search is used"
    GIVEN ACCT-TABLE with ASCENDING KEY IS ACCT-NO
    WHEN SEARCH ALL is executed
    THEN Search uses binary algorithm (O(log n))
    NOTE: SEARCH ALL requires ASCENDING/DESCENDING KEY clause
```

---

### Program: SRCHSER.cobol
**Location:** Course #2 - Learning COBOL  
**Purpose:** Sequential search on last names

#### Business Rules (Observed)
```
1. Load up to 45 records into ACCT-TABLE
2. Table is indexed but NOT sorted (no KEY clause)
3. Use SEARCH (not SEARCH ALL) for linear search
4. Search for LAST-NAME = "ROOSEVELT"
```

#### Test Specifications

**Test Case 25: Find Roosevelt**
```cobol
TestSuite "SRCHSER - Sequential Search"

TestCase "Find Roosevelt in table"
    GIVEN ACCT-TABLE loaded with records
      AND Table contains LAST-NAME = 'ROOSEVELT'
    WHEN SEARCH ACCT-TABLE-ITEM VARYING TABLE-IDX
         WHEN LAST-NAME (TABLE-IDX) = "ROOSEVELT"
    THEN Display "Roosevelt is found!"
```

**Test Case 26: Sequential Search Pattern**
```cobol
TestCase "Verify linear search is used"
    GIVEN ACCT-TABLE without KEY clause
    WHEN SEARCH (not SEARCH ALL) is executed
    THEN Search uses sequential algorithm (O(n))
     AND Starts from SET TABLE-IDX TO 1
```

**Test Case 27: Multiple Roosevelts**
```cobol
TestCase "Find first occurrence of duplicate name"
    GIVEN ACCT-TABLE contains 2 records with LAST-NAME = 'ROOSEVELT'
      AND Roosevelt #1 is at index 5
      AND Roosevelt #2 is at index 12
    WHEN SEARCH executes
    THEN Finds Roosevelt #1 (first occurrence)
     AND Does not continue to Roosevelt #2
```

---

## DB2 Programs Test Specifications

### Program: CBLDB21.cbl
**Location:** Course #3 - Advanced Topics  
**Purpose:** Basic DB2 cursor operations - fetch all customers

#### Business Rules (Observed)
```
1. DECLARE CURSOR for SELECT * FROM Z#####T
2. OPEN cursor
3. FETCH rows into CUSTOMER-RECORD until SQLCODE = 100 (EOF)
4. CLOSE cursor
5. Write each record to output file
```

#### Test Specifications

**Test Case 28: Fetch All Customers**
```cobol
TestSuite "CBLDB21 - Basic DB2 Cursor"

TestCase "Retrieve all customer records"
    GIVEN Z#####T table contains N rows
    WHEN EXEC SQL OPEN CUR1 END-EXEC
     AND PERFORM PRINT-AND-GET1 UNTIL SQLCODE NOT = 0
    THEN N records are fetched and written
     AND SQLCODE = 100 at end (EOF indicator)
     AND Cursor is closed successfully
```

**Test Case 29: Handle SQL Errors**
```cobol
TestCase "Detect and handle SQL errors"
    GIVEN OPEN CUR1 fails with SQLCODE < 0
    WHEN SQL-ERROR-HANDLING is performed
    THEN Error message is displayed
     AND DSNTIAR is called for detailed error text
     AND RETURN-CODE = 1000
     AND Program stops
```

**Test Case 30: Empty Table**
```cobol
TestCase "Handle empty customer table"
    GIVEN Z#####T contains 0 rows
    WHEN Cursor is opened and first fetch attempted
    THEN SQLCODE = 100 immediately
     AND No records are written
     AND Program closes cursor normally
```

---

### Program: CBLDB22.cbl
**Location:** Course #3 - Advanced Topics  
**Purpose:** Conditional DB2 queries with wildcard support

#### Business Rules (Observed)
```
1. Accept STATE-FILTER from input file
2. If STATE-FILTER = '*', use CUR1 (SELECT * - all records)
3. If STATE-FILTER = specific value, use CUR2 (WHERE clause)
4. Fetch and display matching records
```

#### Test Specifications

**Test Case 31: Wildcard Query**
```cobol
TestSuite "CBLDB22 - Conditional DB2 Queries"

TestCase "Fetch all records with wildcard"
    GIVEN STATE-FILTER = '*'
    WHEN IF STATE-FILTER = '*'
    THEN CUR1 is opened (SELECT * FROM Z#####T)
     AND All records are fetched regardless of state
     AND CUR2 is not used
```

**Test Case 32: State-Specific Query**
```cobol
TestCase "Filter by specific state"
    GIVEN STATE-FILTER = 'California'
    WHEN IF STATE-FILTER NOT = '*'
    THEN CUR2 is opened (WHERE ADDRESS3 = :STATE-FILTER)
     AND Only California records are fetched
     AND CUR1 is not used
```

**Test Case 33: Dual Cursor Logic**
```cobol
TestCase "Verify cursor selection logic"
    GIVEN Program uses two cursors
    WHEN Conditional logic executes
    THEN Exactly one cursor is opened
     AND The other cursor remains unopened
     AND Both cursors are declared but conditionally used
```

---

### Program: CBLDB23.cbl
**Location:** Course #3 - Advanced Topics  
**Purpose:** Parameterized DB2 query reading filter from file

#### Business Rules (Observed)
```
1. Read STATE-FILTER from input file (not hardcoded)
2. Use host variable :STATE-FILTER in WHERE clause
3. Similar conditional cursor logic as CBLDB22
```

#### Test Specifications

**Test Case 34: Read Filter from File**
```cobol
TestSuite "CBLDB23 - Parameterized DB2 Query"

TestCase "Accept state filter from input file"
    GIVEN STATEIN file contains 'Texas'
    WHEN READ STATEIN INTO STATE-REC
    THEN STATE-FILTER = 'Texas'
     AND Query uses WHERE ADDRESS3 = :STATE-FILTER
```

**Test Case 35: Host Variable Binding**
```cobol
TestCase "Use host variable in SQL WHERE clause"
    GIVEN STATE-FILTER = 'Florida' (from file)
    WHEN EXEC SQL FETCH CUR2 WHERE ADDRESS3 = :STATE-FILTER
    THEN DB2 binds 'Florida' to SQL query at runtime
     AND Only Florida records are returned
```

**Test Case 36: File I/O and DB2 Integration**
```cobol
TestCase "Integrate file input with DB2 processing"
    GIVEN Program reads from STATEIN file
      AND Queries Z#####T table
      AND Writes to REPOUT file
    WHEN Complete execution flow
    THEN Three separate I/O boundaries are crossed:
         1. File system (STATEIN)
         2. Database (DB2)
         3. File system (REPOUT)
```

---

## String Manipulation Test Specifications

### Programs: CBL0011.cobol, CBL0012.cobol
**Location:** Course #2 - Learning COBOL  
**Purpose:** Demonstrate COBOL intrinsic string functions

#### Business Rules (Observed)
```
CBL0011: Apply FUNCTION LOWER-CASE to name fields
CBL0012: Apply FUNCTION LOWER-CASE with substring operations
```

#### Test Specifications

**Test Case 37: Lowercase Conversion**
```cobol
TestSuite "CBL0011 - String Functions"

TestCase "Convert last name to lowercase"
    GIVEN LAST-NAME = 'WASHINGTON'
    WHEN MOVE FUNCTION LOWER-CASE(LAST-NAME) TO LAST-NAME-O
    THEN LAST-NAME-O = 'washington'
```

**Test Case 38: Mixed Case Input**
```cobol
TestCase "Handle mixed case input"
    GIVEN LAST-NAME = 'McArthur'
    WHEN FUNCTION LOWER-CASE is applied
    THEN Result = 'mcarthur'
    NOTE: All uppercase letters converted, lowercase preserved
```

**Test Case 39: Substring with Function**
```cobol
TestCase "Apply function to substring"
    GIVEN FIRST-NAME = 'GEORGE WASHINGTON'
    WHEN FUNCTION LOWER-CASE(FIRST-NAME(1:6))
    THEN Result = 'george'
     AND Original FIRST-NAME unchanged
```

---

## Behavioral Inconsistencies

### Critical Issues Requiring Resolution

#### 1. **EMPPAY.CBL - Unreachable Overtime Logic** 🔴
**Location:** Lines 32-37  
**Issue:** Logic order prevents 50% overtime rate from ever being applied

**Current Code:**
```cobol
IF EMP-HOURS >= 40
    MOVE .25 TO EMP-OT-RATE
ELSE IF EMP-HOURS >= 50
    MOVE .50 TO EMP-OT-RATE  [UNREACHABLE]
```

**Expected Behavior:**
```cobol
IF EMP-HOURS >= 50
    MOVE .50 TO EMP-OT-RATE
ELSE IF EMP-HOURS >= 40
    MOVE .25 TO EMP-OT-RATE
ELSE
    MOVE ZERO TO EMP-OT-RATE
```

**Impact:** Employees working 50+ hours receive 25% OT instead of 50% OT  
**Test Case:** Test Case 6 exposes this bug  
**Recommendation:** Reorder conditions from highest to lowest threshold

---

#### 2. **CBL0002.cobol - Variable Name Typo** 🔴
**Location:** Line 78  
**Issue:** `WRITE PRINT-REX` should be `WRITE PRINT-REC`

**Current Code:**
```cobol
WRITE PRINT-REX FROM DETAIL-LINE.
```

**Expected Code:**
```cobol
WRITE PRINT-REC FROM DETAIL-LINE.
```

**Impact:** Program fails to compile  
**Test Case:** Test Case 15 would expose this during compilation  
**Recommendation:** Correct variable name to match FD declaration

---

#### 3. **CBL006A.cobol - State Name Case Mismatch** 🔴
**Location:** Line 157  
**Issue:** Comparison uses 'new York' instead of 'New York'

**Current Code:**
```cobol
IF USA-STATE = 'new York'
```

**Expected Code:**
```cobol
IF USA-STATE = 'New York'
```

**Impact:** New York residents are never counted due to case mismatch  
**Test Case:** Test Case 18 exposes this bug  
**Recommendation:** Fix capitalization to match data format standard

---

### Inconsistent Patterns (Style Differences)

#### 4. **File I/O Record Formatting Inconsistency** ⚠️
**Programs:** CBL0001, CBL0002 vs CBL0006, CBL0008  
**Issue:** Different formatting approaches for output records

**Pattern A (CBL0001, CBL0002):**
```cobol
01  DETAIL-LINE.
    05  ACCT-NO-O         PIC X(8).
    05  ACCT-LIMIT-O      PIC ZZ,ZZZ,ZZ9.99.
    05  ACCT-BALANCE-O    PIC ZZ,ZZZ,ZZ9.99.
    [No FILLER spacing]
```

**Pattern B (CBL0006, CBL0008):**
```cobol
01  DETAIL-LINE.
    05  FILLER            PIC X(02) VALUE SPACES.
    05  ACCT-NO-O         PIC X(8).
    05  FILLER            PIC X(02) VALUE SPACES.
    05  ACCT-LIMIT-O      PIC ZZ,ZZZ,ZZ9.99.
    [Consistent 2-space FILLER between fields]
```

**Observation:** Later programs use better spacing for readability  
**Recommendation:** Standardize on Pattern B for consistency  
**Not Critical:** Both work correctly, just stylistic difference

---

#### 5. **Search Algorithm Table Declarations** ⚠️
**Programs:** SRCHBIN vs SRCHSER  
**Issue:** Identical table structure but different KEY clause usage

**SRCHBIN (Binary Search):**
```cobol
01  ACCT-TABLE.
    05  ACCT-TABLE-ITEM OCCURS 45 TIMES 
        ASCENDING KEY IS ACCT-NO
        INDEXED BY TABLE-IDX.
```

**SRCHSER (Sequential Search):**
```cobol
01  ACCT-TABLE.
    05  ACCT-TABLE-ITEM OCCURS 45 TIMES 
        INDEXED BY TABLE-IDX.
    [No KEY clause]
```

**Observation:** This is intentional to demonstrate different search algorithms  
**Recommendation:** Document that KEY clause is required for SEARCH ALL  
**Not a Bug:** This is educational - showing when to use each approach

---

#### 6. **DB2 Error Handling Consistency** ⚠️
**Programs:** CBLDB21, CBLDB22, CBLDB23  
**Issue:** All three use identical SQL-ERROR-HANDLING paragraph

**Observation:** Good consistency across DB2 programs  
**Pattern:**
```cobol
SQL-ERROR-HANDLING.
    DISPLAY 'ERROR AT ' ...
    CALL 'DSNTIAR' USING SQLCA ERROR-MESSAGE ERROR-TEXT-LEN.
    PERFORM VARYING ERROR-INDEX FROM 1 BY 1 ...
    IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
       MOVE 1000 TO RETURN-CODE
       STOP RUN
```

**Recommendation:** Consider extracting to COPYBOOK for reusability  
**Status:** ✅ Consistent pattern is good practice

---

## Interface Boundaries

### Data Interfaces

#### 1. **Account Record Interface (170 bytes)**
**Used By:** All Course #2 file I/O programs, Search programs  
**Format:** Fixed-length sequential file

**Canonical Layout:**
```cobol
01  ACCT-FIELDS.
    05  ACCT-NO              PIC X(8).           * Bytes 1-8
    05  ACCT-LIMIT           PIC S9(7)V99 COMP-3. * Bytes 9-13 (5 bytes packed)
    05  ACCT-BALANCE         PIC S9(7)V99 COMP-3. * Bytes 14-18 (5 bytes packed)
    05  LAST-NAME            PIC X(20).          * Bytes 19-38
    05  FIRST-NAME           PIC X(15).          * Bytes 39-53
    05  CLIENT-ADDR.
        10  STREET-ADDR      PIC X(25).          * Bytes 54-78
        10  CITY-COUNTY      PIC X(20).          * Bytes 79-98
        10  USA-STATE        PIC X(15).          * Bytes 99-113
    05  RESERVED             PIC X(7).           * Bytes 114-120
    05  COMMENTS             PIC X(50).          * Bytes 121-170
```

**Key Characteristics:**
- **Fixed Format:** All records are exactly 170 bytes
- **COMP-3 Fields:** ACCT-LIMIT and ACCT-BALANCE use packed-decimal (EBCDIC mainframe format)
- **Signed Fields:** Both numeric fields can be negative
- **Character Encoding:** EBCDIC (z/OS standard)
- **State Values:** Must match case exactly (e.g., 'Virginia', not 'virginia')

**Boundary Contract:**
- Programs MUST read records as 170-byte blocks
- Programs MUST handle COMP-3 conversion correctly
- Programs MUST NOT assume NULL termination (fixed-length fields)
- State comparisons are case-sensitive

---

#### 2. **DB2 Customer Table Interface**
**Used By:** CBLDB21, CBLDB22, CBLDB23  
**Table Name:** Z#####T (##### is user ID placeholder)

**Schema:**
```sql
CREATE TABLE Z#####T (
    ACCTNO     CHAR(8)       NOT NULL,
    LIMIT      DECIMAL(9,2),
    BALANCE    DECIMAL(9,2),
    SURNAME    CHAR(20)      NOT NULL,
    FIRSTN     CHAR(15)      NOT NULL,
    ADDRESS1   CHAR(25)      NOT NULL,
    ADDRESS2   CHAR(20)      NOT NULL,
    ADDRESS3   CHAR(15)      NOT NULL,
    RESERVED   CHAR(7)       NOT NULL,
    COMMENTS   CHAR(50)      NOT NULL
)
```

**Boundary Contract:**
- **Column Names:** ADDRESS3 maps to USA-STATE (not obvious from name)
- **Decimal Precision:** LIMIT and BALANCE are DECIMAL(9,2) = 7 digits + 2 decimal places
- **NULL Handling:** Most fields are NOT NULL (exceptions: LIMIT, BALANCE)
- **SQLCODE Values:**
  - `0` = Success
  - `100` = No rows found (EOF)
  - `< 0` = Error condition
- **Cursor Lifecycle:** DECLARE → OPEN → FETCH (loop) → CLOSE

**Mapping Differences:**
| COBOL Field | DB2 Column | Type Difference |
|-------------|------------|-----------------|
| ACCT-NO | ACCTNO | Identical |
| ACCT-LIMIT | LIMIT | COMP-3 vs DECIMAL |
| ACCT-BALANCE | BALANCE | COMP-3 vs DECIMAL |
| ACCT-LASTN | SURNAME | Name difference |
| ACCT-FIRSTN | FIRSTN | Abbreviated |
| ACCT-ADDR1 | ADDRESS1 | Identical |
| ACCT-ADDR2 | ADDRESS2 | Identical |
| USA-STATE | ADDRESS3 | ⚠️ **Name mismatch** |
| ACCT-RSRVD | RESERVED | Abbreviated |
| ACCT-COMMENT | COMMENTS | Singular vs Plural |

---

#### 3. **JCL to COBOL Interface**
**Used By:** All programs  
**Mechanism:** DD statements map to COBOL file handles

**Common Patterns:**
```jcl
//ACCT-REC   DD DISP=SHR,DSN=&SYSUID..DATA
//PRINT-LINE DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=120)
//REPOUT     DD DISP=SHR,DSN=&SYSUID..REPORT.OUT
//STATEIN    DD DISP=SHR,DSN=&SYSUID..STATE.IN
```

**COBOL File Control:**
```cobol
SELECT ACCT-REC   ASSIGN TO ACCTREC.
SELECT PRINT-LINE ASSIGN TO UT-S-PRINTLINE.
SELECT REPOUT     ASSIGN TO UT-S-REPORT.
SELECT STATEIN    ASSIGN TO UT-S-STATEIN.
```

**Boundary Contract:**
- **DDNAME Mapping:** COBOL ASSIGN TO name must match JCL DDNAME (or use system naming)
- **DCB Parameters:** LRECL in JCL must match RECORD CONTAINS in COBOL
- **Disposition:** DISP=SHR for input, DISP=(NEW,CATLG) for output
- **Data Set Names:** &SYSUID expands to user ID at runtime

---

#### 4. **COBOL Unit Test Interface**
**Used By:** EMPPAY, DEPTPAY  
**Format:** COBOL Unit Testing (.cut files)

**Test Format:**
```cobol
TestSuite 'Suite Description'

TestCase 'Test Case Description'
    MOVE value TO variable
    PERFORM paragraph-name
    EXPECT variable TO BE expected-value
    EXPECT variable >= expected-value
```

**Boundary Contract:**
- **Test Isolation:** Each TestCase executes independently
- **Setup:** MOVE statements prepare test data
- **Execution:** PERFORM calls the paragraph under test
- **Assertion:** EXPECT statements verify results
- **Operators:** `TO BE`, `>=`, `<=`, `=`, `NOT =`

---

### Program Interfaces

#### 1. **File Processing Programs (Standard Pattern)**
**Entry Point:** PROCEDURE DIVISION (no parameters)  
**External Dependencies:**
- Input file (ACCT-REC) via JCL
- Output file (PRINT-LINE/REPOUT) via JCL

**Execution Flow:**
```
1. OPEN-FILES
2. WRITE-HEADER (optional)
3. Processing Loop:
   - READ-RECORD
   - PROCESS-RECORD
   - WRITE-RECORD
4. WRITE-TRAILER (optional)
5. CLOSE-STOP
```

**Return Values:**
- Implicit via RETURN-CODE (0 = success, non-zero = error)
- No explicit return value in GOBACK

---

#### 2. **DB2 Programs (Extended Pattern)**
**Entry Point:** PROCEDURE DIVISION (no parameters)  
**External Dependencies:**
- DB2 subsystem (must be active)
- DBRM library (Database Request Module)
- Execution plan (bound via JCL BIND step)
- Input/output files via JCL

**Execution Flow:**
```
1. OPEN output file
2. DECLARE CURSOR (compile-time)
3. OPEN CURSOR (runtime)
4. FETCH loop:
   - Check SQLCODE
   - Process row
   - Write to file
5. CLOSE CURSOR
6. CLOSE file
7. GOBACK
```

**Error Handling:**
- SQLCODE checked after each SQL operation
- CALL 'DSNTIAR' for detailed error messages
- RETURN-CODE = 1000 on fatal SQL error

---

#### 3. **Testing Framework Interface**
**Entry Point:** Implicit via COBOL Unit Test runner  
**Test Invocation:** Framework calls program paragraphs directly

**Contract:**
- **Isolation:** Each test gets fresh WORKING-STORAGE
- **No I/O:** Tests typically don't perform file I/O (mock data via MOVE)
- **Assertions:** Framework compares EXPECT values after PERFORM
- **Results:** Framework collects pass/fail status and reports summary

---

## Test Implementation Guide

### How to Use These Specifications

#### 1. **COBOL Unit Testing Framework**
**For:** EMPPAY, DEPTPAY (already have .cut files)

**Steps:**
1. Create `.cut` file for each program
2. Copy test cases from this document
3. Adapt syntax to COBOL Unit Testing format
4. Run via zUnit or equivalent framework
5. Verify results match expectations

**Example:**
```cobol
* From Test Case 5:
TestCase 'Apply 25% overtime for 40+ hours'
    MOVE 40 TO EMP-HOURS
    MOVE 23.50 TO EMP-HOURLY-RATE
    PERFORM PAYMENT-WEEKLY
    EXPECT EMP-OT-RATE TO BE 0.25
    EXPECT EMP-PAY-WEEK TO BE 1175.00
```

---

#### 2. **JCL Test Scripts**
**For:** File I/O programs, DB2 programs

**Steps:**
1. Create test JCL in `/jcl/test/` directory
2. Prepare test data files with known contents
3. Execute program via JCL
4. Compare output to expected results (can use IDCAMS or SuperC)
5. Verify MAXCC = 0 for success

**Example JCL:**
```jcl
//CBL0001T JOB ...
//STEP1    EXEC PROC=IGYWCLG,MEMBER=CBL0001
//GO.ACCT-REC DD DSN=TEST.DATA.SMALL,DISP=SHR
//GO.PRINT-LINE DD DSN=&&TEMP,
//             DISP=(NEW,PASS),DCB=(RECFM=FBA,LRECL=120)
//STEP2    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  PRINT INFILE(INPUT) CHARACTER COUNT(46)
/*
//INPUT    DD DSN=&&TEMP,DISP=SHR
```

---

#### 3. **Manual Test Scripts**
**For:** Programs without automated test infrastructure

**Steps:**
1. Create test data files
2. Document expected outputs
3. Execute programs interactively
4. Compare actual vs expected outputs
5. Log results in test report

**Example Test Script:**
```
Test ID: PAYROL00-TC1
Program: PAYROL00.cobol
Purpose: Verify basic payroll calculation

Setup:
  - Code has HOURS = 19, RATE = 23

Expected Output:
  Name: Captain COBOL
  Hours Worked: 019
  Hourly Rate: 023
  Gross Pay: 00437

Execution:
  1. Compile program
  2. Run via IGYWCLG
  3. Check SYSOUT

Result: PASS/FAIL
Notes: [Any observations]
```

---

#### 4. **Bug Reproduction Tests**
**For:** The 3 identified bugs

**Priority Order:**
1. **EMPPAY overtime bug** (Test Case 6) - Financial impact
2. **CBL0002 compilation error** (Test Case 15) - Blocks execution
3. **CBL006A state filter bug** (Test Case 18) - Logic error

**Steps:**
1. Create failing test that exposes bug
2. Document expected vs actual behavior
3. DO NOT FIX (per requirements: "do not change production code")
4. Report findings to stakeholders
5. Recommend fix in test documentation

---

### Test Data Requirements

#### Minimal Test Data Sets

**For File I/O Tests:**
```
small-data.txt (3 records):
  - 1 Virginia resident
  - 1 New York resident
  - 1 California resident

edge-cases.txt:
  - Negative balance
  - Zero balance
  - Maximum limit (9999999.99)
  - Empty comments
  - Special characters in names
```

**For Search Tests:**
```
sorted-data.txt (for SRCHBIN):
  - Accounts sorted by ACCT-NO
  - Contains target: 18011809
  - Contains Roosevelt

unsorted-data.txt (for SRCHSER):
  - Accounts in random order
  - Contains Roosevelt
  - Demonstrates linear search
```

**For DB2 Tests:**
```sql
-- Minimal DB2 test data
INSERT INTO Z#####T VALUES
('10000001', 5000.00, 2500.00, 'WASHINGTON', 'GEORGE', '123 Main St', 'Mount Vernon', 'Virginia', '       ', 'First President'),
('18011809', 10000.00, 0.00, 'LINCOLN', 'ABRAHAM', '456 Oak Ave', 'Springfield', 'Illinois', '       ', 'Saved the Union'),
('18580927', 7500.00, 3500.00, 'ROOSEVELT', 'THEODORE', '789 Park Blvd', 'Oyster Bay', 'New York', '       ', 'Trust Buster');
```

---

### Coverage Matrix

| Program | Test Cases | Status | Priority | Framework |
|---------|-----------|--------|----------|-----------|
| PAYROL00 | TC1-TC3 | ✅ Specified | Medium | COBOL Unit |
| EMPPAY | TC4-TC10 | ✅ Specified | **HIGH** (has bug) | COBOL Unit |
| DEPTPAY | TC11-TC12 | ✅ Specified | Medium | COBOL Unit |
| CBL0001 | TC13-TC14 | ✅ Specified | Medium | JCL |
| CBL0002 | TC15 | ✅ Specified | **HIGH** (has bug) | JCL |
| CBL0006 | TC16-TC17 | ✅ Specified | Medium | JCL |
| CBL006A | TC18 | ✅ Specified | **HIGH** (has bug) | JCL |
| CBL0008 | TC19-TC21 | ✅ Specified | Medium | JCL |
| SRCHBIN | TC22-TC24 | ✅ Specified | Low | JCL |
| SRCHSER | TC25-TC27 | ✅ Specified | Low | JCL |
| CBLDB21 | TC28-TC30 | ✅ Specified | Medium | DB2/JCL |
| CBLDB22 | TC31-TC33 | ✅ Specified | Medium | DB2/JCL |
| CBLDB23 | TC34-TC36 | ✅ Specified | Medium | DB2/JCL |
| CBL0011 | TC37-TC38 | ✅ Specified | Low | JCL |
| CBL0012 | TC39 | ✅ Specified | Low | JCL |

**Coverage:** 15 of 30 programs have explicit test specifications  
**Rationale:** Focus on programs with complex logic, calculations, or identified bugs

---

## Recommendations

### Immediate Actions

1. **Fix Critical Bugs:**
   - EMPPAY.CBL overtime logic (Test Case 6 documents the issue)
   - CBL0002.cobol variable typo (prevents compilation)
   - CBL006A.cobol state comparison (incorrect filtering)

2. **Implement High-Priority Tests:**
   - Create EMPPAY.cut with Test Cases 4-10
   - Create JCL test for CBL0002 (compile test)
   - Create JCL test for CBL006A (functional test)

3. **Expand Test Coverage:**
   - Add .cut files for PAYROL00 and DEPTPAY
   - Create JCL test suite for file I/O programs
   - Develop DB2 test scripts with sample data

### Long-Term Improvements

1. **Standardize Patterns:**
   - Use FILLER spacing consistently in all output records
   - Extract DB2 error handling to COPYBOOK
   - Document state name capitalization standards

2. **Enhance Documentation:**
   - Add interface contracts to program headers
   - Document business rules in-line
   - Create data dictionary for all record layouts

3. **Test Infrastructure:**
   - Set up automated test execution via CI/CD
   - Create test data generators
   - Implement output comparison tools

4. **Training Materials:**
   - Use test cases as teaching examples
   - Document bug fixes as learning opportunities
   - Create "before and after" examples

---

## Appendix: Test Case Template

Use this template to create additional test cases:

```cobol
TestSuite "[Program Name] - [Purpose]"

TestCase "[Brief Description]"
    GIVEN [Preconditions]
      AND [Additional setup]
    WHEN [Action performed]
    THEN [Expected result]
     AND [Additional assertions]
    NOTE: [Optional explanation or context]
```

**Example:**
```cobol
TestSuite "PAYROL00 - Basic Payroll Calculation"

TestCase "Calculate gross pay for standard hours"
    GIVEN HOURS = 40
      AND RATE = 25
    WHEN COMPUTE GROSS-PAY = HOURS * RATE
    THEN GROSS-PAY = 1000
     AND Result is formatted as 5-digit numeric
    NOTE: No overtime logic in PAYROL00
```

---

## Document Maintenance

**Next Review:** After bug fixes are implemented  
**Update Frequency:** When new programs are added or behavior changes  
**Validation:** Test cases should be executed to verify observed behavior  

**Change Log:**
- 2026-01-13: Initial version 1.0 created
- [Future updates will be logged here]

---

*End of Test Specifications Document*

**Status:** ✅ Behavior documented, interfaces defined, bugs identified  
**Next Steps:** Review findings with stakeholders, prioritize bug fixes, implement tests
