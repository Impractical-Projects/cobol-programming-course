# COBOL Programming Course - Execution Paths & Data Access Patterns

**Document Version:** 1.0  
**Date:** January 13, 2026

---

## Table of Contents

1. [Major Execution Paths](#major-execution-paths)
2. [Data Access Patterns](#data-access-patterns)
3. [Implicit Dependencies](#implicit-dependencies)
4. [Business Rule Implementation](#business-rule-implementation)
5. [Integration Points](#integration-points)

---

## Major Execution Paths

### Path 1: Simple Display Programs

**Programs:** HELLO, COBOL, PAYROL0X, ADDAMT (partial)

**Execution Trace:**
```
1. Program starts (PROCEDURE DIVISION)
2. Initialize variables (VALUE clauses)
3. Execute main logic
   - HELLO: DISPLAY 'HELLO WORLD'
   - COBOL: ACCEPT CURRENT-DATE, DISPLAY formatted
   - PAYROL0X: MOVE/COMPUTE, DISPLAY results
4. STOP RUN (implicit or explicit)
```

**Call Stack:**
```
MAIN PROGRAM
  └─ PROCEDURE DIVISION
       └─ Main paragraph
            └─ DISPLAY/ACCEPT statements
                 └─ STOP RUN
```

**No File I/O, No External Dependencies**

---

### Path 2: File Processing with Report Generation

**Programs:** CBL0001, CBL0002, CBL0004, CBL0005, CBL0006, CBL0007, CBL0008, CBL0009, CBL0010, CBL0011, CBL0012

**Execution Trace (CBL0001 example):**
```
1. Program initialization
   ├─ WORKING-STORAGE variables initialized
   ├─ FLAGS set (FIRST-RECORD='Y', LASTREC='N')
   └─ COUNTERS zeroed

2. OPEN-FILES paragraph
   ├─ OPEN INPUT ACCT-REC
   └─ OPEN OUTPUT PRTLINE

3. READ-RECORD paragraph (priming read)
   └─ READ ACCT-REC INTO ACCT-FIELDS
        AT END: MOVE 'Y' TO LASTREC

4. WRITE-HEADER paragraph
   ├─ IF FIRST-RECORD = 'Y'
   │   ├─ WRITE HEADER-1 (title)
   │   ├─ WRITE HEADER-2 (column headers)
   │   └─ MOVE 'N' TO FIRST-RECORD
   └─ ENDIF

5. PERFORM UNTIL LASTREC = 'Y'
   ├─ PERFORM PROCESS-RECORD
   │   ├─ MOVE input fields to output fields
   │   ├─ Apply formatting (PIC clauses)
   │   ├─ Perform calculations (if any)
   │   ├─ Apply business rules (if any)
   │   └─ INCREMENT counters
   │
   ├─ PERFORM WRITE-RECORD
   │   └─ WRITE PRTLINE FROM DETAIL-LINE
   │
   └─ PERFORM READ-RECORD
        └─ READ ACCT-REC
             AT END: MOVE 'Y' TO LASTREC

6. WRITE-TRAILER paragraph
   ├─ MOVE totals to trailer line
   └─ WRITE PRTLINE FROM TRAILER-LINE

7. CLOSE-FILES paragraph
   ├─ CLOSE ACCT-REC
   └─ CLOSE PRTLINE

8. STOP RUN
```

**Call Stack:**
```
MAIN PROGRAM
  └─ PROCEDURE DIVISION
       ├─ OPEN-FILES
       ├─ READ-RECORD
       ├─ WRITE-HEADER
       ├─ Main Loop (PERFORM UNTIL)
       │    ├─ PROCESS-RECORD
       │    ├─ WRITE-RECORD
       │    └─ READ-RECORD
       ├─ WRITE-TRAILER
       ├─ CLOSE-FILES
       └─ STOP RUN
```

**Data Flow:**
```
ACCT-REC (file) → ACCT-FIELDS (buffer) → DETAIL-LINE (formatted) → PRTLINE (file)
```

---

### Path 3: Table Load and Search

**Programs:** SRCHBIN, SRCHSER

**Execution Trace (SRCHBIN example):**
```
1. Program initialization
   ├─ Table ACCT-TABLE defined (45 entries)
   └─ Index ACCT-IDX initialized

2. OPEN-FILE paragraph
   └─ OPEN INPUT ACCT-REC

3. LOAD-TABLE paragraph
   ├─ SET SUB TO 1
   ├─ PERFORM VARYING SUB FROM 1 BY 1
   │   UNTIL LASTREC = 'Y' OR SUB > 45
   │   ├─ PERFORM READ-RECORD
   │   │   └─ READ ACCT-REC INTO ACCT-FIELDS
   │   │        AT END: MOVE 'Y' TO LASTREC
   │   │
   │   └─ IF NOT LASTREC = 'Y'
   │       ├─ MOVE ACCT-FIELDS TO ACCT-ENTRY(SUB)
   │       └─ SET LIMIT-SUB TO SUB
   └─ END-PERFORM

4. CLOSE-FILE paragraph
   └─ CLOSE ACCT-REC

5. SEARCH-TABLE paragraph
   ├─ DISPLAY 'ENTER ACCOUNT NUMBER'
   ├─ ACCEPT SEARCH-ACCT-NO
   │
   ├─ SEARCH ALL ACCT-ENTRY
   │   AT END
   │       DISPLAY 'ACCOUNT NOT FOUND'
   │   WHEN ACCT-NO-T (ACCT-IDX) = SEARCH-ACCT-NO
   │       ├─ SET ACCT-FOUND TO TRUE
   │       └─ DISPLAY ACCT-ENTRY (ACCT-IDX)
   │
   └─ END-SEARCH

6. STOP RUN
```

**Binary Search Internal Logic:**
```
SEARCH ALL implementation (system-generated):
1. Initialize: LOW = 1, HIGH = table size
2. While LOW <= HIGH:
   ├─ MID = (LOW + HIGH) / 2
   ├─ Compare SEARCH-KEY with ACCT-NO-T(MID)
   │   ├─ If EQUAL: Found, return MID
   │   ├─ If LESS: HIGH = MID - 1
   │   └─ If GREATER: LOW = MID + 1
   └─ Loop
3. Not found
```

**Sequential Search Internal Logic (SRCHSER):**
```
SEARCH implementation:
1. VARYING index FROM 1 BY 1
2. While index <= table size:
   ├─ Compare SEARCH-KEY with LAST-NAME-T(index)
   │   └─ If EQUAL: Found, exit
   └─ Increment index
3. AT END: Not found
```

**Call Stack:**
```
MAIN PROGRAM
  └─ PROCEDURE DIVISION
       ├─ OPEN-FILE
       ├─ LOAD-TABLE (with loop)
       │    └─ READ-RECORD (repeated)
       ├─ CLOSE-FILE
       ├─ SEARCH-TABLE
       │    ├─ ACCEPT
       │    └─ SEARCH/SEARCH ALL
       └─ STOP RUN
```

---

### Path 4: DB2 Cursor Processing

**Programs:** CBLDB21, CBLDB22, CBLDB23

**Execution Trace (CBLDB21 example):**
```
1. Program initialization
   ├─ WORKING-STORAGE variables initialized
   ├─ SQL host variables defined
   └─ SQLCODE initialized to 0

2. SQL DECLARE phase (compile-time)
   └─ EXEC SQL
        DECLARE C1 CURSOR FOR
        SELECT CUSTOMERID, FIRSTNAME, LASTNAME,
               STREET, CITY, STATE
        FROM CUSTOMER
      END-EXEC

3. OPEN-FILES paragraph
   ├─ OPEN OUTPUT CUSTOUT (report file)
   └─ EXEC SQL OPEN C1 END-EXEC
        └─ DB2 prepares cursor for execution

4. WRITE-HEADER paragraph
   └─ WRITE report headers

5. FETCH-RECORDS paragraph
   └─ PERFORM UNTIL SQLCODE NOT = 0
       ├─ EXEC SQL
       │   FETCH C1 INTO
       │     :CUSTOMERID, :FIRSTNAME, :LASTNAME,
       │     :STREET, :CITY, :STATE
       │  END-EXEC
       │
       ├─ IF SQLCODE = 0
       │   ├─ PERFORM PROCESS-RECORD
       │   │   ├─ MOVE :host-vars TO output fields
       │   │   └─ Apply formatting
       │   │
       │   └─ PERFORM WRITE-RECORD
       │       └─ WRITE CUSTOUT FROM REPORT-LINE
       │
       └─ ELSE IF SQLCODE = 100
           └─ (Normal EOF, exit loop)
       └─ ELSE
           ├─ DISPLAY 'SQL ERROR: ' SQLCODE
           └─ (Error handling)

6. CLOSE-CURSOR paragraph
   ├─ EXEC SQL CLOSE C1 END-EXEC
   └─ CLOSE CUSTOUT

7. STOP RUN
```

**DB2 Interaction Flow:**
```
COBOL Program                DB2 Subsystem
     │                            │
     ├─ OPEN C1 ──────────────────►
     │                      Parse SQL
     │                      Prepare cursor
     │  ◄─────────────────── Ready
     │                            │
     ├─ FETCH C1 ─────────────────►
     │                      Read row
     │  ◄─────────────────── Row data
     │                            │
     ├─ FETCH C1 ─────────────────►
     │                      Read row
     │  ◄─────────────────── Row data
     │                            │
     ├─ FETCH C1 ─────────────────►
     │                      No more rows
     │  ◄─────────────────── SQLCODE=100
     │                            │
     ├─ CLOSE C1 ─────────────────►
     │                      Release resources
     │  ◄─────────────────── Closed
     │                            │
```

**Call Stack:**
```
MAIN PROGRAM
  └─ PROCEDURE DIVISION
       ├─ OPEN-FILES
       │    └─ EXEC SQL OPEN C1
       ├─ WRITE-HEADER
       ├─ FETCH-RECORDS (loop)
       │    ├─ EXEC SQL FETCH
       │    ├─ PROCESS-RECORD
       │    └─ WRITE-RECORD
       ├─ CLOSE-CURSOR
       │    └─ EXEC SQL CLOSE C1
       └─ STOP RUN
```

---

### Path 5: Multi-Step PERFORM (CBL0033)

**Program:** CBL0033

**Execution Trace:**
```
1. PERFORM-UNTIL demonstration
   └─ PERFORM PARA-1 UNTIL WS-A > 5
        ├─ Iteration 1: WS-A=1
        ├─ Iteration 2: WS-A=2
        ├─ Iteration 3: WS-A=3
        ├─ Iteration 4: WS-A=4
        └─ Iteration 5: WS-A=5

2. PERFORM-TIMES demonstration
   └─ PERFORM PARA-2 5 TIMES
        ├─ Iteration 1
        ├─ Iteration 2
        ├─ Iteration 3
        ├─ Iteration 4
        └─ Iteration 5

3. PERFORM-THRU demonstration
   └─ PERFORM PARA-3 THRU PARA-6
        ├─ Execute PARA-3
        ├─ Execute PARA-4
        ├─ Execute PARA-5
        └─ Execute PARA-6

4. PERFORM-VARYING demonstration
   └─ PERFORM PARA-7 
        VARYING WS-B FROM 1 BY 1 
        UNTIL WS-B > 10
        ├─ Iteration 1: WS-B=1
        ├─ Iteration 2: WS-B=2
        ...
        └─ Iteration 10: WS-B=10

5. STOP RUN
```

**Control Flow Diagram:**
```
MAIN LINE
  │
  ├─ PERFORM-UNTIL ──► PARA-1 (loop 5 times)
  │                      └─ ADD 1 TO WS-A
  │
  ├─ PERFORM-TIMES ──► PARA-2 (5 times)
  │                      └─ DISPLAY count
  │
  ├─ PERFORM-THRU ──► PARA-3
  │                    │
  │                    ├─ PARA-4
  │                    │
  │                    ├─ PARA-5
  │                    │
  │                    └─ PARA-6
  │
  ├─ PERFORM-VARYING ──► PARA-7 (loop 10 times)
  │                       └─ Process with WS-B
  │
  └─ STOP RUN
```

---

## Data Access Patterns

### Pattern 1: Sequential File Processing

**Access Method:** Sequential READ  
**Used By:** CBL0001-0012, SRCHBIN/SRCHSER (load phase)

**Pattern:**
```cobol
OPEN INPUT file
READ file INTO buffer
PERFORM UNTIL EOF
    Process record
    READ file INTO buffer
END-PERFORM
CLOSE file
```

**Characteristics:**
- Records read in physical order
- One record at a time
- Forward-only cursor
- Cannot rewind without CLOSE/REOPEN
- Optimal for batch processing

**File Position Tracking:**
```
File: [Rec1][Rec2][Rec3][Rec4]...[RecN]
        ▲
        │
    File Pointer moves forward with each READ
```

---

### Pattern 2: In-Memory Table Access

**Access Method:** Direct indexing or SEARCH  
**Used By:** SRCHBIN (binary), SRCHSER (sequential)

**Load Pattern:**
```cobol
PERFORM VARYING I FROM 1 BY 1 UNTIL EOF
    READ file INTO buffer
    MOVE buffer TO table-entry(I)
END-PERFORM
```

**Access Pattern (Binary Search):**
```cobol
SEARCH ALL table-entry
    WHEN key-field(index) = search-value
        Process table-entry(index)
END-SEARCH
```

**Access Pattern (Sequential Search):**
```cobol
SEARCH table-entry VARYING index
    WHEN key-field(index) = search-value
        Process table-entry(index)
END-SEARCH
```

**Memory Layout:**
```
WORKING-STORAGE SECTION
  ACCT-TABLE
    [Entry 1][Entry 2][Entry 3]...[Entry 45]
       ▲       ▲       ▲           ▲
       │       │       │           │
    Direct access via index: ACCT-ENTRY(n)
```

---

### Pattern 3: DB2 Cursor-Based Access

**Access Method:** SQL Cursor  
**Used By:** CBLDB21, CBLDB22, CBLDB23

**Pattern:**
```cobol
EXEC SQL DECLARE cursor FOR SELECT ... END-EXEC
EXEC SQL OPEN cursor END-EXEC
PERFORM UNTIL SQLCODE NOT = 0
    EXEC SQL FETCH cursor INTO :host-vars END-EXEC
    IF SQLCODE = 0
        Process :host-vars
    END-IF
END-PERFORM
EXEC SQL CLOSE cursor END-EXEC
```

**Characteristics:**
- Server-side result set
- Row-by-row retrieval
- Can have WHERE clauses (filtered)
- SQLCODE indicates status
- Resources held until CLOSE

**Data Flow:**
```
DB2 Table
    ↓ (SELECT query)
Result Set (server-side)
    ↓ (FETCH)
Host Variables (:vars in COBOL)
    ↓ (MOVE)
WORKING-STORAGE fields
    ↓ (WRITE)
Output File
```

---

### Pattern 4: Accumulator/Counter Pattern

**Access Method:** Running totals  
**Used By:** CBL0006, CBL0008, CBL0009

**Pattern:**
```cobol
INITIALIZE counters TO ZERO
PERFORM UNTIL EOF
    IF condition
        ADD 1 TO counter
    END-IF
    ADD field-value TO running-total
    READ next record
END-PERFORM
DISPLAY counters and totals
```

**Example (CBL0006 - Virginia count):**
```cobol
01  VA-COUNT PIC 9(5) VALUE ZERO.

PERFORM UNTIL EOF
    IF USA-STATE = 'Virginia'
        ADD 1 TO VA-COUNT
    END-IF
    READ ACCT-REC
END-PERFORM

DISPLAY 'VIRGINIA CUSTOMERS: ' VA-COUNT
```

**Example (CBL0008 - Account totals):**
```cobol
01  TLIMIT   PIC S9(9)V99 VALUE ZERO.
01  TBALANCE PIC S9(9)V99 VALUE ZERO.

PERFORM UNTIL EOF
    COMPUTE TLIMIT = TLIMIT + ACCT-LIMIT
    COMPUTE TBALANCE = TBALANCE + ACCT-BALANCE
    READ ACCT-REC
END-PERFORM

DISPLAY 'TOTAL LIMITS: ' TLIMIT
DISPLAY 'TOTAL BALANCES: ' TBALANCE
```

---

### Pattern 5: Conditional Data Filtering

**Access Method:** Read-all, filter-some  
**Used By:** CBL0006, CBL0007, CBLDB22, CBLDB23

**Pattern (File-based):**
```cobol
PERFORM UNTIL EOF
    READ record
    IF filter-condition
        WRITE output-record
    ELSE
        (skip record)
    END-IF
END-PERFORM
```

**Pattern (DB2-based):**
```cobol
IF filter-value = '*'
    EXEC SQL
        SELECT * FROM table
    END-EXEC
ELSE
    EXEC SQL
        SELECT * FROM table
        WHERE field = :filter-value
    END-EXEC
END-IF
```

**Example Filters:**
- State filter: `USA-STATE = 'Virginia'`
- Name filter: `LASTNAME = :search-name`
- Wildcard: `filter-value = '*'` (bypass filter)

---

## Implicit Dependencies

### Compiler Dependencies

**All Programs Depend On:**
```
1. IBM Enterprise COBOL Compiler v6.3 (IGYCRCTL)
   - COBOL Language Standards
   - Intrinsic Functions
   - COMP-3 Support

2. Language Environment (LE)
   - Runtime library (CEE.SCEERUN)
   - Linkage library (CEE.SCEELKED)
   - Error handling (CEEDUMP)

3. z/OS Operating System
   - File system (VSAM, Sequential)
   - JCL job scheduling
   - SYSOUT handling
```

### Data Format Dependencies

**Programs with COMP-3 Fields:**
```
CBL0001-0012, SRCHBIN, SRCHSER, CBL0106/C
   │
   └─► Require: Packed-decimal support
        └─► Platform: z/OS (or compatible)
             └─► Cannot run on: ASCII systems without conversion
```

**Data File Format:**
```
File: &SYSUID..DATA
   ├─ Record Length: 170 bytes fixed
   ├─ Organization: Sequential
   ├─ COMP-3 fields: bytes 9-18 (packed-decimal)
   └─ Character fields: EBCDIC encoding

Implication: Programs expect exact 170-byte records
            with packed-decimal in specific positions
```

### JCL Procedure Dependencies

**All Jobs Depend On:**
```
1. Procedure Libraries
   ├─ &SYSUID..PROCLIB (or system PROCLIB)
   └─ Contains: IGYWC, IGYWCL, IGYWCLG, DB2CBL, etc.

2. Source Library
   └─ &SYSUID..CBL (PDS with COBOL source members)

3. Load Library
   └─ &SYSUID..LOAD (PDS/PDSE for executables)

4. Data Files (where applicable)
   └─ &SYSUID..DATA (test data)
```

### DB2 Programs Additional Dependencies

**CBLDB21, CBLDB22, CBLDB23 Depend On:**
```
1. DB2 Subsystem Running
   └─ Subsystem name (e.g., DSNV10)

2. CUSTOMER Table Exists
   └─ Schema matches program declarations

3. DBRM Library
   └─ &SYSUID..DBRMLIB (Database Request Modules)

4. DB2 Plan Bound
   ├─ Plan name matches program
   └─ Bind completed successfully

5. DB2 Load Libraries
   ├─ DSNV10.SDSNLOAD
   └─ DSNV10.RUNLIB.LOAD

6. TSO/DB2 Interface
   └─ IKJEFT01 (TSO EXEC program)
```

### Implicit Naming Conventions

**Programs Assume:**
```
1. Dataset Naming
   ├─ CBL source: &SYSUID..CBL(membername)
   ├─ Load module: &SYSUID..LOAD(membername)
   ├─ Data file: &SYSUID..DATA
   └─ DBRM: &SYSUID..DBRMLIB(membername)

2. File DDNAMEs
   ├─ ACCT-REC → ACCTREC (DD statement)
   ├─ PRTLINE → PRTLINE (DD statement)
   └─ CUSTOUT → CUSTOUT (DD statement)

3. Procedure Names
   ├─ IGYWCLG expects SRC= parameter
   ├─ DB2CBL expects SRC= and DB2SYS= parameters
   └─ Step names: COMPILE, BIND, RUN (or GO)
```

### Platform-Specific Dependencies

**z/OS Specific Features Used:**
```
1. EBCDIC character encoding
   └─ Character fields in data files

2. Packed-decimal (COMP-3)
   └─ Numeric storage format

3. JCL syntax
   └─ Job control and dataset allocation

4. PDS/PDSE structures
   └─ Library members

5. VSAM or Sequential file organization
   └─ Dataset types
```

---

## Business Rule Implementation

### Rule 1: Payroll Calculation (PAYROL00, EMPPAY)

**Location:** PROCEDURE DIVISION  
**Implementation:**

```cobol
* Basic Payroll (PAYROL00)
COMPUTE GROSS-PAY = HOURS * HOURLY-RATE

* Complex Payroll with Overtime (EMPPAY)
IF EMP-HOURS <= 40
    COMPUTE EMP-PAY-WEEK = EMP-HOURS * EMP-HOURLY-RATE
ELSE IF EMP-HOURS <= 80
    COMPUTE BASE-PAY = 40 * EMP-HOURLY-RATE
    COMPUTE OT-HOURS = EMP-HOURS - 40
    COMPUTE OT-PAY = OT-HOURS * EMP-HOURLY-RATE * 
                     (1 + EMP-OT-RATE)
    COMPUTE EMP-PAY-WEEK = BASE-PAY + OT-PAY
ELSE
    COMPUTE EMP-PAY-WEEK = 80 * EMP-HOURLY-RATE * 
                           (1 + EMP-OT-RATE)
END-IF

* Monthly Bonus
IF EMP-HOURS >= 150
    COMPUTE EMP-PAY-MONTH = (EMP-PAY-WEEK * 4) * 
                            (1 + EMP-REWARD)
ELSE
    COMPUTE EMP-PAY-MONTH = EMP-PAY-WEEK * 4
END-IF
```

**Business Logic:**
- Regular time: First 40 hours at base rate
- Overtime: Hours 41-80 at premium rate (base + OT-RATE%)
- Maximum: Capped at 80 hours per week
- Monthly bonus: 150+ hours triggers reward multiplier

**Traceability:**
```
Requirement: "Pay employees for hours worked with overtime premium"
    ↓
Implementation: EMPPAY.CBL, lines 30-50
    ↓
Test Data: Hardcoded in program (can be modified)
```

---

### Rule 2: Customer State Filtering (CBL0006, CBL0007)

**Location:** PROCESS-RECORD paragraph  
**Implementation:**

```cobol
* Using IF statement (CBL0006)
IF USA-STATE = 'Virginia'
    ADD 1 TO VA-COUNT
END-IF

* Using Level-88 condition-name (CBL0007)
88 STATE VALUE 'Virginia'.
...
IF STATE
    ADD 1 TO VA-COUNT
END-IF
```

**Business Logic:**
- Count only customers from Virginia
- Ignore all other states
- Display total at end

**Traceability:**
```
Requirement: "Count customers in specific state"
    ↓
Implementation: CBL0006/CBL0007, PROCESS-RECORD
    ↓
Variation: CBL006A filters for New York instead
```

---

### Rule 3: Account Limit Totals (CBL0008, CBL0009)

**Location:** PROCESS-RECORD paragraph  
**Implementation:**

```cobol
COMPUTE TLIMIT = TLIMIT + ACCT-LIMIT
COMPUTE TBALANCE = TBALANCE + ACCT-BALANCE
ADD 1 TO TOTAL-RECORDS
```

**Business Logic:**
- Accumulate all account credit limits
- Accumulate all account balances
- Count total records processed
- Display grand totals at end

**Traceability:**
```
Requirement: "Calculate total credit exposure and outstanding balances"
    ↓
Implementation: CBL0008/CBL0009, COMPUTE statements
    ↓
Output: Trailer line with totals
```

---

### Rule 4: Department Average Salary (DEPTPAY)

**Location:** PROCEDURE DIVISION  
**Implementation:**

```cobol
COMPUTE DEPT-AVG-SALARY = 
    DEPT-TOTAL-SALARIES / DEPT-NBR-EMPS
```

**Business Logic:**
- Average = Total salaries / Number of employees
- Simple arithmetic calculation
- No null or zero-division handling (assumes valid data)

**Traceability:**
```
Requirement: "Calculate average salary per department"
    ↓
Implementation: DEPTPAY.CBL, COMPUTE statement
    ↓
Test: Hardcoded values (Finance dept, 5 employees, $500,000 total)
```

---

### Rule 5: Binary vs Sequential Search Strategy

**Location:** SEARCH-TABLE paragraphs  
**Implementation:**

```cobol
* Binary Search (SRCHBIN) - for sorted data
SEARCH ALL ACCT-ENTRY
    AT END
        DISPLAY 'NOT FOUND'
    WHEN ACCT-NO-T (ACCT-IDX) = SEARCH-ACCT-NO
        DISPLAY ACCT-ENTRY (ACCT-IDX)
END-SEARCH

* Sequential Search (SRCHSER) - for unsorted data
SEARCH ACCT-ENTRY VARYING ACCT-IDX
    AT END
        DISPLAY 'NOT FOUND'
    WHEN LAST-NAME-T (ACCT-IDX) = SEARCH-LAST-NAME
        DISPLAY ACCT-ENTRY (ACCT-IDX)
END-SEARCH
```

**Business Logic:**
- SRCHBIN: Use when data is sorted by account number
  - Requires ASCENDING KEY clause
  - O(log n) complexity - faster for large datasets
  
- SRCHSER: Use when data is unsorted or sorted differently
  - No special requirements
  - O(n) complexity - slower but works on any data

**Traceability:**
```
Requirement: "Efficiently locate customer records"
    ↓
Implementation: SRCHBIN (sorted) vs SRCHSER (unsorted)
    ↓
Performance: Binary search preferred for frequent lookups
```

---

### Rule 6: SQL Query Filtering (CBLDB22, CBLDB23)

**Location:** OPEN-CURSOR logic  
**Implementation:**

```cobol
* CBLDB22: Name-based filtering
IF LNAME = '*'
    EXEC SQL OPEN C1 END-EXEC     (all records)
ELSE
    EXEC SQL OPEN C2 END-EXEC     (filtered by LNAME)
END-IF

* CBLDB23: State-based filtering
EXEC SQL
    DECLARE C1 CURSOR FOR
    SELECT * FROM CUSTOMER
    WHERE STATE = :STATE
END-EXEC
```

**Business Logic:**
- Wildcard '*' means "fetch all records"
- Specific value means "filter to matching records only"
- Filtering done at database level (efficient)

**Traceability:**
```
Requirement: "Query customers by name or state with optional filter"
    ↓
Implementation: CBLDB22/23, dual cursor or WHERE clause
    ↓
Advantage: Database filtering reduces network traffic
```

---

## Integration Points

### Point 1: File System Integration

**Interface:** COBOL I/O verbs → z/OS File System

```
COBOL Program
    │
    ├─ OPEN INPUT ACCT-REC
    │     ↓
    │   z/OS File System locates &SYSUID..DATA
    │     ↓
    │   Allocate buffer, initialize file pointer
    │
    ├─ READ ACCT-REC INTO buffer
    │     ↓
    │   Fetch next record (170 bytes)
    │   Convert COMP-3 fields if needed
    │     ↓
    │   Move to COBOL buffer
    │
    └─ CLOSE ACCT-REC
          ↓
        Flush buffers, release resources
```

**Error Conditions:**
- File not found → Runtime error, job fails
- Wrong LRECL → Data misalignment
- COMP-3 corruption → S0C7 abend

---

### Point 2: DB2 Integration

**Interface:** Embedded SQL → DB2 Subsystem

```
COBOL Program (CBLDB21)
    │
    ├─ EXEC SQL DECLARE CURSOR
    │     ↓ (compile-time)
    │   SQL Preprocessor extracts SQL
    │     ↓
    │   Generates DBRM (Database Request Module)
    │
    ├─ Bind Process (separate step)
    │     ↓
    │   DB2 validates SQL, creates execution plan
    │     ↓
    │   Stores plan in DB2 catalog
    │
    ├─ EXEC SQL OPEN C1 (runtime)
    │     ↓
    │   COBOL calls DB2 interface
    │     ↓
    │   DB2 loads plan, prepares result set
    │
    ├─ EXEC SQL FETCH C1 INTO :vars
    │     ↓
    │   DB2 retrieves row, moves to host variables
    │     ↓
    │   Sets SQLCODE (0=success, 100=EOF, <0=error)
    │
    └─ EXEC SQL CLOSE C1
          ↓
        DB2 releases result set resources
```

**Dependencies:**
- DB2 subsystem must be active
- Plan must be bound before execution
- CUSTOMER table must exist with correct schema
- DBRM library must be accessible

---

### Point 3: JCL Job Scheduler Integration

**Interface:** JCL → JES2 → COBOL Program

```
JCL Submitted
    │
    ▼
JES2 Job Entry Subsystem
    │
    ├─ Parse JCL statements
    ├─ Allocate datasets
    ├─ Load procedures (IGYWCLG, etc.)
    │
    ▼
STEP 1: COMPILE
    ├─ Execute IGYCRCTL (compiler)
    ├─ Read source from &SYSUID..CBL
    ├─ Write object to &&LOADSET
    └─ Return Code (RC) to JES2
         │
         ▼ (if RC < 4)
STEP 2: BIND
    ├─ Execute IEWBLINK (linker)
    ├─ Read object from &&LOADSET
    ├─ Link with LE libraries
    ├─ Write load module to &SYSUID..LOAD
    └─ Return Code to JES2
         │
         ▼ (if RC < 4)
STEP 3: RUN
    ├─ Execute compiled program
    ├─ Read data from &SYSUID..DATA
    ├─ Write output to PRTLINE (SYSOUT)
    └─ Return Code to JES2
         │
         ▼
JES2 Completion
    ├─ Send notification to user
    ├─ Store output in spool
    └─ Clean up temporary datasets
```

---

### Point 4: Language Environment (LE) Integration

**Interface:** COBOL Runtime → LE Services

```
COBOL Program Execution
    │
    ├─ Program Initialization
    │     ↓
    │   LE acquires storage
    │   LE initializes runtime
    │   LE sets up error handlers
    │
    ├─ Intrinsic Functions
    │   (CURRENT-DATE, LOWER-CASE, etc.)
    │     ↓
    │   COBOL calls LE service routines
    │     ↓
    │   LE performs operation, returns result
    │
    ├─ Error Conditions
    │   (Division by zero, S0C7, etc.)
    │     ↓
    │   Hardware interrupt
    │     ↓
    │   LE error handler invoked
    │     ↓
    │   Generate CEEDUMP (if not DUMMY)
    │     ↓
    │   Abnormal termination
    │
    └─ Program Termination
          ↓
        LE cleanup
        Release storage
        Return to caller (JCL)
```

**Services Used:**
- Storage management (HEAP, STACK)
- Error handling (CEEDUMP)
- Intrinsic functions (DATE, TIME, STRING)
- Math operations (COMPUTE)

---

### Point 5: SYSOUT/Output Management

**Interface:** COBOL WRITE → SYSOUT Spool

```
COBOL Program
    │
    ├─ OPEN OUTPUT PRTLINE
    │     ↓
    │   JCL: //PRTLINE DD SYSOUT=*
    │     ↓
    │   z/OS allocates spool dataset
    │
    ├─ WRITE PRTLINE FROM record
    │     ↓
    │   COBOL formats record (132 bytes)
    │     ↓
    │   Write to spool buffer
    │     ↓
    │   (buffered, not immediately visible)
    │
    └─ CLOSE PRTLINE
          ↓
        Flush buffer to spool
          ↓
        JES2 manages spool file
          ↓
        User views via SDSF or equivalent
```

**Output Characteristics:**
- LRECL=132 (standard print line)
- RECFM=FBA (Fixed Blocked with ASA control char)
- Held in spool until viewed/purged
- OUTLIM parameter limits output size

---

*End of Execution Paths & Data Access Patterns Document*
