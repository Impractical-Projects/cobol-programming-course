# COBOL Programming Course - Quick Reference Guide

**Document Version:** 1.0  
**Date:** January 13, 2026

---

## Program Quick Reference

### By Functionality

#### **File I/O & Reports**
- `CBL0001` - Basic account report with headers
- `CBL0002` - Account report with all fields including comments
- `CBL0004` - Demonstration of FILLER and spacing
- `CBL0005` - Picture clause formatting (zero suppression)

#### **Conditional Logic**
- `CBL0006` - IF statement for Virginia customers
- `CBL006A` - IF statement for New York customers  
- `CBL0007` - Level-88 condition-names

#### **Calculations**
- `CBL0008` - COMPUTE verb for totals
- `CBL0009` - COMPUTE verb variant
- `CBL0010` - COMP-3 packed-decimal usage
- `PAYROL00` - Basic payroll (hours × rate)
- `PAYROL0X` - Payroll display formatting
- `EMPPAY` - Complex payroll with overtime
- `DEPTPAY` - Department average salary
- `ADDAMT` - Interactive addition calculator

#### **String Functions**
- `CBL0011` - LOWER-CASE function
- `CBL0012` - LOWER-CASE with substrings
- `COBOL` - CURRENT-DATE function

#### **Control Structures**
- `CBL0033` - PERFORM UNTIL/TIMES/THRU/VARYING

#### **Table Operations**
- `SRCHBIN` - Binary search (SEARCH ALL)
- `SRCHSER` - Sequential search (SEARCH)

#### **Database (DB2)**
- `CBLDB21` - Basic SQL cursor operations
- `CBLDB22` - Conditional SQL with dual cursors
- `CBLDB23` - State-filtered SQL queries

#### **Debugging**
- `CBL0013` - Division by zero error
- `CBL0014` - S0C7 data exception
- `CBL0106` - Array bounds error (buggy)
- `CBL0106C` - Array bounds error (fixed)

#### **Utilities**
- `HELLO` - Hello World

---

## JCL Quick Reference

### Standard Procedures

| Procedure | Steps | Usage | Example |
|-----------|-------|-------|---------|
| **IGYWC** | Compile only | Development, testing | `//COMP EXEC IGYWC,SRC=CBL0001` |
| **IGYWCL** | Compile + Link | Standard approach | `//COBRUN EXEC IGYWCL,SRC=CBL0001` |
| **IGYWCLG** | Compile + Link + Go | Simple programs | `//COBRUN EXEC IGYWCLG,SRC=HELLO` |
| **DB2CBL** | DB2 Compile + Link | DB2 programs | `//COMP EXEC DB2CBL,SRC=CBLDB21` |

### Common JCL Patterns

**Pattern 1: Simple CLG**
```jcl
//JOBNAME JOB 1,NOTIFY=&SYSUID
//COBRUN  EXEC IGYWCLG,SRC=HELLO
```

**Pattern 2: Compile-Link with Data**
```jcl
//JOBNAME JOB 1,NOTIFY=&SYSUID
//COBRUN  EXEC IGYWCL,SRC=CBL0001
//RUN     EXEC PGM=CBL0001,COND=(4,LT,BIND)
//STEPLIB  DD DSNAME=&SYSUID..LOAD,DISP=SHR
//ACCTREC  DD DSNAME=&SYSUID..DATA,DISP=SHR
//PRTLINE  DD SYSOUT=*,OUTLIM=15000
//SYSOUT   DD SYSOUT=*
//CEEDUMP  DD DUMMY
//SYSUDUMP DD DUMMY
```

**Pattern 3: DB2 Compile and Bind**
```jcl
//JOBNAME JOB 1,NOTIFY=&SYSUID
//JOBLIB  DD DSNAME=DSNV10.SDSNLOAD,DISP=SHR
//COMP    EXEC DB2CBL,SRC=CBLDB21
//BIND    EXEC PGM=IKJEFT01
//SYSTSIN DD *
  DSN SYSTEM(&DB2SYS)
  BIND PLAN(CBLDB21) PKLIST(*.CBLDB21.*)
  END
/*
```

---

## Common COBOL Patterns

### File Processing Loop
```cobol
OPEN INPUT input-file
OPEN OUTPUT output-file
PERFORM READ-RECORD
PERFORM UNTIL EOF-FLAG = 'Y'
    PERFORM PROCESS-RECORD
    PERFORM WRITE-RECORD
    PERFORM READ-RECORD
END-PERFORM
CLOSE input-file output-file
STOP RUN.

READ-RECORD.
    READ input-file
        AT END MOVE 'Y' TO EOF-FLAG
    END-READ.
```

### Binary Search
```cobol
01  TABLE-DATA.
    05  ENTRY-ITEM OCCURS 50 TIMES
        INDEXED BY IDX
        ASCENDING KEY IS KEY-FIELD.
        10  KEY-FIELD    PIC X(8).
        10  DATA-FIELD   PIC X(100).

SEARCH ALL ENTRY-ITEM
    AT END
        DISPLAY 'NOT FOUND'
    WHEN KEY-FIELD(IDX) = SEARCH-VALUE
        DISPLAY ENTRY-ITEM(IDX)
END-SEARCH.
```

### DB2 Cursor Processing
```cobol
EXEC SQL
    DECLARE C1 CURSOR FOR
    SELECT col1, col2 FROM table
    WHERE condition
END-EXEC.

EXEC SQL OPEN C1 END-EXEC.

PERFORM UNTIL SQLCODE NOT = 0
    EXEC SQL FETCH C1 INTO :var1, :var2 END-EXEC
    IF SQLCODE = 0
        (process data)
    END-IF
END-PERFORM.

EXEC SQL CLOSE C1 END-EXEC.
```

### Counter Accumulation
```cobol
01  COUNTERS.
    05  RECORD-COUNT    PIC 9(5) VALUE ZERO.
    05  TOTAL-AMOUNT    PIC 9(9)V99 VALUE ZERO.

PERFORM UNTIL EOF
    ADD 1 TO RECORD-COUNT
    ADD INPUT-AMOUNT TO TOTAL-AMOUNT
    READ input-file
END-PERFORM.

DISPLAY 'RECORDS: ' RECORD-COUNT.
DISPLAY 'TOTAL: ' TOTAL-AMOUNT.
```

---

## Data Types Quick Reference

### Numeric Types
| Type | Example | Storage | Range | Use Case |
|------|---------|---------|-------|----------|
| **Display** | `PIC 9(5)` | 5 bytes | 0-99999 | Human-readable |
| **Display Signed** | `PIC S9(5)` | 5 bytes | -99999 to +99999 | Signed display |
| **COMP-3 Packed** | `PIC S9(7)V99 COMP-3` | 5 bytes | -9999999.99 to +9999999.99 | Efficient storage |
| **COMP Binary** | `PIC 9(4) COMP` | 2 bytes | 0-9999 | Fast arithmetic |

### Character Types
| Type | Example | Storage | Use Case |
|------|---------|---------|----------|
| **Alphanumeric** | `PIC X(20)` | 20 bytes | Names, addresses |
| **Alphabetic** | `PIC A(10)` | 10 bytes | Letters only |
| **FILLER** | `FILLER PIC X(5)` | 5 bytes | Spacing, unused |

### Formatted Types
| Type | Example | Output | Use Case |
|------|---------|--------|----------|
| **Zero Suppression** | `PIC ZZZ,ZZ9` | "  1,234" | Leading zeros removed |
| **Currency** | `PIC $$,$$$,$$9.99` | "$1,234.56" | Money amounts |
| **Check Protection** | `PIC ***,**9.99` | "**1,234.56" | Check writing |

---

## Error Codes Reference

### COBOL Runtime Errors
| Error | Description | Common Cause | Example Program |
|-------|-------------|--------------|-----------------|
| **S0C7** | Data exception | Non-numeric in numeric field | CBL0014 |
| **S0C4** | Protection exception | Array bounds violation | CBL0106 |
| **Division by zero** | Arithmetic error | Divide by zero | CBL0013 |

### DB2 SQL Codes
| SQLCODE | Meaning | Action |
|---------|---------|--------|
| **0** | Success | Continue |
| **100** | No data found (EOF) | End of result set |
| **-204** | Object not found | Check table name |
| **-305** | Null value | Handle null |
| **-803** | Duplicate key | Unique constraint violation |

### JCL Return Codes
| RC | Meaning | Impact |
|----|---------|--------|
| **0** | Success | Continue |
| **4** | Warning | Usually continue |
| **8** | Error | Typically fail |
| **12** | Severe error | Fail |
| **16** | Critical error | Abort |

---

## Dataset Naming Conventions

### Standard Names
```
&SYSUID..CBL         - COBOL source library (PDS)
&SYSUID..LOAD        - Load module library (PDS/PDSE)
&SYSUID..DATA        - Test data file (sequential)
&SYSUID..DBRMLIB     - DB2 DBRM library (PDS)
&&LOADSET            - Temporary object code (pass dataset)
```

### File DDNAMEs
```
ACCTREC    - Account input file (CBL programs)
PRTLINE    - Print output file (report)
CUSTOUT    - Customer output (DB2 programs)
CARDIN     - Card input (parameter file)
SYSOUT     - System output (console)
SYSPRINT   - Compiler output
SYSLIN     - Object code (linker input)
SYSLMOD    - Load module (linker output)
```

---

## Useful Commands

### TSO/ISPF Commands
```
EDIT dataset(member)      - Edit source member
SUBMIT dataset(jcl)       - Submit JCL job
SDSF                      - View job output
LISTDS dataset            - List dataset info
DELETE dataset            - Delete dataset
RENAME old new            - Rename dataset
```

### SDSF Commands
```
ST                        - Job status
DA                        - Deferred output
H                         - Held output
O                         - Output queue
?jobname                  - Filter by jobname
S linenum                 - View job output
```

### DB2 Commands (via IKJEFT01)
```
DSN SYSTEM(subsys)        - Connect to DB2
BIND PLAN(name) ...       - Bind execution plan
RUN PROGRAM(pgm) ...      - Run DB2 program
END                       - End DB2 session
```

---

## Learning Path

### Beginner Track (Course #2 Start)
1. **HELLO** - Basic DISPLAY
2. **COBOL** - Intrinsic functions
3. **PAYROL00** - ACCEPT and COMPUTE
4. **CBL0001** - File I/O basics
5. **CBL0004** - Formatting with FILLER
6. **CBL0006** - IF statements

### Intermediate Track (Course #2 Middle)
1. **CBL0007** - Level-88 conditions
2. **CBL0008** - COMPUTE for totals
3. **CBL0010** - COMP-3 packed decimals
4. **CBL0011** - String functions
5. **CBL0033** - PERFORM variations

### Advanced Track (Course #2 End)
1. **SRCHSER** - Sequential search
2. **SRCHBIN** - Binary search
3. **CBL0013** - Error handling (div by zero)
4. **CBL0014** - Error handling (S0C7)

### Expert Track (Course #3)
1. **CBLDB21** - Basic DB2/SQL
2. **CBLDB22** - Conditional queries
3. **CBLDB23** - Parameterized queries
4. **CBL0106/C** - Debugging challenges

### Testing Track (Course #4)
1. **EMPPAY** - Complex business logic
2. **DEPTPAY** - Calculation testing

---

## Troubleshooting Guide

### Problem: S0C7 Abend
**Symptoms:** Program abends with S0C7  
**Cause:** Non-numeric data in numeric field  
**Solution:**
1. Check COMP-3 fields for corruption
2. Verify LRECL matches program definition
3. Initialize numeric fields with VALUE clause
4. Display field contents before arithmetic

### Problem: File Not Found
**Symptoms:** IEC161I/IEC130I errors  
**Cause:** Dataset not allocated or wrong name  
**Solution:**
1. Verify dataset name matches JCL
2. Check LISTDS to confirm existence
3. Verify DISP parameter in DD statement
4. Ensure &SYSUID resolves correctly

### Problem: SQL Error -204
**Symptoms:** DB2 table/view not found  
**Cause:** Table doesn't exist or wrong name  
**Solution:**
1. Verify table exists: `SELECT * FROM table`
2. Check schema/qualifier
3. Ensure BIND completed successfully
4. Verify DB2 subsystem is correct

### Problem: MAXCC=0012 (Compile)
**Symptoms:** Compile step fails with RC=12  
**Cause:** Severe syntax errors in COBOL  
**Solution:**
1. Review SYSPRINT output
2. Check for missing periods
3. Verify matching IF/END-IF, PERFORM/END-PERFORM
4. Check DATA DIVISION structure

### Problem: Output Not Appearing
**Symptoms:** Job completes but no output  
**Cause:** DUMMY DD or wrong DDNAME  
**Solution:**
1. Check for `DD DUMMY` in JCL
2. Verify DDNAME matches COBOL SELECT
3. Check SYSOUT class is viewable
4. Verify OUTLIM not exceeded

---

## Performance Tips

### File Processing
- Use COMP-3 for numeric fields (smaller, faster)
- Block files for efficiency (BLKSIZE)
- Minimize I/O with buffering
- Close files when done

### Table Operations
- Use binary search (SEARCH ALL) for large sorted tables
- Dimension tables appropriately (OCCURS clause)
- Use INDEXED BY for efficiency
- Load tables once, search multiple times

### DB2 Queries
- Use WHERE clauses to filter at database
- Fetch only needed columns
- Close cursors promptly
- Use plans with appropriate isolation level

### General
- Minimize DISPLAY statements in production
- Use COMP for frequently accessed counters
- Avoid unnecessary moves (reference in place)
- Use PERFORM rather than GO TO

---

## Additional Resources

### Documentation
- `CODEBASE_DOCUMENTATION.md` - Complete system documentation
- `ARCHITECTURE_DIAGRAMS.md` - Visual architecture and flows
- `EXECUTION_PATHS.md` - Detailed execution traces and patterns

### Course Materials
- Course #1 README - Getting started guide
- Course #2 README - Learning path
- Course #3 README - Advanced topics
- Course #4 README - Testing approach

### External Resources
- IBM Enterprise COBOL documentation
- DB2 for z/OS SQL Reference
- z/OS JCL Reference
- Language Environment Programming Guide

---

*End of Quick Reference Guide*
