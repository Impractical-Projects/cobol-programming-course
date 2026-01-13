# COBOL Programming Course - Architecture Diagrams

**Document Version:** 1.0  
**Date:** January 13, 2026

---

## System Component Diagram

```
┌────────────────────────────────────────────────────────────────┐
│                    z/OS Mainframe Environment                  │
│                                                                │
│  ┌──────────────────────────────────────────────────────────┐ │
│  │                 JES2 Job Entry Subsystem                 │ │
│  │                                                          │ │
│  │  ┌────────────────────────────────────────────────────┐ │ │
│  │  │          JCL Job Control Language                  │ │ │
│  │  │                                                    │ │ │
│  │  │  Step 1: COMPILE                                   │ │ │
│  │  │  ┌──────────────────────────────────────────────┐ │ │ │
│  │  │  │  Program: IGYCRCTL (COBOL Compiler v6.3)    │ │ │ │
│  │  │  │  Input:   &SYSUID..CBL(member)               │ │ │ │
│  │  │  │  Output:  SYSLIN (object code)               │ │ │ │
│  │  │  │  Parms:   Region=0M, STEPLIB libraries       │ │ │ │
│  │  │  └──────────────────────────────────────────────┘ │ │ │
│  │  │                                                    │ │ │
│  │  │  Step 2: LINK                                      │ │ │
│  │  │  ┌──────────────────────────────────────────────┐ │ │ │
│  │  │  │  Program: IEWBLINK (Linkage Editor)         │ │ │ │
│  │  │  │  Input:   SYSLIN (object code)               │ │ │ │
│  │  │  │  Output:  &SYSUID..LOAD(member)              │ │ │ │
│  │  │  │  Libs:    CEE.SCEELKED (LE runtime)          │ │ │ │
│  │  │  └──────────────────────────────────────────────┘ │ │ │
│  │  │                                                    │ │ │
│  │  │  Step 3: EXECUTE (GO)                              │ │ │
│  │  │  ┌──────────────────────────────────────────────┐ │ │ │
│  │  │  │  Program: Compiled COBOL program             │ │ │ │
│  │  │  │  Input:   &SYSUID..DATA (test data)          │ │ │ │
│  │  │  │  Output:  PRTLINE (report), SYSOUT           │ │ │ │
│  │  │  │  Runtime: Language Environment (LE)          │ │ │ │
│  │  │  └──────────────────────────────────────────────┘ │ │ │
│  │  └────────────────────────────────────────────────────┘ │ │
│  └──────────────────────────────────────────────────────────┘ │
│                                                                │
│  ┌──────────────────────────────────────────────────────────┐ │
│  │            DB2 Database Subsystem (Course #3)            │ │
│  │                                                          │ │
│  │  - CUSTOMER Table (customer records)                    │ │
│  │  - DBRM Library (Database Request Modules)              │ │
│  │  - Execution Plans (bound SQL statements)               │ │
│  │  - Load Libraries: DSNV10.SDSNLOAD, RUNLIB.LOAD        │ │
│  └──────────────────────────────────────────────────────────┘ │
└────────────────────────────────────────────────────────────────┘
```

---

## Data Flow Architecture

### Pattern 1: File-to-Report Processing (CBL0001-0012)

```
┌─────────────────────────┐
│    Input File           │
│  &SYSUID..DATA          │
│  (ACCT-REC)             │
│  170 bytes/record       │
│  Sequential, Fixed      │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│  OPEN INPUT ACCT-REC    │
│  OPEN OUTPUT PRTLINE    │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│  WRITE-HEADER           │
│  (First time only)      │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│  READ-RECORD            │  ◄──────┐
│  (Priming read)         │         │
└───────────┬─────────────┘         │
            │                       │
            ▼                       │
┌─────────────────────────┐         │
│  PERFORM UNTIL EOF      │         │
│  ┌───────────────────┐  │         │
│  │ PROCESS-RECORD    │  │         │
│  │ - Business logic  │  │         │
│  │ - Formatting      │  │         │
│  │ - Calculations    │  │         │
│  └───────────────────┘  │         │
│  ┌───────────────────┐  │         │
│  │ WRITE-RECORD      │  │         │
│  │ - Output to file  │  │         │
│  └───────────────────┘  │         │
│  ┌───────────────────┐  │         │
│  │ READ-RECORD       │──┼─────────┘
│  └───────────────────┘  │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│  WRITE-TRAILER          │
│  (Summary/totals)       │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│  CLOSE ACCT-REC         │
│  CLOSE PRTLINE          │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│    Output Report        │
│  PRTLINE (SYSOUT=*)     │
│  132 bytes/line         │
│  Formatted report       │
└─────────────────────────┘
```

### Pattern 2: Table Load and Search (SRCHBIN, SRCHSER)

```
┌─────────────────────────┐
│    Input File           │
│  &SYSUID..DATA          │
│  (ACCT-REC)             │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│  LOAD-TABLE             │
│  PERFORM VARYING I      │
│    FROM 1 BY 1          │
│    UNTIL EOF OR I > 45  │
│  ┌───────────────────┐  │
│  │ READ ACCT-REC     │  │
│  │ MOVE TO           │  │
│  │   ACCT-ENTRY(I)   │  │
│  └───────────────────┘  │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────────────────┐
│   In-Memory Table                   │
│   ACCT-TABLE                        │
│   ┌─────────────────────────────┐   │
│   │ ACCT-ENTRY(1)               │   │
│   │ ACCT-ENTRY(2)               │   │
│   │ ACCT-ENTRY(3)               │   │
│   │ ...                         │   │
│   │ ACCT-ENTRY(45)              │   │
│   └─────────────────────────────┘   │
│   INDEXED BY ACCT-IDX               │
│   ASCENDING KEY IS ACCT-NO-T        │
└───────────┬─────────────────────────┘
            │
            ▼
┌─────────────────────────┐
│  SEARCH Operation       │
│                         │
│  SRCHBIN:               │
│  ┌───────────────────┐  │
│  │ SEARCH ALL        │  │
│  │ Binary search     │  │
│  │ O(log n)          │  │
│  └───────────────────┘  │
│                         │
│  SRCHSER:               │
│  ┌───────────────────┐  │
│  │ SEARCH            │  │
│  │ Sequential        │  │
│  │ O(n)              │  │
│  └───────────────────┘  │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│  DISPLAY Result         │
│  - Found: Show record   │
│  - Not Found: Message   │
└─────────────────────────┘
```

### Pattern 3: DB2 Cursor Processing (CBLDB21-23)

```
┌─────────────────────────┐
│   DB2 CUSTOMER Table    │
│   (Database)            │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│  DECLARE CURSOR         │
│  EXEC SQL               │
│    DECLARE C1 CURSOR    │
│    FOR SELECT ...       │
│    FROM CUSTOMER        │
│    [WHERE clause]       │
│  END-EXEC               │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│  OPEN CURSOR            │
│  EXEC SQL               │
│    OPEN C1              │
│  END-EXEC               │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│  FETCH Loop             │  ◄──────┐
│  PERFORM UNTIL          │         │
│    SQLCODE NOT = 0      │         │
│  ┌───────────────────┐  │         │
│  │ EXEC SQL          │  │         │
│  │   FETCH C1 INTO   │  │         │
│  │   :host-vars      │  │         │
│  │ END-EXEC          │  │         │
│  └───────────────────┘  │         │
│  ┌───────────────────┐  │         │
│  │ IF SQLCODE = 0    │  │         │
│  │   Process record  │  │         │
│  │   Write output    │──┼─────────┘
│  │ END-IF            │  │
│  └───────────────────┘  │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│  CLOSE CURSOR           │
│  EXEC SQL               │
│    CLOSE C1             │
│  END-EXEC               │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│   Output Report         │
│   CUSTOUT (SYSOUT=*)    │
└─────────────────────────┘
```

---

## JCL Procedure Architecture

### Standard Procedures Comparison

```
┌──────────────────────────────────────────────────────────────┐
│                      IGYWC (Compile Only)                    │
├──────────────────────────────────────────────────────────────┤
│  Step 1: COMPILE                                             │
│  ├─ PGM=IGYCRCTL                                             │
│  ├─ Input: &SYSUID..CBL(SRC)                                 │
│  └─ Output: &&LOADSET (temp object code)                     │
└──────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────┐
│                    IGYWCL (Compile & Link)                   │
├──────────────────────────────────────────────────────────────┤
│  Step 1: COMPILE                                             │
│  ├─ PGM=IGYCRCTL                                             │
│  ├─ Input: &SYSUID..CBL(SRC)                                 │
│  └─ Output: &&LOADSET                                        │
│                                                              │
│  Step 2: BIND (if COMPILE RC < 4)                            │
│  ├─ PGM=IEWBLINK                                             │
│  ├─ Input: &&LOADSET                                         │
│  └─ Output: &SYSUID..LOAD(SRC)                               │
└──────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────┐
│                 IGYWCLG (Compile, Link & Go)                 │
├──────────────────────────────────────────────────────────────┤
│  Step 1: COMPILE                                             │
│  ├─ PGM=IGYCRCTL                                             │
│  ├─ Input: &SYSUID..CBL(SRC)                                 │
│  └─ Output: &&LOADSET                                        │
│                                                              │
│  Step 2: BIND (if COMPILE RC < 4)                            │
│  ├─ PGM=IEWBLINK                                             │
│  ├─ Input: &&LOADSET                                         │
│  └─ Output: &SYSUID..LOAD(SRC)                               │
│                                                              │
│  Step 3: RUN (if COMPILE & BIND both RC < 4)                 │
│  ├─ PGM=SRC (the compiled program)                           │
│  ├─ Input: (depends on program)                              │
│  └─ Output: SYSOUT                                           │
└──────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────┐
│                  DB2CBL (DB2 Compile & Link)                 │
├──────────────────────────────────────────────────────────────┤
│  Step 1: COMP (with PARM='SQL')                              │
│  ├─ PGM=IGYCRCTL                                             │
│  ├─ Input: &SYSUID..CBL(SRC)                                 │
│  ├─ Output: &&LOADSET + DBRMLIB(SRC)                         │
│  └─ Libraries: DSNV10.SDSNLOAD (DB2)                         │
│                                                              │
│  Step 2: BIND                                                │
│  ├─ PGM=IEWBLINK                                             │
│  ├─ Input: &&LOADSET                                         │
│  ├─ Output: &SYSUID..LOAD(SRC)                               │
│  └─ Libraries: DB2 + LE libraries                            │
└──────────────────────────────────────────────────────────────┘
```

---

## Data Structure Relationships

### Customer/Account Record Layout

```
ACCT-FIELDS (170 bytes total)
┌────────────────────────────────────────────────────┐
│ ACCT-NO          (8 bytes)    PIC X(8)             │ Bytes 1-8
├────────────────────────────────────────────────────┤
│ ACCT-LIMIT       (5 bytes)    PIC S9(7)V99 COMP-3  │ Bytes 9-13
├────────────────────────────────────────────────────┤
│ ACCT-BALANCE     (5 bytes)    PIC S9(7)V99 COMP-3  │ Bytes 14-18
├────────────────────────────────────────────────────┤
│ LAST-NAME        (20 bytes)   PIC X(20)            │ Bytes 19-38
├────────────────────────────────────────────────────┤
│ FIRST-NAME       (15 bytes)   PIC X(15)            │ Bytes 39-53
├────────────────────────────────────────────────────┤
│ CLIENT-ADDR      (60 bytes)                        │
│ ├─ STREET-ADDR   (25 bytes)   PIC X(25)            │ Bytes 54-78
│ ├─ CITY-COUNTY   (20 bytes)   PIC X(20)            │ Bytes 79-98
│ └─ USA-STATE     (15 bytes)   PIC X(15)            │ Bytes 99-113
├────────────────────────────────────────────────────┤
│ RESERVED         (7 bytes)    PIC X(7)             │ Bytes 114-120
├────────────────────────────────────────────────────┤
│ COMMENTS         (50 bytes)   PIC X(50)            │ Bytes 121-170
└────────────────────────────────────────────────────┘

COMP-3 Packed Decimal Format:
┌─────────┬─────────┬─────────┬─────────┬─────────┐
│  Byte 1 │  Byte 2 │  Byte 3 │  Byte 4 │  Byte 5 │
├─────────┼─────────┼─────────┼─────────┼─────────┤
│ DD DD   │ DD DD   │ DD DD   │ DD DD   │ DD DS   │
└─────────┴─────────┴─────────┴─────────┴─────────┘
D = Decimal digit (0-9)
S = Sign nibble (C=positive, D=negative)

Example: 1234567.89 stored as: 01 23 45 67 89 C
         (5 bytes vs 10 bytes for display format)
```

### Table Structure (SRCHBIN, SRCHSER)

```
ACCT-TABLE (In-Memory Array)
┌─────────────────────────────────────────┐
│  ACCT-ENTRY(1)                          │ ◄─┐
│  ┌───────────────────────────────────┐  │   │
│  │ ACCT-NO-T        PIC X(8)         │  │   │
│  │ ACCT-LIMIT-T     COMP-3           │  │   │
│  │ ACCT-BALANCE-T   COMP-3           │  │   │
│  │ LAST-NAME-T      PIC X(20)        │  │   │
│  │ FIRST-NAME-T     PIC X(15)        │  │   │
│  │ CLIENT-ADDR-T    (nested)         │  │   │
│  │ RESERVED-T       PIC X(7)         │  │   │
│  │ COMMENTS-T       PIC X(50)        │  │   │
│  └───────────────────────────────────┘  │   │
├─────────────────────────────────────────┤   │
│  ACCT-ENTRY(2)                          │   │ 170 bytes
│  [same structure]                       │   │ per entry
├─────────────────────────────────────────┤   │
│  ACCT-ENTRY(3)                          │   │
│  [same structure]                       │   │
├─────────────────────────────────────────┤   │
│  ...                                    │   │
├─────────────────────────────────────────┤   │
│  ACCT-ENTRY(45)                         │ ◄─┘
│  [same structure]                       │
└─────────────────────────────────────────┘

Total Memory: 45 × 170 = 7,650 bytes

INDEXED BY ACCT-IDX (4-byte index register)
ASCENDING KEY IS ACCT-NO-T (for binary search)
```

---

## Program Dependency Map

### Course #2 Program Dependencies

```
Input Data File (&SYSUID..DATA)
        │
        ├───► CBL0001 ──► PRTLINE (Basic report)
        ├───► CBL0002 ──► PRTLINE (All fields)
        ├───► CBL0004 ──► PRTLINE (Formatted)
        ├───► CBL0005 ──► PRTLINE (Zero suppression)
        ├───► CBL0006 ──► PRTLINE (Virginia filter)
        ├───► CBL006A ──► PRTLINE (New York filter)
        ├───► CBL0007 ──► PRTLINE (Level 88)
        ├───► CBL0008 ──► PRTLINE (Totals)
        ├───► CBL0009 ──► PRTLINE (Totals variant)
        ├───► CBL0010 ──► PRTLINE (COMP-3)
        ├───► CBL0011 ──► PRTLINE (Lowercase)
        ├───► CBL0012 ──► PRTLINE (String functions)
        ├───► SRCHBIN ──► DISPLAY (Binary search result)
        └───► SRCHSER ──► DISPLAY (Sequential search result)

Console/Terminal Input
        │
        ├───► PAYROL00 ──► DISPLAY (Gross pay)
        ├───► PAYROL0X ──► DISPLAY (Formatted pay)
        └───► ADDAMT ──► DISPLAY (Sum of amounts)

No External Dependencies
        │
        ├───► HELLO ──► DISPLAY (Hello World)
        ├───► COBOL ──► DISPLAY (Date/time)
        ├───► CBL0013 ──► ABEND (Division by zero)
        ├───► CBL0014 ──► ABEND (S0C7)
        └───► CBL0033 ──► DISPLAY (PERFORM demos)
```

### Course #3 Program Dependencies

```
DB2 CUSTOMER Table
        │
        ├───► CBLDB21 ──► CUSTOUT (All customers)
        ├───► CBLDB22 ──► CUSTOUT (Name filter)
        └───► CBLDB23 ──► CUSTOUT (State filter)
                  ▲
                  │
            CARDIN (State parameter)

Input Data File
        │
        └───► CBL0106 ──► PRTLINE (Buggy - buffer overflow)
        └───► CBL0106C ──► PRTLINE (Fixed version)
```

### Course #4 Program Dependencies

```
No External Files (Hardcoded Data)
        │
        ├───► EMPPAY ──► DISPLAY (Employee payment)
        └───► DEPTPAY ──► DISPLAY (Department average)
```

---

## Business Logic Flow Diagrams

### Payroll Calculation Logic (EMPPAY)

```
START
  │
  ▼
Initialize Variables
  │
  ▼
┌─────────────────────┐
│ Hours <= 40?        │
└──┬────────────┬─────┘
   │ YES        │ NO
   ▼            ▼
┌──────────┐  ┌────────────────────┐
│ Regular  │  │ Hours <= 80?       │
│ Pay Only │  └──┬──────────────┬──┘
└──────────┘     │ YES          │ NO
                 ▼              ▼
          ┌──────────────┐  ┌──────────────┐
          │ Base + OT    │  │ Max Cap      │
          │ 40*Rate +    │  │ 80*Rate*1.25 │
          │ (H-40)*Rate* │  └──────────────┘
          │ (1+OT-Rate)  │
          └──────────────┘
                 │
                 ▼
          ┌──────────────┐
          │ Hours >= 150?│
          └──┬────────┬──┘
            │ YES    │ NO
            ▼        ▼
       ┌────────┐  ┌────────┐
       │ Bonus  │  │ No     │
       │ Week*4*│  │ Bonus  │
       │ (1+Rwd)│  │ Week*4 │
       └────────┘  └────────┘
            │        │
            └────┬───┘
                 ▼
          Display Results
                 │
                 ▼
                END
```

### Search Algorithm Comparison

```
SRCHBIN (Binary Search)          SRCHSER (Sequential Search)
─────────────────────            ────────────────────────────

LOAD TABLE (same for both)
  │
  ▼
┌─────────────────────┐          ┌─────────────────────┐
│ SEARCH ALL          │          │ SEARCH              │
│ ACCT-ENTRY          │          │ ACCT-ENTRY          │
│                     │          │ VARYING ACCT-IDX    │
└──────────┬──────────┘          └──────────┬──────────┘
           │                                │
           ▼                                ▼
┌─────────────────────┐          ┌─────────────────────┐
│ Compare with        │          │ Compare with        │
│ middle element      │          │ current element     │
└──────────┬──────────┘          └──────────┬──────────┘
           │                                │
     ┌─────┴─────┐                   ┌─────┴─────┐
     │ Match?    │                   │ Match?    │
     └──┬─────┬──┘                   └──┬─────┬──┘
   NO   │ YES │                    NO   │ YES │
     ┌──┴──┐  │                      ┌──┴──┐  │
     │Split│  │                      │Next │  │
     │Table│  │                      │Item │  │
     └──┬──┘  │                      └──┬──┘  │
        │     │                         │     │
        └─────┼─────► FOUND ◄───────────┘     │
              │                               │
         AT END (NOT FOUND)              AT END
              │                               │
              └───────────┬───────────────────┘
                          ▼
                  Display Result

Complexity: O(log n)             Complexity: O(n)
Best for: Sorted data            Best for: Unsorted data
Requires: ASCENDING KEY          Requires: Nothing special
```

---

## Error Handling Patterns

### File I/O Error Handling

```
┌─────────────────────┐
│ OPEN INPUT file     │
└──────────┬──────────┘
           │
           ▼
     ┌─────────┐
     │ Success?│
     └──┬───┬──┘
    YES │   │ NO
        │   └──► DISPLAY ERROR
        │        STOP RUN
        ▼
┌─────────────────────┐
│ READ file           │
└──────────┬──────────┘
           │
           ▼
     ┌─────────┐
     │ AT END? │
     └──┬───┬──┘
    YES │   │ NO
        │   └──► Process Record
        │             │
        │             └──► Loop back
        ▼
┌─────────────────────┐
│ CLOSE file          │
└─────────────────────┘
```

### DB2 SQL Error Handling

```
┌─────────────────────┐
│ EXEC SQL operation  │
└──────────┬──────────┘
           │
           ▼
    ┌──────────────┐
    │ Check        │
    │ SQLCODE      │
    └──┬─────┬─────┘
       │     │
   ┌───┴─┬───┴──┬─────┐
   │ =0  │ =100 │ <0  │
   │     │      │     │
   ▼     ▼      ▼     
SUCCESS EOF   ERROR
   │     │      │
   │     │      └──► DISPLAY SQLCODE
   │     │           Error Recovery
   │     │           or STOP RUN
   │     │
   │     └──► Normal End
   │          (No more rows)
   │
   └──► Continue Processing
```

---

*End of Architecture Diagrams Document*
