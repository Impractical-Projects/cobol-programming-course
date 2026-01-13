# COBOL Programming Course - System Documentation

**Document Version:** 1.0  
**Analysis Date:** January 13, 2026  
**Repository:** Impractical-Projects/cobol-programming-course

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Repository Structure](#repository-structure)
3. [COBOL Programs Inventory](#cobol-programs-inventory)
4. [System Architecture](#system-architecture)
5. [Data Structures & Record Layouts](#data-structures--record-layouts)
6. [Business Rules & Logic](#business-rules--logic)
7. [JCL Execution Patterns](#jcl-execution-patterns)
8. [Data Flow Patterns](#data-flow-patterns)
9. [Dependencies & Integration](#dependencies--integration)
10. [Cross-Reference Index](#cross-reference-index)

---

## Executive Summary

This document provides comprehensive technical documentation for the COBOL Programming Course codebase, a training repository containing educational materials for learning COBOL on z/OS platforms. The analysis reconstructs system knowledge, execution patterns, and architectural decisions across 30 COBOL programs organized into 4 progressive courses.

### Key Statistics

- **Total COBOL Programs:** 30 source files (~3,100 lines of code)
- **JCL Scripts:** 43 job control files
- **Courses:** 4 progressive learning modules
- **Primary Technologies:** IBM Enterprise COBOL v6.3, JCL, DB2, z/OS
- **Program Types:** File I/O (13), Calculations (5), DB2/SQL (3), Testing (2), Debugging (2), Searching (2), Utilities (3)

### System Purpose

This is an educational codebase designed to teach COBOL programming concepts progressively from basic syntax to advanced topics including:
- File handling and formatted output
- Business calculations and data transformations
- Table operations and search algorithms
- Database integration with DB2
- Testing and debugging techniques

---

## Repository Structure

```
cobol-programming-course/
├── COBOL Programming Course #1 - Getting Started/
│   ├── README.md (course overview)
│   └── Images/ (documentation assets)
├── COBOL Programming Course #2 - Learning COBOL/
│   ├── Labs/
│   │   ├── cbl/ (23 COBOL programs - foundational)
│   │   ├── jcl/ (27 job control scripts)
│   │   ├── jclproc/ (3 standard procedures)
│   │   └── data/ (test data files)
│   └── README.md
├── COBOL Programming Course #3 - Advanced Topics/
│   ├── Labs/
│   │   ├── cbl/ (3 DB2 programs)
│   │   ├── jcl/ (14 DB2 job scripts)
│   │   └── jclproc/ (3 DB2 procedures)
│   └── Challenges/
│       └── Debugging/ (2 debugging exercises)
├── COBOL Programming Course #4 - Testing/
│   └── Labs/
│       ├── cbl/ (2 payroll testing programs)
│       └── jcl/ (2 test execution scripts)
└── [Documentation files: README.md, CONTRIBUTING.md, etc.]
```

### Course Progression

| Course | Focus | Programs | Complexity |
|--------|-------|----------|------------|
| **#1** | Getting Started | 0 (documentation only) | Conceptual |
| **#2** | Learning COBOL | 23 | Beginner → Intermediate |
| **#3** | Advanced Topics | 5 | Advanced (DB2/SQL) |
| **#4** | Testing | 2 | Intermediate (TDD concepts) |

---

## COBOL Programs Inventory

### Course #2 - Learning COBOL (23 Programs)

#### Basic I/O Programs
| Program | LOC | Purpose | Key Concepts |
|---------|-----|---------|--------------|
| **HELLO.cobol** | 10 | Display "Hello World" | PROCEDURE DIVISION, DISPLAY verb |
| **CBL0001.cobol** | 99 | Read accounts, write report | FD, OPEN, READ, WRITE, CLOSE |
| **CBL0002.cobol** | 80 | File I/O with all fields | Complete field output including comments |
| **COBOL.cobol** | 68 | Date/time functions | CURRENT-DATE, intrinsic functions |

#### Formatting & Display Programs
| Program | LOC | Purpose | Key Concepts |
|---------|-----|---------|--------------|
| **CBL0004.cobol** | 164 | Structured formatting | FILLER, spacing, alignment |
| **CBL0005.cobol** | 164 | Picture clause formatting | ZZ,ZZZ,ZZ9.99 (zero suppression) |

#### Conditional Logic Programs
| Program | LOC | Purpose | Key Concepts |
|---------|-----|---------|--------------|
| **CBL0006.cobol** | 164 | Boolean conditionals | IF/THEN/ELSE, state filtering (Virginia) |
| **CBL006A.cobol** | 173 | Modified conditionals | State filtering (New York) - variant |
| **CBL0007.cobol** | 160 | Conditional data-names | Level 88 declarations, condition-names |

#### Arithmetic & Calculation Programs
| Program | LOC | Purpose | Key Concepts |
|---------|-----|---------|--------------|
| **CBL0008.cobol** | 195 | COMPUTE verb usage | Accumulation of limits and balances |
| **CBL0009.cobol** | 195 | COMPUTE variant | Alternative variable naming |
| **CBL0010.cobol** | 184 | USAGE COMP-3 | Packed-decimal representation |
| **ADDAMT.cobol** | 42 | Addition calculator | ACCEPT, COMPUTE, interactive input |
| **PAYROL00.cobol** | 61 | Payroll calculation | Hours × Rate = Gross Pay |
| **PAYROL0X.cobol** | 35 | Payroll display | MOVE, COMPUTE, formatted output |

#### String Manipulation Programs
| Program | LOC | Purpose | Key Concepts |
|---------|-----|---------|--------------|
| **CBL0011.cobol** | 174 | Intrinsic functions | FUNCTION LOWER-CASE |
| **CBL0012.cobol** | 169 | String functions | LOWER-CASE with substring |

#### Control Structure Programs
| Program | LOC | Purpose | Key Concepts |
|---------|-----|---------|--------------|
| **CBL0033.cobol** | 131 | PERFORM variations | UNTIL, TIMES, THRU, VARYING |

#### Table & Search Programs
| Program | LOC | Purpose | Key Concepts |
|---------|-----|---------|--------------|
| **SRCHBIN.cobol** | 74 | Binary search | SEARCH ALL, ASCENDING KEY, indexed tables |
| **SRCHSER.cobol** | 73 | Sequential search | SEARCH, VARYING, linear search |

#### Error Handling Programs
| Program | LOC | Purpose | Key Concepts |
|---------|-----|---------|--------------|
| **CBL0013.cobol** | 17 | Division by zero | Runtime error demonstration |
| **CBL0014.cobol** | 16 | S0C7 data exception | Data type mismatch abend |

### Course #3 - Advanced Topics (5 Programs)

#### DB2 Integration Programs
| Program | LOC | Purpose | Key Concepts |
|---------|-----|---------|--------------|
| **CBLDB21.cbl** | 145 | Basic SQL cursor | DECLARE CURSOR, OPEN, FETCH, CLOSE |
| **CBLDB22.cbl** | 202 | Conditional SQL queries | Dual cursor approach, wildcard filtering |
| **CBLDB23.cbl** | 189 | State-based filtering | WHERE clause with parameter, file input |

#### Debugging Challenges
| Program | LOC | Purpose | Key Concepts |
|---------|-----|---------|--------------|
| **CBL0106.cbl** | 196 | Overlimit detection (buggy) | Array bounds error (5 vs 20 elements) |
| **CBL0106C.cbl** | 205 | Overlimit detection (fixed) | Corrected array size and bounds checking |

### Course #4 - Testing (2 Programs)

#### Payroll Testing Programs
| Program | LOC | Purpose | Key Concepts |
|---------|-----|---------|--------------|
| **EMPPAY.CBL** | 55 | Employee payment | Overtime calculation, bonus logic |
| **DEPTPAY.CBL** | 37 | Department payroll | Average salary calculation |

---

## System Architecture

### Execution Environment

The COBOL programs in this course execute on z/OS mainframe systems using:
- **Compiler**: IBM Enterprise COBOL v6.3 (IGYCRCTL)
- **Linker**: IBM Binder (IEWBLINK)
- **Runtime**: Language Environment (LE)
- **Database**: DB2 for z/OS (Course #3 only)
- **Job Scheduler**: JES2 (Job Entry Subsystem)

### Standard Execution Flow

```
Source Code (.cobol) → Compiler (IGYCRCTL) → Object Code (SYSLIN)
                                                      ↓
                                            Linker (IEWBLINK)
                                                      ↓
                                            Load Module (.LOAD)
                                                      ↓
                                            Execute with Data
                                                      ↓
                                            Output (PRTLINE/SYSOUT)
```

---

## Data Structures & Record Layouts

### Customer/Account Master Record

**Used By:** CBL0001-0012, SRCHBIN, SRCHSER, CBL0106/C  
**File DDNAME:** ACCT-REC  
**Record Length:** 170 bytes  
**Organization:** Sequential, Fixed-length

**COBOL Definition:**
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

**Field Details:**
- **ACCT-NO** (8 bytes): Alphanumeric account identifier
- **ACCT-LIMIT** (5 bytes): Credit limit in packed-decimal, signed, 2 decimal places
- **ACCT-BALANCE** (5 bytes): Current balance in packed-decimal, signed, 2 decimal places
- **Names** (35 bytes): Customer name fields
- **Address** (60 bytes): Street, city/county, and state
- **RESERVED** (7 bytes): Padding/future use
- **COMMENTS** (50 bytes): Free-form text

**Sample Data:** See `/Labs/data/xdata` for human-readable format  
**Total Records:** 46 (US Presidents historical data)

---

## Business Rules & Logic

### Calculation Rules

1. **Basic Payroll (PAYROL00)**
   ```
   GROSS-PAY = HOURS × HOURLY-RATE
   ```

2. **Overtime Payroll (EMPPAY)**
   ```
   IF hours ≤ 40: PAY = hours × rate
   ELSE IF hours ≤ 80: PAY = 40×rate + (hours-40)×rate×(1+OT_RATE)
   ELSE: PAY = 80×rate×(1+OT_RATE)
   
   IF hours ≥ 150: MONTHLY = WEEKLY×4×(1+REWARD)
   ELSE: MONTHLY = WEEKLY×4
   ```

3. **Department Average (DEPTPAY)**
   ```
   AVG_SALARY = TOTAL_SALARIES ÷ NUM_EMPLOYEES
   ```

4. **Account Totals (CBL0008, CBL0009)**
   ```
   TOTAL_LIMITS = Σ(ACCT-LIMIT)
   TOTAL_BALANCES = Σ(ACCT-BALANCE)
   ```

### Validation Rules

1. **State Filtering (CBL0006, CBL0007)**
   - Count records where `USA-STATE = 'Virginia'` (or other state)
   - Level-88 condition-names provide readable code

2. **Wildcard Query (CBLDB22, CBLDB23)**
   - `'*'` = fetch all records
   - Specific value = filter to matches only

3. **Array Bounds (CBL0106C)**
   - Correctly size arrays to match iteration limits
   - `OCCURS 20 TIMES` for loop of 1 to 20

### Search Algorithms

1. **Binary Search (SRCHBIN)**
   - Requires: `ASCENDING KEY` clause, sorted data
   - Complexity: O(log n)
   - Uses: `SEARCH ALL` statement

2. **Sequential Search (SRCHSER)**
   - No sorting required
   - Complexity: O(n)
   - Uses: `SEARCH` statement with `VARYING`

---

## JCL Execution Patterns

### Standard Procedures Summary

| Procedure | Purpose | Steps | Output |
|-----------|---------|-------|--------|
| **IGYWC** | Compile only | 1 | Object code (SYSLIN) |
| **IGYWCL** | Compile & Link | 2 | Load module (.LOAD) |
| **IGYWCLG** | Compile, Link & Go | 3 | Execution results |
| **DB2CBL** | DB2 Compile & Link | 2 | Load module + DBRM |

### Execution Patterns

**Pattern A: Simple CLG**
- Used for: HELLO, PAYROL00, PAYROL0X, ADDAMT
- One-step execution
- No external data files

**Pattern B: Compile-Link with Conditional Run**
- Used for: Most CBL programs
- Separate RUN step with file DD statements
- Conditional execution: `COND=(4,LT,BIND)`

**Pattern C: DB2 Programs**
- Compile with SQL preprocessing (PARM='SQL')
- Separate BIND step for plan creation
- Execute via IKJEFT01 (TSO EXEC)

---

## Data Flow Patterns

### File-to-Report Pattern
```
Input File → READ → Process → Format → WRITE → Output Report
```
Used by: CBL0001-0012

### Table Load-and-Search Pattern
```
Input File → LOAD Table → SEARCH → DISPLAY Result
```
Used by: SRCHBIN, SRCHSER

### DB2 Cursor Pattern
```
DB2 Table → DECLARE CURSOR → OPEN → FETCH Loop → CLOSE → Output
```
Used by: CBLDB21, CBLDB22, CBLDB23

---

## Dependencies & Integration

### Compiler Dependencies
- IBM Enterprise COBOL v6.3 (IGYCRCTL)
- Language Environment (CEE libraries)
- z/OS file system

### Runtime Dependencies
- STEPLIB: Source and load libraries
- Data files: &SYSUID..DATA (where applicable)
- DB2 subsystem (for DB2 programs)

### Implicit Dependencies
- EBCDIC character encoding
- COMP-3 packed-decimal support
- JCL/JES2 job scheduler
- z/OS dataset naming conventions

---

## Cross-Reference Index

### Programs by Topic

**File I/O:** CBL0001, CBL0002, CBL0004, CBL0005, CBL0006, CBL0007, CBL0008, CBL0009, CBL0010, CBL0011, CBL0012  
**Calculations:** PAYROL00, PAYROL0X, ADDAMT, EMPPAY, DEPTPAY, CBL0008, CBL0009  
**Conditionals:** CBL0006, CBL006A, CBL0007  
**Tables:** SRCHBIN, SRCHSER, CBL0106, CBL0106C  
**DB2/SQL:** CBLDB21, CBLDB22, CBLDB23  
**String Functions:** CBL0011, CBL0012, COBOL  
**Control Structures:** CBL0033  
**Error Handling:** CBL0013, CBL0014  

### COBOL Verbs Used

**ACCEPT:** ADDAMT, PAYROL00, SRCHBIN, SRCHSER  
**DISPLAY:** All programs  
**OPEN/CLOSE:** All file I/O programs  
**READ:** All file I/O programs  
**WRITE:** All file I/O programs  
**COMPUTE:** PAYROL00, PAYROL0X, CBL0008, CBL0009, CBL0010, EMPPAY, DEPTPAY  
**SEARCH:** SRCHSER  
**SEARCH ALL:** SRCHBIN  
**EXEC SQL:** CBLDB21, CBLDB22, CBLDB23  
**PERFORM:** Most programs (various forms)  

---

## Appendix: Key Findings Summary

### System Architecture
This is an educational COBOL codebase with 30 programs organized into 4 progressive courses. Programs demonstrate fundamental through advanced COBOL concepts on z/OS.

### Major Execution Paths
1. Simple display programs (no I/O)
2. File processing with reports (sequential I/O)
3. Table operations with search (in-memory)
4. Database integration (DB2/SQL)
5. Complex calculations (payroll)

### Data Access Patterns
1. Sequential file processing (read-ahead pattern)
2. In-memory table access (load once, search multiple)
3. DB2 cursor processing (row-by-row fetch)
4. Accumulator/counter patterns (running totals)

### Implicit Dependencies
- z/OS platform (EBCDIC, COMP-3, JCL)
- IBM Enterprise COBOL compiler v6.3
- Language Environment runtime
- DB2 subsystem (for Course #3)
- Standard dataset naming conventions

### Business Rules Locations
- Payroll calculations: PAYROL00, EMPPAY
- State filtering: CBL0006, CBL0007
- Account totals: CBL0008, CBL0009
- Department averages: DEPTPAY
- Search strategies: SRCHBIN (binary), SRCHSER (sequential)

### Integration Points
1. z/OS File System (OPEN/READ/WRITE/CLOSE)
2. DB2 Database (EXEC SQL statements)
3. JCL/JES2 Job Scheduler (compile-link-execute)
4. Language Environment (intrinsic functions, error handling)
5. SYSOUT Management (report output)

---

## Document Status

**Analysis Method:** Static code inspection and pattern analysis  
**Validation Status:** Hypotheses based on code structure  
**Limitations:**
- No runtime testing performed
- Assumptions based on code inspection only
- DB2 table structures inferred from SQL statements
- JCL execution paths not verified

**Recommendations:**
1. Validate findings with actual program execution
2. Verify DB2 table structures match program expectations
3. Test error handling paths (CBL0013, CBL0014)
4. Confirm JCL procedures execute as documented
5. Review with subject matter experts for accuracy

---

## Related Documentation

- **ARCHITECTURE_DIAGRAMS.md** - Visual system architecture and data flows
- **EXECUTION_PATHS.md** - Detailed program execution traces and patterns
- **QUICK_REFERENCE.md** - Quick lookup guide for common tasks
- **README.md** - Repository overview and getting started
- **Course READMEs** - Individual course instructions and objectives

---

*End of COBOL Programming Course System Documentation v1.0*

**Last Updated:** 2026-01-13  
**Analysis Coverage:** 30 COBOL programs, 43 JCL scripts, 4 courses  
**Next Review:** After validation testing
