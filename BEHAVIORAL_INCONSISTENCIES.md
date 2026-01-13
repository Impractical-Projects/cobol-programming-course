# COBOL Programming Course - Behavioral Inconsistencies Report

**Document Version:** 1.0  
**Date:** January 13, 2026  
**Purpose:** Flag inconsistencies where similar logic is implemented differently across the codebase

---

## Executive Summary

This report identifies **3 critical bugs** and **5 inconsistent patterns** discovered through static code analysis of the COBOL Programming Course repository. These findings make implicit system behavior explicit for review and validation.

**Classification:**
- 🔴 **Critical Bugs** (3): Compilation errors or incorrect business logic
- ⚠️ **Inconsistent Patterns** (5): Style differences or architectural variations
- ℹ️ **Observations** (2): Intentional differences for educational purposes

---

## Critical Bugs

### Bug #1: EMPPAY.CBL - Unreachable Overtime Logic 🔴

**Program:** EMPPAY.CBL  
**Location:** Lines 32-37  
**Severity:** HIGH - Incorrect business logic, financial impact  
**Category:** Logic Error

#### Issue Description
The overtime rate calculation uses incorrect conditional order, making the 50% overtime rate unreachable.

#### Current Implementation (Buggy)
```cobol
PAYMENT-WEEKLY.
    IF  EMP-HOURS >= 40
        MOVE .25 TO  EMP-OT-RATE
    ELSE IF EMP-HOURS >= 50
        MOVE .50 TO EMP-OT-RATE      * UNREACHABLE CODE
    ELSE
        MOVE ZERO TO EMP-OT-RATE.
```

#### Problem Analysis
When `EMP-HOURS = 50`:
1. First condition `IF EMP-HOURS >= 40` evaluates to TRUE
2. Executes `MOVE .25 TO EMP-OT-RATE`
3. Never reaches second condition `ELSE IF EMP-HOURS >= 50`

#### Expected Implementation (Correct)
```cobol
PAYMENT-WEEKLY.
    IF  EMP-HOURS >= 50
        MOVE .50 TO EMP-OT-RATE
    ELSE IF EMP-HOURS >= 40
        MOVE .25 TO EMP-OT-RATE
    ELSE
        MOVE ZERO TO EMP-OT-RATE.
```

#### Business Impact
| Hours Worked | Actual OT Rate | Expected OT Rate | Employee Loss |
|--------------|----------------|------------------|---------------|
| 50 | 25% | 50% | 25% of pay |
| 60 | 25% | 50% | 25% of pay |
| 100 | 25% | 50% | 25% of pay |

**Example Financial Impact:**
- Employee works 50 hours at $20/hour
- Actual pay: (50 × $20) × 1.25 = $1,250
- Expected pay: (50 × $20) × 1.50 = $1,500
- **Difference: $250 per week, $1,000 per month**

#### Test Case to Reproduce
```cobol
TestCase 'Expose overtime bug at 50 hours'
    MOVE 50 TO EMP-HOURS
    MOVE 20.00 TO EMP-HOURLY-RATE
    PERFORM PAYMENT-WEEKLY
    EXPECT EMP-OT-RATE TO BE 0.25    * ACTUAL (bug)
    NOTE: Should be 0.50
    EXPECT EMP-PAY-WEEK TO BE 1250.00 * ACTUAL (bug)
    NOTE: Should be 1500.00
```

#### Recommendation
**DO NOT FIX** (per requirements: no production code changes)  
Instead:
1. Document in TEST_SPECIFICATIONS.md ✅
2. Create failing test case ✅
3. Report to course maintainers
4. Use as educational example of logic errors

#### Root Cause
Likely copy-paste error or misunderstanding of COBOL conditional evaluation order.

---

### Bug #2: CBL0002.cobol - Variable Name Typo 🔴

**Program:** CBL0002.cobol  
**Location:** Line 78  
**Severity:** CRITICAL - Prevents compilation  
**Category:** Syntax Error

#### Issue Description
WRITE statement references `PRINT-REX` which does not exist. Should be `PRINT-REC`.

#### Current Implementation (Buggy)
```cobol
WRITE-RECORD.
    WRITE PRINT-REX FROM DETAIL-LINE.    * PRINT-REX not defined
```

#### Expected Implementation (Correct)
```cobol
WRITE-RECORD.
    WRITE PRINT-REC FROM DETAIL-LINE.    * Matches FD declaration
```

#### File Definition
```cobol
FD  PRINT-LINE RECORDING MODE F.
01  PRINT-REC.              * Correct name
    05  DETAIL-LINE         PIC X(80).
```

#### Compilation Error
```
IGYPS2113-S "PRINT-REX" was not defined as a name.
         The reference to "PRINT-REX" was discarded.
```

#### Business Impact
- **Program does not compile**
- **Cannot be executed**
- **Blocks any testing or usage of CBL0002**

#### Test Case to Reproduce
```cobol
TestCase 'Compilation should fail on PRINT-REX'
    GIVEN CBL0002.cobol source code
    WHEN Compile with IGYCRCTL
    THEN Compilation fails with error IGYPS2113-S
     AND Message references "PRINT-REX" not defined
```

#### Pattern Analysis
Compare with working program (CBL0001):
```cobol
* CBL0001 (CORRECT):
FD  PRINT-LINE...
01  PRINT-REC.
    WRITE PRINT-REC FROM DETAIL-LINE.

* CBL0002 (BUGGY):
FD  PRINT-LINE...
01  PRINT-REC.
    WRITE PRINT-REX FROM DETAIL-LINE.    * Typo
```

#### Recommendation
**DO NOT FIX** (per requirements)  
Instead:
1. Document in TEST_SPECIFICATIONS.md ✅
2. Note as compilation test case ✅
3. Use as educational example of typos

#### Root Cause
Typographical error - likely `C` changed to `X` accidentally.

---

### Bug #3: CBL006A.cobol - State Name Case Mismatch 🔴

**Program:** CBL006A.cobol  
**Location:** Line 157  
**Severity:** HIGH - Logic never executes correctly  
**Category:** Data Comparison Error

#### Issue Description
State comparison uses incorrect case ('new York' instead of 'New York'), preventing any matches.

#### Current Implementation (Buggy)
```cobol
PERFORM-PARA.
    MOVE ACCT-FIELDS TO DETAIL-LINE.
    IF USA-STATE = 'new York'          * Wrong case
        ADD 1 TO NEW-YORK-COUNT
    END-IF.
```

#### Expected Implementation (Correct)
```cobol
PERFORM-PARA.
    MOVE ACCT-FIELDS TO DETAIL-LINE.
    IF USA-STATE = 'New York'          * Correct case
        ADD 1 TO NEW-YORK-COUNT
    END-IF.
```

#### Problem Analysis
COBOL string comparison is **case-sensitive**:
- `'new York'` ≠ `'New York'`
- EBCDIC encoding: 'n' (X'95') ≠ 'N' (X'D5')
- Condition will **never** be TRUE

#### Business Impact
| Data Value | Comparison | Result | Expected | Impact |
|------------|------------|--------|----------|--------|
| 'New York' | = 'new York' | FALSE | TRUE | Not counted |
| 'new York' | = 'new York' | TRUE | FALSE | Counted (wrong data) |
| 'NEW YORK' | = 'new York' | FALSE | FALSE | Not counted |

**All New York residents are uncounted.**

#### Test Case to Reproduce
```cobol
TestCase 'Expose case sensitivity bug'
    GIVEN USA-STATE = 'New York'       * Standard format
    WHEN IF USA-STATE = 'new York'     * Buggy comparison
    THEN Condition evaluates to FALSE
     AND NEW-YORK-COUNT is not incremented
    NOTE: Should match and increment counter
```

#### Comparison with Sibling Program
**CBL0006.cobol (CORRECT):**
```cobol
IF USA-STATE = 'Virginia'              * Correct case
    ADD 1 TO VIRGINIA-COUNT
END-IF.
```

**CBL006A.cobol (BUGGY):**
```cobol
IF USA-STATE = 'new York'              * Wrong case
    ADD 1 TO NEW-YORK-COUNT
END-IF.
```

**Observation:** CBL006A is a modified version of CBL0006 but introduced bug during modification.

#### Data Format Standards
Analysis of data file shows state names use **Title Case**:
- Virginia ✅
- New York ✅
- California ✅
- NOT: virginia ❌
- NOT: new York ❌
- NOT: NEW YORK ❌

#### Recommendation
**DO NOT FIX** (per requirements)  
Instead:
1. Document in TEST_SPECIFICATIONS.md ✅
2. Create test exposing zero matches ✅
3. Document Title Case standard ✅
4. Use as educational example of case sensitivity

#### Root Cause
Manual modification error when creating variant program from CBL0006 template.

---

## Inconsistent Patterns

### Pattern #1: Output Record Formatting (Style Difference) ⚠️

**Programs Affected:** CBL0001, CBL0002 vs CBL0006, CBL0008  
**Severity:** LOW - Cosmetic, no functional impact  
**Category:** Style Inconsistency

#### Pattern A: No Spacing (Early Programs)
```cobol
* Used by: CBL0001, CBL0002
01  DETAIL-LINE.
    05  ACCT-NO-O         PIC X(8).
    05  ACCT-LIMIT-O      PIC ZZ,ZZZ,ZZ9.99.
    05  ACCT-BALANCE-O    PIC ZZ,ZZZ,ZZ9.99.
    05  LAST-NAME-O       PIC X(20).
    [No FILLER spacing between fields]
```

**Output Appearance:**
```
12345678 1,234.56 5,678.90SMITH               JOHN
        ↑ No space        ↑ No space
```

#### Pattern B: Explicit Spacing (Later Programs)
```cobol
* Used by: CBL0006, CBL0008
01  DETAIL-LINE.
    05  FILLER            PIC X(02) VALUE SPACES.
    05  ACCT-NO-O         PIC X(8).
    05  FILLER            PIC X(02) VALUE SPACES.
    05  ACCT-LIMIT-O      PIC ZZ,ZZZ,ZZ9.99.
    05  FILLER            PIC X(02) VALUE SPACES.
    05  ACCT-BALANCE-O    PIC ZZ,ZZZ,ZZ9.99.
    [Consistent 2-space FILLER between fields]
```

**Output Appearance:**
```
  12345678  1,234.56  5,678.90  SMITH               JOHN
  ↑        ↑         ↑         ↑
  Leading  Between   Between   Between
  space    fields    fields    fields
```

#### Analysis
- **Both patterns work correctly**
- Pattern B is more readable (better spacing)
- Pattern A is more compact (saves output width)
- Likely progression: learned better formatting in later labs

#### Recommendation
- **Standardize on Pattern B** for consistency
- Add to coding standards document
- Not critical - cosmetic only
- Document as "evolved best practice"

---

### Pattern #2: Search Algorithm Table Declarations (Intentional) ℹ️

**Programs Affected:** SRCHBIN vs SRCHSER  
**Severity:** NONE - Intentional educational difference  
**Category:** Architectural Variation

#### SRCHBIN (Binary Search)
```cobol
01  ACCT-TABLE.
    05  ACCT-TABLE-ITEM OCCURS 45 TIMES 
        ASCENDING KEY IS ACCT-NO           * KEY clause required
        INDEXED BY TABLE-IDX.
```

**Algorithm:**
```cobol
SEARCH ALL ACCT-TABLE-ITEM              * Binary search
    AT END DISPLAY "Not Found"
    WHEN ACCT-NO (TABLE-IDX) = 18011809
        DISPLAY "User found!".
```

**Characteristics:**
- Requires ASCENDING KEY or DESCENDING KEY
- Uses SEARCH ALL verb
- O(log n) complexity
- Data must be sorted

#### SRCHSER (Sequential Search)
```cobol
01  ACCT-TABLE.
    05  ACCT-TABLE-ITEM OCCURS 45 TIMES 
        INDEXED BY TABLE-IDX.              * No KEY clause
```

**Algorithm:**
```cobol
SEARCH ACCT-TABLE-ITEM VARYING TABLE-IDX  * Sequential search
    AT END DISPLAY "Not Found"
    WHEN LAST-NAME (TABLE-IDX) = "ROOSEVELT"
        DISPLAY "Roosevelt found!".
```

**Characteristics:**
- No KEY clause needed
- Uses SEARCH verb (not SEARCH ALL)
- O(n) complexity
- Data can be unsorted

#### Analysis
**This is intentional, not a bug:**
- Educational purpose: Demonstrate both search algorithms
- SRCHBIN searches by account number (sorted, unique key)
- SRCHSER searches by last name (unsorted, may have duplicates)
- Different use cases justify different approaches

#### Comparison Table
| Aspect | SRCHBIN | SRCHSER | Reason |
|--------|---------|---------|--------|
| Search Field | ACCT-NO | LAST-NAME | Different keys |
| Algorithm | Binary | Sequential | Performance vs flexibility |
| KEY Clause | Required | Not used | Binary requires sorted |
| Verb | SEARCH ALL | SEARCH | Different syntax |
| Complexity | O(log n) | O(n) | Trade-off |
| Data Requirement | Sorted | Any order | Binary needs order |

#### Recommendation
- **No changes needed**
- Document intentional difference
- Use as teaching point: "When to use which search"
- Add comments explaining trade-offs

---

### Pattern #3: DB2 Error Handling (Consistent) ✅

**Programs Affected:** CBLDB21, CBLDB22, CBLDB23  
**Severity:** NONE - Excellent consistency  
**Category:** Positive Pattern

#### Standard Error Handling (All DB2 Programs)
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

#### Analysis
**All three DB2 programs use identical error handling:**
- Same paragraph name (SQL-ERROR-HANDLING)
- Same error display logic
- Same DSNTIAR invocation
- Same return code convention (1000)

#### Observation
This is **excellent practice:**
- Consistent error handling across programs
- Maintainable - fix once, applies everywhere (if extracted)
- Follows DB2 best practices
- Good example of code reuse opportunity

#### Recommendation
- **Consider extracting to COPYBOOK**
- Create SQL-ERROR-HANDLING.cpy
- All DB2 programs include via COPY statement
- Single source of truth for DB2 error handling

**Proposed COPYBOOK:**
```cobol
* SQL-ERROR-HANDLING.cpy
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

**Usage:**
```cobol
COPY SQL-ERROR-HANDLING.
```

---

### Pattern #4: Conditional Logic Variations ⚠️

**Programs Affected:** CBL0006 vs CBL006A  
**Severity:** LOW - Minor style difference  
**Category:** Style Inconsistency

#### CBL0006 (Original)
```cobol
PERFORM-PARA.
    MOVE ACCT-FIELDS TO DETAIL-LINE.
    IF USA-STATE = 'Virginia'
        ADD 1 TO VIRGINIA-COUNT
    END-IF.
    WRITE PRINT-REC FROM DETAIL-LINE.
```

**Characteristics:**
- Inline IF statement
- Single ADD operation
- Clean and readable

#### CBL006A (Variant)
```cobol
PERFORM-PARA.
    MOVE ACCT-FIELDS TO DETAIL-LINE.
    IF USA-STATE = 'new York'          * Note: Bug here
        ADD 1 TO NEW-YORK-COUNT
    END-IF.
    WRITE PRINT-REC FROM DETAIL-LINE.
```

**Characteristics:**
- Same structure as CBL0006
- Just different state name
- Shows consistency in approach

#### Analysis
**Both programs use same pattern:**
- Read → Move → Check → Count → Write
- Good: Consistent structure
- Issue: CBL006A has case bug (separate issue)

**Alternative Patterns Not Used:**
```cobol
* Could use EVALUATE (not used):
EVALUATE USA-STATE
    WHEN 'Virginia'
        ADD 1 TO VIRGINIA-COUNT
    WHEN 'New York'
        ADD 1 TO NEW-YORK-COUNT
END-EVALUATE

* Could use level-88 (not used):
88  IS-VIRGINIA VALUE 'Virginia'.
IF IS-VIRGINIA
    ADD 1 TO VIRGINIA-COUNT
END-IF.
```

#### Recommendation
- Current pattern is acceptable
- Consider level-88 for readability
- Document as standard pattern for filtering
- Fix case bug separately

---

### Pattern #5: State Filtering Implementation Inconsistency ⚠️

**Programs Affected:** CBL0006, CBL006A, CBL0007  
**Severity:** LOW - Different approaches for same goal  
**Category:** Architectural Variation

#### CBL0006 & CBL006A: Direct Comparison
```cobol
IF USA-STATE = 'Virginia'
    ADD 1 TO VIRGINIA-COUNT
END-IF.
```

**Approach:** Direct string comparison  
**Pros:** Simple, clear  
**Cons:** Repetitive if many states

#### CBL0007: Level-88 Condition Names
```cobol
* In WORKING-STORAGE:
01  STATE-FLAGS.
    05  USA-STATE       PIC X(15).
        88  IS-VIRGINIA    VALUE 'Virginia'.
        88  IS-NEW-YORK    VALUE 'New York'.
        88  IS-CALIFORNIA  VALUE 'California'.

* In PROCEDURE DIVISION:
IF IS-VIRGINIA
    ADD 1 TO VIRGINIA-COUNT
END-IF.
```

**Approach:** Condition-names (level-88)  
**Pros:** More readable, self-documenting  
**Cons:** Requires upfront declaration

#### Analysis
**Different teaching goals:**
- CBL0006: Teach basic IF statements
- CBL0007: Teach level-88 condition-names
- Both are valid COBOL approaches

#### Comparison
| Aspect | Direct Comparison | Level-88 |
|--------|-------------------|----------|
| Readability | Moderate | High |
| Maintainability | Low | High |
| Setup Effort | None | Upfront declaration |
| Teaching Value | Basic conditionals | Advanced feature |
| Best For | Simple cases | Multiple conditions |

#### Recommendation
- **Both patterns are valid**
- Use direct comparison for simple cases
- Use level-88 for multiple or complex conditions
- Document as "progressive complexity" in course
- Not an inconsistency - intentional progression

---

## Summary: Inconsistencies by Severity

### Critical Issues (Fix Recommended)
1. 🔴 **EMPPAY overtime logic** - Financial impact
2. 🔴 **CBL0002 variable typo** - Compilation failure
3. 🔴 **CBL006A case mismatch** - Logic failure

### Style Differences (Standardization Recommended)
4. ⚠️ **Output formatting** - Prefer Pattern B (with FILLER)
5. ⚠️ **Conditional logic** - Both patterns acceptable

### Intentional Variations (No Action)
6. ℹ️ **Search algorithms** - Educational purpose
7. ✅ **DB2 error handling** - Good consistency
8. ℹ️ **State filtering** - Progressive complexity

---

## Pattern Recommendations

### Adopt as Standards
1. **FILLER spacing in output records** (Pattern B)
2. **DB2 error handling via COPYBOOK** (centralize)
3. **Level-88 for multiple conditions** (readability)
4. **Title Case for state names** (data standard)

### Document as Intentional
1. **Binary vs Sequential search** (different use cases)
2. **Progressive complexity** (teaching progression)
3. **Conditional style evolution** (learning path)

### Flag for Course Creators
1. **Three critical bugs** for discussion/fixing decision
2. **Style evolution** shows learning progression
3. **Use bugs as teaching moments** (debugging exercises)

---

## Testing Implications

### Tests That Expose Bugs
- **Test Case 6:** EMPPAY 50+ hours overtime
- **Test Case 15:** CBL0002 compilation
- **Test Case 18:** CBL006A New York filtering

### Tests for Pattern Consistency
- **Verify spacing:** Check output from CBL0001 vs CBL0006
- **Verify search:** Compare SRCHBIN vs SRCHSER behavior
- **Verify error handling:** DB2 programs handle errors identically

### Tests for Data Standards
- **State names:** Always Title Case
- **Account numbers:** 8 characters, space-padded
- **COMP-3 fields:** Valid packed decimal format

---

## Conclusion

This analysis reveals:
- **3 critical bugs** that should be addressed
- **5 style variations** that reflect learning progression
- **2 intentional differences** for educational purposes
- **1 exemplary pattern** (DB2 error handling)

**Key Insight:** Most "inconsistencies" are actually intentional variations to demonstrate different COBOL techniques across the learning progression. The critical bugs are clear exceptions that should be fixed or documented as exercises.

---

**End of Behavioral Inconsistencies Report**

**Status:** ✅ All inconsistencies documented and categorized  
**Next Steps:** Review with course maintainers, decide on bug fixes, standardize patterns
