      ****************************************************************
      * MULTI-LEVEL OCCURS - NESTED ARRAYS
      * TEST: MULTIPLE OCCURS AT DIFFERENT LEVELS
      ****************************************************************
       01 MULTI-OCCURS-RECORD.
          05 MO-HEADER-DATA.
             10 MO-RECORD-ID          PIC 9(8).
             10 MO-RECORD-DATE        PIC 9(8).
          05 MO-DEPARTMENT OCCURS 5 TIMES.
             10 MO-DEPT-CODE           PIC X(4).
             10 MO-DEPT-NAME           PIC X(30).
             10 MO-DEPT-BUDGET         PIC S9(9)V99 COMP-3.
             10 MO-EMPLOYEE OCCURS 20 TIMES.
                15 MO-EMP-ID           PIC 9(6).
                15 MO-EMP-NAME         PIC X(30).
                15 MO-EMP-SALARY       PIC S9(7)V99 COMP-3.
                15 MO-EMP-SKILL OCCURS 5 TIMES.
                   20 MO-SKILL-CODE    PIC X(4).
                   20 MO-SKILL-LEVEL   PIC 9.
          05 MO-VARIABLE-SECTION.
             10 MO-LINE-COUNT          PIC 9(3).
             10 MO-DETAIL-LINE OCCURS 1 TO 99
                   DEPENDING ON MO-LINE-COUNT.
                15 MO-LINE-NUM         PIC 9(4).
                15 MO-LINE-DESC        PIC X(40).
                15 MO-LINE-AMT         PIC S9(7)V99 COMP-3.
          05 MO-SUMMARY.
             10 MO-TOTAL-DEPTS         PIC 9(2).
             10 MO-TOTAL-EMPS          PIC 9(5).
             10 MO-TOTAL-BUDGET        PIC S9(11)V99 COMP-3.
