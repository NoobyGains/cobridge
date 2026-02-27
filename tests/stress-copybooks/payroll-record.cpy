      ****************************************************************
      * EMPLOYEE PAYROLL MASTER RECORD
      * SYSTEM: PAYSYS  COPYBOOK: PAYROLL
      * FREQUENCY: BI-WEEKLY
      ****************************************************************
       01 PAYROLL-MASTER-RECORD.
          05 PR-EMPLOYEE-ID            PIC 9(8).
          05 PR-EMPLOYEE-INFO.
             10 PR-EMP-LAST-NAME       PIC X(30).
             10 PR-EMP-FIRST-NAME      PIC X(20).
             10 PR-EMP-MI              PIC X.
             10 PR-EMP-SSN             PIC 9(9).
             10 PR-EMP-DOB             PIC 9(8).
             10 PR-HIRE-DATE           PIC 9(8).
             10 PR-DEPT-CODE           PIC X(6).
             10 PR-JOB-CODE            PIC X(8).
             10 PR-PAY-GRADE           PIC XX.
             10 PR-PAY-FREQUENCY       PIC X.
                88 PR-WEEKLY           VALUE 'W'.
                88 PR-BIWEEKLY         VALUE 'B'.
                88 PR-SEMIMONTHLY      VALUE 'S'.
                88 PR-MONTHLY          VALUE 'M'.
          05 PR-COMPENSATION.
             10 PR-BASE-SALARY         PIC S9(9)V99 COMP-3.
             10 PR-HOURLY-RATE         PIC S9(5)V99 COMP-3.
             10 PR-REGULAR-HOURS       PIC 9(3)V99 COMP-3.
             10 PR-OT-HOURS            PIC 9(3)V99 COMP-3.
             10 PR-OT-RATE             PIC S9(5)V99 COMP-3.
             10 PR-GROSS-PAY           PIC S9(9)V99 COMP-3.
          05 PR-DEDUCTIONS.
             10 PR-FED-TAX             PIC S9(7)V99 COMP-3.
             10 PR-STATE-TAX           PIC S9(7)V99 COMP-3.
             10 PR-LOCAL-TAX           PIC S9(5)V99 COMP-3.
             10 PR-FICA-SS             PIC S9(7)V99 COMP-3.
             10 PR-FICA-MED            PIC S9(5)V99 COMP-3.
             10 PR-401K-AMOUNT         PIC S9(7)V99 COMP-3.
             10 PR-401K-EMPLOYER       PIC S9(7)V99 COMP-3.
             10 PR-HEALTH-INS          PIC S9(5)V99 COMP-3.
             10 PR-DENTAL-INS          PIC S9(5)V99 COMP-3.
             10 PR-VISION-INS          PIC S9(5)V99 COMP-3.
             10 PR-LIFE-INS            PIC S9(5)V99 COMP-3.
             10 PR-HSA-AMOUNT          PIC S9(5)V99 COMP-3.
             10 PR-UNION-DUES          PIC S9(5)V99 COMP-3.
             10 PR-GARNISHMENT         PIC S9(7)V99 COMP-3.
             10 PR-OTHER-DEDUCT        PIC S9(5)V99 COMP-3.
             10 PR-TOTAL-DEDUCTIONS    PIC S9(9)V99 COMP-3.
          05 PR-NET-PAY                PIC S9(9)V99 COMP-3.
          05 PR-YTD-ACCUMULATORS.
             10 PR-YTD-GROSS           PIC S9(9)V99 COMP-3.
             10 PR-YTD-FED-TAX         PIC S9(9)V99 COMP-3.
             10 PR-YTD-STATE-TAX       PIC S9(7)V99 COMP-3.
             10 PR-YTD-FICA-SS         PIC S9(7)V99 COMP-3.
             10 PR-YTD-FICA-MED        PIC S9(7)V99 COMP-3.
             10 PR-YTD-401K            PIC S9(9)V99 COMP-3.
             10 PR-YTD-NET-PAY         PIC S9(9)V99 COMP-3.
          05 PR-DIRECT-DEPOSIT OCCURS 3 TIMES.
             10 PR-DD-ROUTING          PIC 9(9).
             10 PR-DD-ACCOUNT          PIC X(17).
             10 PR-DD-TYPE             PIC X.
                88 PR-DD-CHECKING      VALUE 'C'.
                88 PR-DD-SAVINGS       VALUE 'S'.
             10 PR-DD-AMOUNT           PIC S9(7)V99 COMP-3.
             10 PR-DD-PERCENT          PIC 9V99 COMP-3.
