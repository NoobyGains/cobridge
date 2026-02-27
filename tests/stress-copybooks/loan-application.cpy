      ****************************************************************
      * CONSUMER LOAN APPLICATION RECORD
      * SYSTEM: LOANSYS  COPYBOOK: LOANAPP
      * SUPPORTS UP TO 3 CO-APPLICANTS
      ****************************************************************
       01 LOAN-APPLICATION-RECORD.
          05 LA-APPLICATION-ID         PIC 9(12).
          05 LA-APP-DATE               PIC 9(8).
          05 LA-APP-STATUS             PIC XX.
          05 LA-LOAN-TYPE              PIC XX.
          05 LA-REQUESTED-AMOUNT       PIC S9(9)V99 COMP-3.
          05 LA-REQUESTED-TERM         PIC 9(3) COMP.
          05 LA-NUM-APPLICANTS         PIC 9.
          05 LA-APPLICANT OCCURS 4 TIMES.
             10 LA-APPL-TYPE           PIC X.
                88 LA-PRIMARY          VALUE 'P'.
                88 LA-CO-APPLICANT     VALUE 'C'.
                88 LA-GUARANTOR        VALUE 'G'.
             10 LA-APPL-NAME.
                15 LA-APPL-FIRST      PIC X(25).
                15 LA-APPL-MIDDLE     PIC X.
                15 LA-APPL-LAST       PIC X(35).
                15 LA-APPL-SUFFIX     PIC X(5).
             10 LA-APPL-SSN           PIC 9(9).
             10 LA-APPL-DOB           PIC 9(8).
             10 LA-APPL-ADDRESS.
                15 LA-APPL-STREET     PIC X(40).
                15 LA-APPL-CITY       PIC X(25).
                15 LA-APPL-STATE      PIC XX.
                15 LA-APPL-ZIP        PIC 9(5).
             10 LA-APPL-INCOME.
                15 LA-ANNUAL-INCOME   PIC S9(9)V99 COMP-3.
                15 LA-OTHER-INCOME    PIC S9(7)V99 COMP-3.
                15 LA-MONTHLY-DEBT    PIC S9(7)V99 COMP-3.
             10 LA-APPL-EMPLOYMENT.
                15 LA-EMPLOYER-NAME   PIC X(30).
                15 LA-EMP-YEARS       PIC 99.
                15 LA-EMP-MONTHS      PIC 99.
             10 LA-CREDIT-CHECK.
                15 LA-CREDIT-SCORE    PIC 9(3).
                15 LA-CREDIT-DATE     PIC 9(8).
                15 LA-CREDIT-BUREAU   PIC X.
                15 LA-CREDIT-STATUS   PIC XX.
          05 LA-COLLATERAL.
             10 LA-COLLAT-TYPE         PIC XX.
             10 LA-COLLAT-VALUE        PIC S9(9)V99 COMP-3.
             10 LA-COLLAT-DESC         PIC X(50).
             10 LA-VIN-NUMBER          PIC X(17).
          05 LA-DECISION.
             10 LA-APPROVED-AMOUNT     PIC S9(9)V99 COMP-3.
             10 LA-APPROVED-RATE       PIC 9V9(4) COMP-3.
             10 LA-APPROVED-TERM       PIC 9(3) COMP.
             10 LA-DECISION-DATE       PIC 9(8).
             10 LA-DECISION-CODE       PIC XX.
             10 LA-UNDERWRITER-ID      PIC X(8).
