      ****************************************************************
      * INSURANCE CLAIM RECORD - PROPERTY & CASUALTY
      * SYSTEM: CLMSYS  COPYBOOK: INCLMREC
      * LAST MODIFIED: 2024-01-15  BY: MAINFRAME OPS
      ****************************************************************
       01 INSURANCE-CLAIM-RECORD.
          05 ICR-CLAIM-HEADER.
             10 ICR-CLAIM-NUMBER       PIC X(12).
             10 ICR-POLICY-NUMBER      PIC X(15).
             10 ICR-CLAIM-TYPE         PIC XX.
                88 ICR-AUTO-CLAIM      VALUE 'AU'.
                88 ICR-HOME-CLAIM      VALUE 'HM'.
                88 ICR-LIFE-CLAIM      VALUE 'LF'.
                88 ICR-HEALTH-CLAIM    VALUE 'HE'.
             10 ICR-CLAIM-STATUS       PIC X(2).
                88 ICR-OPEN            VALUE 'OP'.
                88 ICR-CLOSED          VALUE 'CL'.
                88 ICR-PENDING         VALUE 'PD'.
                88 ICR-DENIED          VALUE 'DN'.
             10 ICR-PRIORITY           PIC 9.
                88 ICR-URGENT          VALUE 1.
                88 ICR-HIGH            VALUE 2.
                88 ICR-NORMAL          VALUE 3.
          05 ICR-CLAIMANT-INFO.
             10 ICR-CLAIMANT-ID        PIC 9(10).
             10 ICR-FIRST-NAME         PIC X(25).
             10 ICR-LAST-NAME          PIC X(35).
             10 ICR-SSN                PIC 9(9).
             10 ICR-DATE-OF-BIRTH      PIC 9(8).
             10 ICR-PHONE-NUMBER       PIC 9(10).
             10 ICR-EMAIL              PIC X(50).
          05 ICR-INCIDENT-DATA.
             10 ICR-INCIDENT-DATE      PIC 9(8).
             10 ICR-INCIDENT-TIME      PIC 9(6).
             10 ICR-REPORT-DATE        PIC 9(8).
             10 ICR-LOCATION.
                15 ICR-STREET-ADDR     PIC X(40).
                15 ICR-CITY            PIC X(25).
                15 ICR-STATE           PIC XX.
                15 ICR-ZIP-CODE        PIC 9(5).
                15 ICR-ZIP-PLUS4       PIC 9(4).
             10 ICR-DESCRIPTION        PIC X(200).
             10 ICR-POLICE-REPORT-NO   PIC X(15).
          05 ICR-FINANCIAL-DATA.
             10 ICR-CLAIMED-AMOUNT     PIC S9(9)V99 COMP-3.
             10 ICR-APPROVED-AMOUNT    PIC S9(9)V99 COMP-3.
             10 ICR-DEDUCTIBLE-AMT     PIC S9(7)V99 COMP-3.
             10 ICR-PAID-AMOUNT        PIC S9(9)V99 COMP-3.
             10 ICR-RESERVE-AMOUNT     PIC S9(9)V99 COMP-3.
             10 ICR-RECOVERY-AMT       PIC S9(7)V99 COMP-3.
             10 ICR-SUBROGATION-AMT    PIC S9(7)V99 COMP-3.
          05 ICR-PAYMENT-HISTORY OCCURS 10 TIMES.
             10 ICR-PMT-DATE           PIC 9(8).
             10 ICR-PMT-AMOUNT         PIC S9(7)V99 COMP-3.
             10 ICR-PMT-TYPE           PIC X(2).
             10 ICR-PMT-CHECK-NO       PIC 9(8).
          05 ICR-ADJUSTER-INFO.
             10 ICR-ADJUSTER-ID        PIC 9(6).
             10 ICR-ADJUSTER-NAME      PIC X(30).
             10 ICR-ASSIGN-DATE        PIC 9(8).
          05 ICR-AUDIT-TRAIL.
             10 ICR-CREATE-DATE        PIC 9(8).
             10 ICR-CREATE-TIME        PIC 9(6).
             10 ICR-CREATE-USER        PIC X(8).
             10 ICR-UPDATE-DATE        PIC 9(8).
             10 ICR-UPDATE-TIME        PIC 9(6).
             10 ICR-UPDATE-USER        PIC X(8).
