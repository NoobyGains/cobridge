      ****************************************************************
      * ACH BATCH HEADER AND DETAIL RECORDS
      * SYSTEM: ACHSYS  COPYBOOK: ACHBATCH
      * NACHA FILE FORMAT
      ****************************************************************
       01 ACH-BATCH-RECORD.
          05 ACH-BATCH-HEADER.
             10 ACH-RECORD-TYPE        PIC 9.
             10 ACH-SERVICE-CLASS       PIC 9(3).
                88 ACH-MIXED-DEBITS-CREDITS VALUE 200.
                88 ACH-CREDITS-ONLY    VALUE 220.
                88 ACH-DEBITS-ONLY     VALUE 225.
             10 ACH-COMPANY-NAME       PIC X(16).
             10 ACH-DISCRETIONARY      PIC X(20).
             10 ACH-COMPANY-ID         PIC X(10).
             10 ACH-SEC-CODE           PIC X(3).
                88 ACH-PPD             VALUE 'PPD'.
                88 ACH-CCD             VALUE 'CCD'.
                88 ACH-WEB             VALUE 'WEB'.
                88 ACH-TEL             VALUE 'TEL'.
             10 ACH-ENTRY-DESC         PIC X(10).
             10 ACH-DESC-DATE          PIC X(6).
             10 ACH-EFFECTIVE-DATE     PIC 9(6).
             10 ACH-SETTLEMENT-DATE    PIC X(3).
             10 ACH-ORIGINATOR-STATUS  PIC X.
             10 ACH-ODFI-ID            PIC 9(8).
             10 ACH-BATCH-NUMBER       PIC 9(7).
          05 ACH-ENTRY-COUNT           PIC 9(4) COMP.
          05 ACH-ENTRY-DETAIL OCCURS 50 TIMES.
             10 ACH-DET-RECORD-TYPE    PIC 9.
             10 ACH-DET-TXN-CODE       PIC 9(2).
                88 ACH-CHECKING-CREDIT VALUE 22.
                88 ACH-CHECKING-DEBIT  VALUE 27.
                88 ACH-SAVINGS-CREDIT  VALUE 32.
                88 ACH-SAVINGS-DEBIT   VALUE 37.
             10 ACH-DET-RDFI-ID        PIC 9(8).
             10 ACH-DET-CHECK-DIGIT    PIC 9.
             10 ACH-DET-ACCOUNT-NO     PIC X(17).
             10 ACH-DET-AMOUNT         PIC 9(10) COMP.
             10 ACH-DET-INDIV-ID       PIC X(15).
             10 ACH-DET-INDIV-NAME     PIC X(22).
             10 ACH-DET-DISCRET        PIC XX.
             10 ACH-DET-ADDENDA-IND    PIC 9.
             10 ACH-DET-TRACE-NO       PIC 9(15).
          05 ACH-BATCH-CONTROL.
             10 ACH-CTL-RECORD-TYPE    PIC 9.
             10 ACH-CTL-SERVICE-CLASS  PIC 9(3).
             10 ACH-CTL-ENTRY-COUNT    PIC 9(6).
             10 ACH-CTL-ENTRY-HASH     PIC 9(10).
             10 ACH-CTL-TOTAL-DEBIT    PIC 9(12) COMP.
             10 ACH-CTL-TOTAL-CREDIT   PIC 9(12) COMP.
             10 ACH-CTL-COMPANY-ID     PIC X(10).
             10 ACH-CTL-MSG-AUTH       PIC X(19).
             10 ACH-CTL-RESERVED       PIC X(6).
             10 ACH-CTL-ODFI-ID        PIC 9(8).
             10 ACH-CTL-BATCH-NO       PIC 9(7).
