      ****************************************************************
      * MIXED FORMAT - LEVEL 01, 77, 88 CONDITIONS
      * TEST: VARIOUS LEVEL NUMBERS AND SPECIAL LEVELS
      ****************************************************************
       77 WS-RECORD-COUNT              PIC 9(8) COMP.
       77 WS-ERROR-FLAG                PIC X.
          88 WS-NO-ERROR               VALUE 'N'.
          88 WS-ERROR-FOUND            VALUE 'Y'.
       77 WS-PROCESS-DATE              PIC 9(8).
       77 WS-TOTAL-AMOUNT              PIC S9(11)V99 COMP-3.
       01 MAIN-RECORD.
          05 MR-HEADER.
             10 MR-RECORD-TYPE         PIC XX.
                88 MR-DETAIL           VALUE 'DT'.
                88 MR-HEADER-REC       VALUE 'HD'.
                88 MR-TRAILER          VALUE 'TR'.
             10 MR-SEQUENCE-NO         PIC 9(6).
          05 MR-CUSTOMER-DATA.
             10 MR-CUST-ID             PIC 9(10).
             10 MR-CUST-NAME           PIC X(30).
             10 MR-CUST-TYPE           PIC X.
                88 MR-INDIVIDUAL       VALUE 'I'.
                88 MR-CORPORATE        VALUE 'C'.
                88 MR-GOVERNMENT       VALUE 'G'.
             10 MR-CUST-STATUS         PIC X.
                88 MR-ACTIVE           VALUE 'A'.
                88 MR-INACTIVE         VALUE 'I'.
                88 MR-SUSPENDED        VALUE 'S'.
          05 MR-FINANCIAL.
             10 MR-BALANCE             PIC S9(11)V99 COMP-3.
             10 MR-CREDIT-LIMIT        PIC S9(9)V99 COMP-3.
             10 MR-AVAILABLE           PIC S9(9)V99 COMP-3.
       01 ERROR-RECORD.
          05 ER-ERROR-CODE             PIC 9(4).
          05 ER-ERROR-MSG              PIC X(80).
          05 ER-ERROR-DATE             PIC 9(8).
          05 ER-ERROR-TIME             PIC 9(6).
          05 ER-ERROR-PROGRAM          PIC X(8).
