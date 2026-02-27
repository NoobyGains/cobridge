      ****************************************************************
      * CHAINED REDEFINES - MULTIPLE OVERLAPPING DEFINITIONS
      * TEST: A REDEFINES B, C REDEFINES A
      ****************************************************************
       01 REDEFINES-CHAIN-RECORD.
          05 RC-TRANSACTION-DATA       PIC X(50).
          05 RC-PAYMENT-VIEW REDEFINES RC-TRANSACTION-DATA.
             10 RC-PAY-TYPE            PIC XX.
             10 RC-PAY-AMOUNT          PIC S9(9)V99.
             10 RC-PAY-DATE            PIC 9(8).
             10 RC-PAY-REF             PIC X(20).
             10 RC-PAY-FILLER          PIC X(9).
          05 RC-REVERSAL-VIEW REDEFINES RC-TRANSACTION-DATA.
             10 RC-REV-TYPE            PIC XX.
             10 RC-REV-ORIG-AMT        PIC S9(9)V99.
             10 RC-REV-ORIG-DATE       PIC 9(8).
             10 RC-REV-REASON          PIC X(10).
             10 RC-REV-AUTH-BY         PIC X(8).
             10 RC-REV-FILLER          PIC X(11).
          05 RC-ADDITIONAL-DATA.
             10 RC-STATUS-BLOCK        PIC X(20).
             10 RC-STATUS-AS-FIELDS REDEFINES RC-STATUS-BLOCK.
                15 RC-STAT-CODE        PIC XX.
                15 RC-STAT-DATE        PIC 9(8).
                15 RC-STAT-USER        PIC X(8).
                15 RC-STAT-FLAG        PIC XX.
             10 RC-AMOUNT-DISPLAY      PIC 9(9)V99.
             10 RC-AMOUNT-PACKED REDEFINES RC-AMOUNT-DISPLAY
                   PIC S9(9)V99 COMP-3.
          05 RC-RECORD-TRAILER         PIC X(10).
