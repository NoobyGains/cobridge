      ****************************************************************
      * SWIFT WIRE TRANSFER MESSAGE RECORD
      * SYSTEM: WIRESYS  COPYBOOK: WIREXFER
      * BASED ON SWIFT MT103 FORMAT
      ****************************************************************
       01 WIRE-TRANSFER-RECORD.
          05 WT-MESSAGE-HEADER.
             10 WT-MSG-TYPE            PIC X(4).
             10 WT-MSG-REF-NO          PIC X(16).
             10 WT-MSG-PRIORITY        PIC X.
             10 WT-INPUT-TIME          PIC 9(10).
             10 WT-OUTPUT-DATE         PIC 9(8).
             10 WT-OUTPUT-TIME         PIC 9(6).
          05 WT-SENDER-INFO.
             10 WT-SENDER-BIC          PIC X(11).
             10 WT-SENDER-NAME         PIC X(35).
             10 WT-SENDER-ADDR1        PIC X(35).
             10 WT-SENDER-ADDR2        PIC X(35).
             10 WT-SENDER-COUNTRY      PIC XX.
          05 WT-RECEIVER-INFO.
             10 WT-RECEIVER-BIC        PIC X(11).
             10 WT-RECEIVER-NAME       PIC X(35).
             10 WT-RECEIVER-ADDR1      PIC X(35).
             10 WT-RECEIVER-ADDR2      PIC X(35).
             10 WT-RECEIVER-COUNTRY    PIC XX.
          05 WT-TRANSACTION-DATA.
             10 WT-VALUE-DATE          PIC 9(8).
             10 WT-CURRENCY-CODE       PIC X(3).
             10 WT-AMOUNT              PIC S9(13)V99 COMP-3.
             10 WT-EXCHANGE-RATE       PIC 9(2)V9(6) COMP-3.
             10 WT-CHARGES             PIC X(3).
                88 WT-CHARGES-OUR      VALUE 'OUR'.
                88 WT-CHARGES-BEN      VALUE 'BEN'.
                88 WT-CHARGES-SHA      VALUE 'SHA'.
             10 WT-CHARGE-AMOUNT       PIC S9(9)V99 COMP-3.
          05 WT-ORDERING-CUSTOMER.
             10 WT-ORD-ACCT-NO         PIC X(34).
             10 WT-ORD-NAME            PIC X(35).
             10 WT-ORD-ADDR1           PIC X(35).
             10 WT-ORD-ADDR2           PIC X(35).
             10 WT-ORD-COUNTRY         PIC XX.
          05 WT-BENEFICIARY.
             10 WT-BEN-ACCT-NO         PIC X(34).
             10 WT-BEN-NAME            PIC X(35).
             10 WT-BEN-ADDR1           PIC X(35).
             10 WT-BEN-ADDR2           PIC X(35).
             10 WT-BEN-COUNTRY         PIC XX.
          05 WT-INTERMEDIARY-BANK.
             10 WT-INTER-BIC           PIC X(11).
             10 WT-INTER-NAME          PIC X(35).
             10 WT-INTER-CLEARING      PIC X(15).
          05 WT-REFERENCE-INFO.
             10 WT-REMIT-INFO          PIC X(140).
             10 WT-SENDER-REF          PIC X(16).
             10 WT-RELATED-REF         PIC X(16).
          05 WT-COMPLIANCE.
             10 WT-SANCTION-FLAG       PIC X.
             10 WT-AML-SCORE           PIC 9(3).
             10 WT-OFAC-CHECKED        PIC X.
             10 WT-SCREENING-DATE      PIC 9(8).
             10 WT-SCREENING-TIME      PIC 9(6).
