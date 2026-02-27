      ****************************************************************
      * SWIFT MT103 SINGLE CUSTOMER CREDIT TRANSFER
      * SYSTEM: TREASURY  COPYBOOK: SWFTMT103
      * STANDARD: SWIFT FIN MT103
      ****************************************************************
       01 SWIFT-MT103-RECORD.
          05 MT-BASIC-HEADER.
             10 MT-APP-ID              PIC X.
             10 MT-SERVICE-ID          PIC XX.
             10 MT-LT-ADDR            PIC X(12).
             10 MT-SESSION-NUMBER      PIC 9(4).
             10 MT-SEQUENCE-NUMBER     PIC 9(6).
          05 MT-APPLICATION-HEADER.
             10 MT-IO-IDENTIFIER       PIC X.
             10 MT-MSG-TYPE            PIC X(3).
             10 MT-DEST-ADDR           PIC X(12).
             10 MT-PRIORITY            PIC X.
             10 MT-DELIVERY-MONITOR    PIC X.
             10 MT-OBSOLESCENCE-PRD    PIC X(3).
          05 MT-USER-HEADER.
             10 MT-BANK-PRIORITY       PIC 9(4).
             10 MT-MSG-USER-REF        PIC X(16).
          05 MT-TEXT-BLOCK.
             10 MT-SENDER-REFERENCE    PIC X(16).
             10 MT-BANK-OP-CODE        PIC X(4).
             10 MT-INSTRUCT-CODE       PIC X(35).
             10 MT-TXN-TYPE-CODE       PIC X(4).
             10 MT-VALUE-DATE          PIC 9(6).
             10 MT-CURRENCY            PIC X(3).
             10 MT-INTERBANK-AMOUNT    PIC S9(13)V99 COMP-3.
             10 MT-INSTRUCTED-AMT      PIC S9(13)V99 COMP-3.
             10 MT-EXCHANGE-RATE       PIC 9(4)V9(8) COMP-3.
             10 MT-ORDERING-CUST.
                15 MT-ORD-ACCOUNT      PIC X(34).
                15 MT-ORD-NAME         PIC X(35).
                15 MT-ORD-ADDR-LINE1   PIC X(35).
                15 MT-ORD-ADDR-LINE2   PIC X(35).
                15 MT-ORD-ADDR-LINE3   PIC X(35).
             10 MT-ORDERING-INST.
                15 MT-ORD-INST-BIC     PIC X(11).
                15 MT-ORD-INST-NAME    PIC X(35).
             10 MT-SENDER-CORRESP.
                15 MT-SNDR-COR-BIC     PIC X(11).
                15 MT-SNDR-COR-NAME    PIC X(35).
             10 MT-RECEIVER-CORRESP.
                15 MT-RCVR-COR-BIC     PIC X(11).
                15 MT-RCVR-COR-NAME    PIC X(35).
             10 MT-INTERMEDIARY.
                15 MT-INTER-BIC        PIC X(11).
                15 MT-INTER-NAME       PIC X(35).
             10 MT-ACCOUNT-WITH-INST.
                15 MT-ACCT-INST-BIC    PIC X(11).
                15 MT-ACCT-INST-NAME   PIC X(35).
             10 MT-BENEFICIARY-CUST.
                15 MT-BENE-ACCOUNT     PIC X(34).
                15 MT-BENE-NAME        PIC X(35).
                15 MT-BENE-ADDR-LINE1  PIC X(35).
                15 MT-BENE-ADDR-LINE2  PIC X(35).
             10 MT-REMIT-INFO          PIC X(140).
             10 MT-DETAILS-CHARGES     PIC X(3).
             10 MT-SENDER-CHARGES.
                15 MT-CHRG-CURRENCY    PIC X(3).
                15 MT-CHRG-AMOUNT      PIC S9(9)V99 COMP-3.
             10 MT-RECEIVER-CHARGES.
                15 MT-RCHR-CURRENCY    PIC X(3).
                15 MT-RCHR-AMOUNT      PIC S9(9)V99 COMP-3.
          05 MT-TRAILER-BLOCK.
             10 MT-CHECKSUM            PIC X(12).
             10 MT-MSG-REF             PIC X(16).
             10 MT-POSSIBLE-DUP        PIC X.
