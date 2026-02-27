      *================================================================*
      * TRANSACTION RECORD - COBridge Example
      *================================================================*
       01  TRANSACTION-RECORD.
           05  TXN-HEADER.
               10  TXN-ID                PIC X(20).
               10  TXN-TIMESTAMP.
                   15  TXN-DATE          PIC 9(8).
                   15  TXN-TIME          PIC 9(6).
               10  TXN-TYPE              PIC X(3).
                   88  TXN-DEPOSIT       VALUE 'DEP'.
                   88  TXN-WITHDRAWAL    VALUE 'WDR'.
                   88  TXN-TRANSFER      VALUE 'TRF'.
                   88  TXN-PAYMENT       VALUE 'PMT'.
           05  TXN-AMOUNTS.
               10  TXN-AMOUNT            PIC S9(11)V99 COMP-3.
               10  TXN-FEE               PIC S9(7)V99 COMP-3.
               10  TXN-BALANCE-BEFORE    PIC S9(13)V99 COMP-3.
               10  TXN-BALANCE-AFTER     PIC S9(13)V99 COMP-3.
           05  TXN-ACCOUNTS.
               10  TXN-FROM-ACCOUNT      PIC X(12).
               10  TXN-TO-ACCOUNT        PIC X(12).
           05  TXN-DETAILS.
               10  TXN-DESCRIPTION       PIC X(50).
               10  TXN-REFERENCE         PIC X(20).
               10  TXN-CHANNEL           PIC X(3).
                   88  TXN-ONLINE        VALUE 'ONL'.
                   88  TXN-ATM           VALUE 'ATM'.
                   88  TXN-BRANCH        VALUE 'BRN'.
                   88  TXN-MOBILE        VALUE 'MOB'.
           05  TXN-STATUS                PIC X(1).
               88  TXN-COMPLETED         VALUE 'C'.
               88  TXN-PENDING           VALUE 'P'.
               88  TXN-FAILED            VALUE 'F'.
               88  TXN-REVERSED          VALUE 'R'.
