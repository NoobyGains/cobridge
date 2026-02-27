      *================================================================*
      * ACCOUNT MASTER RECORD - COBridge Example
      *================================================================*
       01  ACCOUNT-RECORD.
           05  ACCT-NUMBER               PIC X(12).
           05  ACCT-TYPE                 PIC X(2).
               88  ACCT-CHECKING         VALUE 'CK'.
               88  ACCT-SAVINGS          VALUE 'SV'.
               88  ACCT-MONEY-MARKET     VALUE 'MM'.
               88  ACCT-CD               VALUE 'CD'.
           05  ACCT-OWNER-ID            PIC 9(10).
           05  ACCT-BALANCES.
               10  ACCT-CURRENT-BAL     PIC S9(13)V99 COMP-3.
               10  ACCT-AVAILABLE-BAL   PIC S9(13)V99 COMP-3.
               10  ACCT-PENDING-BAL     PIC S9(13)V99 COMP-3.
           05  ACCT-INTEREST.
               10  ACCT-INT-RATE        PIC 9V9(4) COMP-3.
               10  ACCT-INT-ACCRUED     PIC S9(9)V99 COMP-3.
               10  ACCT-INT-YTD         PIC S9(11)V99 COMP-3.
           05  ACCT-DATES.
               10  ACCT-OPEN-DATE       PIC 9(8).
               10  ACCT-CLOSE-DATE      PIC 9(8).
               10  ACCT-LAST-TXN-DATE   PIC 9(8).
           05  ACCT-SIGNATORIES.
               10  ACCT-NUM-SIGNERS     PIC 9(1).
               10  ACCT-SIGNER          OCCURS 5 TIMES.
                   15  SIGNER-ID        PIC 9(10).
                   15  SIGNER-NAME      PIC X(30).
                   15  SIGNER-ROLE      PIC X(1).
           05  ACCT-FLAGS.
               10  ACCT-STATUS          PIC X(1).
               10  ACCT-HOLD-FLAG       PIC X(1).
               10  ACCT-FREEZE-FLAG     PIC X(1).
               10  ACCT-DORMANT-FLAG    PIC X(1).
