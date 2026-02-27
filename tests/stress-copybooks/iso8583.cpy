      ****************************************************************
      * ISO 8583 PAYMENT MESSAGE FORMAT
      * SYSTEM: PAYMENT SWITCH  COPYBOOK: ISO8583
      * STANDARD: ISO 8583:1993 FINANCIAL TRANSACTION
      ****************************************************************
       01 ISO8583-MESSAGE.
          05 ISO-MSG-TYPE              PIC 9(4).
             88 ISO-AUTH-REQUEST       VALUE 0100.
             88 ISO-AUTH-RESPONSE      VALUE 0110.
             88 ISO-FINANCIAL-REQ      VALUE 0200.
             88 ISO-FINANCIAL-RSP      VALUE 0210.
             88 ISO-REVERSAL-REQ       VALUE 0400.
             88 ISO-REVERSAL-RSP       VALUE 0410.
          05 ISO-BITMAP                PIC X(16).
          05 ISO-PAN                   PIC X(19).
          05 ISO-PROC-CODE             PIC 9(6).
          05 ISO-TXN-AMOUNT            PIC 9(12) COMP.
          05 ISO-SETTLE-AMOUNT         PIC 9(12) COMP.
          05 ISO-CARDHOLDER-AMOUNT     PIC 9(12) COMP.
          05 ISO-TRANSMISSION-DT       PIC 9(10).
          05 ISO-SETTLE-CONV-RATE      PIC 9(8).
          05 ISO-CARD-CONV-RATE        PIC 9(8).
          05 ISO-SYSTEM-TRACE          PIC 9(6).
          05 ISO-LOCAL-TXN-TIME        PIC 9(6).
          05 ISO-LOCAL-TXN-DATE        PIC 9(4).
          05 ISO-EXPIRY-DATE           PIC 9(4).
          05 ISO-SETTLE-DATE           PIC 9(4).
          05 ISO-CONVERSION-DATE       PIC 9(4).
          05 ISO-CAPTURE-DATE          PIC 9(4).
          05 ISO-MCC                   PIC 9(4).
          05 ISO-ACQUIRING-COUNTRY     PIC 9(3).
          05 ISO-POS-ENTRY-MODE        PIC 9(3).
          05 ISO-CARD-SEQ-NUM          PIC 9(3).
          05 ISO-NII                   PIC 9(3).
          05 ISO-POS-CONDITION         PIC XX.
          05 ISO-POS-PIN-CAPTURE       PIC XX.
          05 ISO-AUTH-ID-RESP          PIC X(6).
          05 ISO-RESPONSE-CODE         PIC XX.
          05 ISO-TERMINAL-ID           PIC X(8).
          05 ISO-MERCHANT-ID           PIC X(15).
          05 ISO-CARD-ACCEPTOR-NAME    PIC X(40).
          05 ISO-ADDITIONAL-DATA       PIC X(25).
          05 ISO-TRACK2-DATA           PIC X(37).
          05 ISO-RETRIEVAL-REF         PIC X(12).
          05 ISO-APPROVAL-CODE         PIC X(6).
          05 ISO-ACTION-CODE           PIC X(3).
          05 ISO-TERMINAL-DATA.
             10 ISO-TERM-COUNTRY       PIC 9(3).
             10 ISO-TERM-STATE         PIC X(3).
             10 ISO-TERM-CITY          PIC X(15).
          05 ISO-TXN-CURRENCY          PIC 9(3).
          05 ISO-SETTLE-CURRENCY       PIC 9(3).
          05 ISO-PIN-DATA              PIC X(8).
          05 ISO-SECURITY-INFO         PIC X(16).
          05 ISO-ADDITIONAL-AMOUNTS    PIC X(20).
          05 ISO-ICC-DATA              PIC X(255).
          05 ISO-PRIVATE-DATA.
             10 ISO-PRIV-NETWORK-ID    PIC X(4).
             10 ISO-PRIV-TXN-ID        PIC X(20).
             10 ISO-PRIV-BATCH-NUM     PIC 9(6).
             10 ISO-PRIV-ORIG-TXN      PIC X(20).
             10 ISO-PRIV-AUTH-SOURCE   PIC X.
