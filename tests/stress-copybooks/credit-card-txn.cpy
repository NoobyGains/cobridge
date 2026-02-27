      ****************************************************************
      * CREDIT CARD TRANSACTION RECORD
      * SYSTEM: CARDSYS  COPYBOOK: CCARDTXN
      * FORMAT: ISO 8583 MAPPED FIELDS
      ****************************************************************
       01 CREDIT-CARD-TXN-RECORD.
          05 CC-CARD-DATA.
             10 CC-CARD-NUMBER         PIC X(19).
             10 CC-CARD-BRAND          PIC XX.
                88 CC-VISA             VALUE 'VI'.
                88 CC-MASTERCARD       VALUE 'MC'.
                88 CC-AMEX             VALUE 'AX'.
                88 CC-DISCOVER         VALUE 'DI'.
             10 CC-EXPIRY-DATE         PIC 9(4).
             10 CC-CARDHOLDER-NAME     PIC X(26).
             10 CC-CVV-PRESENT         PIC X.
          05 CC-TRANSACTION-INFO.
             10 CC-TXN-ID              PIC X(20).
             10 CC-TXN-DATE            PIC 9(8).
             10 CC-TXN-TIME            PIC 9(6).
             10 CC-TXN-TYPE            PIC XX.
                88 CC-PURCHASE         VALUE 'PU'.
                88 CC-REFUND           VALUE 'RF'.
                88 CC-REVERSAL         VALUE 'RV'.
                88 CC-PREAUTH          VALUE 'PA'.
             10 CC-TXN-AMOUNT          PIC S9(11)V99 COMP-3.
             10 CC-TXN-CURRENCY        PIC X(3).
             10 CC-ORIG-AMOUNT         PIC S9(11)V99 COMP-3.
             10 CC-ORIG-CURRENCY       PIC X(3).
             10 CC-CONVERSION-RATE     PIC 9(2)V9(6) COMP-3.
          05 CC-MERCHANT-INFO.
             10 CC-MERCHANT-ID         PIC X(15).
             10 CC-MERCHANT-NAME       PIC X(25).
             10 CC-MERCHANT-CITY       PIC X(13).
             10 CC-MERCHANT-STATE      PIC XX.
             10 CC-MERCHANT-ZIP        PIC X(10).
             10 CC-MERCHANT-COUNTRY    PIC X(3).
             10 CC-MCC-CODE            PIC 9(4).
             10 CC-TERMINAL-ID         PIC X(8).
          05 CC-AUTHORIZATION.
             10 CC-AUTH-CODE           PIC X(6).
             10 CC-AUTH-RESPONSE       PIC XX.
                88 CC-APPROVED         VALUE 'AP'.
                88 CC-DECLINED         VALUE 'DC'.
                88 CC-REFERRAL         VALUE 'RF'.
             10 CC-RESPONSE-CODE       PIC XX.
             10 CC-AVS-RESULT          PIC X.
             10 CC-CVV-RESULT          PIC X.
          05 CC-SETTLEMENT.
             10 CC-SETTLE-DATE         PIC 9(8).
             10 CC-SETTLE-AMOUNT       PIC S9(11)V99 COMP-3.
             10 CC-INTERCHANGE-FEE     PIC S9(7)V99 COMP-3.
             10 CC-PROCESSING-FEE      PIC S9(5)V99 COMP-3.
             10 CC-NET-AMOUNT          PIC S9(11)V99 COMP-3.
             10 CC-BATCH-NUMBER        PIC 9(6).
