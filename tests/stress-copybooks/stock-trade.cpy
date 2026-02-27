      ****************************************************************
      * STOCK TRADE EXECUTION RECORD
      * SYSTEM: TRDSYS  COPYBOOK: STKTRADE
      * FIX PROTOCOL 4.4 MAPPED FIELDS
      ****************************************************************
       01 STOCK-TRADE-RECORD.
          05 ST-ORDER-HEADER.
             10 ST-ORDER-ID            PIC X(20).
             10 ST-EXEC-ID             PIC X(20).
             10 ST-TRADE-DATE          PIC 9(8).
             10 ST-TRADE-TIME          PIC 9(9).
             10 ST-SETTLE-DATE         PIC 9(8).
          05 ST-INSTRUMENT.
             10 ST-SYMBOL              PIC X(8).
             10 ST-CUSIP               PIC X(9).
             10 ST-ISIN                PIC X(12).
             10 ST-SEDOL               PIC X(7).
             10 ST-EXCHANGE            PIC X(4).
             10 ST-SECURITY-TYPE       PIC XX.
                88 ST-EQUITY           VALUE 'EQ'.
                88 ST-OPTION           VALUE 'OP'.
                88 ST-BOND             VALUE 'BD'.
                88 ST-FUTURE           VALUE 'FU'.
          05 ST-TRADE-DETAILS.
             10 ST-SIDE                PIC X.
                88 ST-BUY              VALUE 'B'.
                88 ST-SELL             VALUE 'S'.
                88 ST-SHORT-SELL       VALUE 'X'.
             10 ST-ORDER-TYPE          PIC XX.
                88 ST-MARKET           VALUE 'MK'.
                88 ST-LIMIT            VALUE 'LM'.
                88 ST-STOP             VALUE 'ST'.
                88 ST-STOP-LIMIT       VALUE 'SL'.
             10 ST-QUANTITY            PIC 9(10) COMP.
             10 ST-EXEC-QUANTITY       PIC 9(10) COMP.
             10 ST-LEAVES-QTY          PIC 9(10) COMP.
             10 ST-PRICE               PIC S9(7)V9(4) COMP-3.
             10 ST-STOP-PRICE          PIC S9(7)V9(4) COMP-3.
             10 ST-AVG-PRICE           PIC S9(7)V9(4) COMP-3.
             10 ST-LAST-PRICE          PIC S9(7)V9(4) COMP-3.
             10 ST-BID-PRICE           PIC S9(7)V9(4) COMP-3.
             10 ST-ASK-PRICE           PIC S9(7)V9(4) COMP-3.
             10 ST-GROSS-AMOUNT        PIC S9(13)V99 COMP-3.
             10 ST-NET-AMOUNT          PIC S9(13)V99 COMP-3.
             10 ST-COMMISSION          PIC S9(7)V99 COMP-3.
             10 ST-FEES                PIC S9(7)V99 COMP-3.
          05 ST-COUNTERPARTY.
             10 ST-BROKER-ID           PIC X(12).
             10 ST-BROKER-NAME         PIC X(30).
             10 ST-CLEARING-FIRM       PIC X(12).
             10 ST-CONTRA-BROKER       PIC X(12).
          05 ST-ACCOUNT-INFO.
             10 ST-ACCOUNT-NUMBER      PIC X(12).
             10 ST-ACCOUNT-TYPE        PIC X(2).
             10 ST-CAPACITY            PIC X.
                88 ST-AGENCY           VALUE 'A'.
                88 ST-PRINCIPAL        VALUE 'P'.
          05 ST-ALLOCATIONS OCCURS 5 TIMES.
             10 ST-ALLOC-ACCT          PIC X(12).
             10 ST-ALLOC-QTY           PIC 9(10) COMP.
             10 ST-ALLOC-PRICE         PIC S9(7)V9(4) COMP-3.
             10 ST-ALLOC-AMOUNT        PIC S9(13)V99 COMP-3.
