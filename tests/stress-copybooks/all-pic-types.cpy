      ****************************************************************
      * ALL PIC TYPES - COMPREHENSIVE PIC CLAUSE TESTING
      * TEST: EVERY POSSIBLE PIC CHARACTER TYPE
      ****************************************************************
       01 ALL-PIC-TYPES-RECORD.
          05 APT-ALPHANUMERIC          PIC X(20).
          05 APT-ALPHABETIC            PIC A(15).
          05 APT-NUMERIC-DISPLAY       PIC 9(8).
          05 APT-SIGNED-NUMERIC        PIC S9(7).
          05 APT-DECIMAL-NUMERIC       PIC 9(5)V99.
          05 APT-SIGNED-DECIMAL        PIC S9(7)V99.
          05 APT-ZERO-SUPPRESS         PIC Z(6)9.
          05 APT-ZERO-SUPP-DEC         PIC Z(5)9.99.
          05 APT-ASTERISK-FILL         PIC *(6)9.
          05 APT-ASTERISK-DEC          PIC *(5)9.99.
          05 APT-BLANK-INSERT          PIC 9(3)B9(3)B9(3).
          05 APT-CREDIT-SYMBOL         PIC 9(7).99CR.
          05 APT-DEBIT-SYMBOL          PIC 9(7).99DB.
          05 APT-PLUS-SIGN             PIC +9(6).99.
          05 APT-MINUS-SIGN            PIC -9(6).99.
          05 APT-ZERO-INSERT           PIC 9(4)09(4).
          05 APT-SLASH-EDIT            PIC 99/99/9999.
          05 APT-COMMA-EDIT            PIC 9(3),9(3),9(3).
          05 APT-FLOAT-PLUS            PIC +(5)9.99.
          05 APT-FLOAT-MINUS           PIC -(5)9.99.
          05 APT-FLOAT-DOLLAR          PIC $(6)9.99.
          05 APT-MIXED-ALPHA-NUM       PIC X(5)9(5).
          05 APT-COMP-FIELD            PIC S9(8) COMP.
          05 APT-COMP3-FIELD           PIC S9(7)V99 COMP-3.
          05 APT-BINARY-FIELD          PIC 9(9) BINARY.
          05 APT-PACKED-DEC            PIC S9(5)V9(3) PACKED-DECIMAL.
          05 APT-COMP1-FIELD           COMP-1.
          05 APT-COMP2-FIELD           COMP-2.
