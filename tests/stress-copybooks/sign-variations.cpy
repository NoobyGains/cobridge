      ****************************************************************
      * SIGN VARIATIONS - LEADING/TRAILING/SEPARATE
      * TEST: SIGN CLAUSE HANDLING
      * NOTE: PARSER MAY NEED TO HANDLE SIGN CLAUSE KEYWORDS
      ****************************************************************
       01 SIGN-VARIATIONS-RECORD.
          05 SV-STANDARD-SIGNED        PIC S9(7)V99.
          05 SV-UNSIGNED               PIC 9(7)V99.
          05 SV-COMP3-SIGNED           PIC S9(7)V99 COMP-3.
          05 SV-COMP-SIGNED            PIC S9(7) COMP.
          05 SV-DISPLAY-SIGNED         PIC S9(5).
          05 SV-POSITIVE-ONLY          PIC 9(9)V99.
          05 SV-LARGE-SIGNED           PIC S9(15)V99 COMP-3.
          05 SV-SMALL-SIGNED           PIC S9(2) COMP.
          05 SV-SIGNED-GROUP.
             10 SV-GRP-AMOUNT1         PIC S9(7)V99 COMP-3.
             10 SV-GRP-AMOUNT2         PIC S9(7)V99 COMP-3.
             10 SV-GRP-AMOUNT3         PIC S9(7)V99 COMP-3.
          05 SV-MIXED-SIGNS.
             10 SV-MIX-POS             PIC 9(5).
             10 SV-MIX-NEG             PIC S9(5).
             10 SV-MIX-PACKED          PIC S9(5) COMP-3.
             10 SV-MIX-BINARY          PIC S9(5) COMP.
