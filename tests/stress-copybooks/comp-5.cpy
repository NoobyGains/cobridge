      ****************************************************************
      * COMP-5 (NATIVE BINARY) FIELDS
      * TEST: COMMON IBM EXTENSION FOR NATIVE BYTE ORDER
      * NOTE: COMP-5 IS LIKE BINARY BUT USES FULL RANGE
      ****************************************************************
       01 COMP5-TEST-RECORD.
          05 C5-SMALL-INT              PIC 9(4) COMP.
          05 C5-MEDIUM-INT             PIC 9(9) COMP.
          05 C5-LARGE-INT              PIC 9(18) COMP.
          05 C5-SIGNED-SMALL           PIC S9(4) COMP.
          05 C5-SIGNED-MED             PIC S9(9) COMP.
          05 C5-SIGNED-LARGE           PIC S9(18) COMP.
          05 C5-BINARY-SMALL           PIC 9(4) BINARY.
          05 C5-BINARY-MED             PIC 9(9) BINARY.
          05 C5-BINARY-LARGE           PIC 9(18) BINARY.
          05 C5-PACKED-SMALL           PIC 9(3) COMP-3.
          05 C5-PACKED-MED             PIC S9(7)V99 COMP-3.
          05 C5-PACKED-LARGE           PIC S9(15)V99 COMP-3.
          05 C5-DISPLAY-NUM            PIC 9(8).
          05 C5-DISPLAY-SIGNED         PIC S9(8).
          05 C5-DISPLAY-DEC            PIC 9(5)V99.
          05 C5-FLOAT-SINGLE           COMP-1.
          05 C5-FLOAT-DOUBLE           COMP-2.
          05 C5-ALPHA-FIELD            PIC X(20).
          05 C5-GROUP-ITEM.
             10 C5-GROUP-NUM           PIC 9(4) COMP.
             10 C5-GROUP-TEXT          PIC X(10).
             10 C5-GROUP-AMT           PIC S9(7)V99 COMP-3.
