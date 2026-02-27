      ****************************************************************
      * VALUE CLAUSES - EXTENSIVE VALUE TESTING
      * TEST: VARIOUS VALUE CLAUSE FORMATS
      ****************************************************************
       01 VALUE-CLAUSES-RECORD.
          05 VC-STATUS-CODE            PIC X(2) VALUE 'OK'.
          05 VC-ERROR-COUNT            PIC 9(4) VALUE 0.
          05 VC-DESCRIPTION            PIC X(20) VALUE SPACES.
          05 VC-AMOUNT-FIELD           PIC 9(8)V99 VALUE ZEROS.
          05 VC-FILL-CHAR              PIC X(10) VALUE '*'.
          05 VC-CONSTANT               PIC X(5) VALUE 'HELLO'.
          05 VC-NUMERIC-INIT           PIC 9(6) VALUE 100.
          05 VC-SIGNED-INIT            PIC S9(5)V99 COMP-3
                                       VALUE 0.
          05 VC-FLAG                   PIC X VALUE 'Y'.
             88 VC-YES                 VALUE 'Y'.
             88 VC-NO                  VALUE 'N'.
          05 VC-RECORD-DATA.
             10 VC-DATA-NAME           PIC X(25).
             10 VC-DATA-VALUE          PIC 9(8).
             10 VC-DATA-STATUS         PIC XX VALUE 'AC'.
          05 VC-COUNTER                PIC 9(3) COMP VALUE 0.
          05 VC-LONG-VALUE             PIC X(30)
                                       VALUE 'DEFAULT-VALUE-HERE'.
          05 VC-BINARY-INIT            PIC 9(4) BINARY VALUE 0.
          05 VC-PACKED-INIT            PIC S9(5)V99 COMP-3 VALUE 0.
