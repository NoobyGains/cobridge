      ****************************************************************
      * DEEPLY NESTED RECORD - 7 LEVELS OF NESTING
      * TEST: PARSER DEPTH HANDLING
      ****************************************************************
       01 DEEP-NESTED-RECORD.
          05 DN-LEVEL-05.
             10 DN-LEVEL-10.
                15 DN-LEVEL-15.
                   20 DN-LEVEL-20.
                      25 DN-LEVEL-25.
                         30 DN-LEVEL-30.
                            35 DN-DEEPEST-FIELD PIC X(10).
                            35 DN-DEEPEST-NUM   PIC 9(5) COMP-3.
                         30 DN-DEEP-CODE        PIC X(3).
                      25 DN-MID-DEEP-AMT        PIC S9(7)V99 COMP-3.
                   20 DN-MID-NAME               PIC X(20).
                   20 DN-MID-DATE               PIC 9(8).
                15 DN-UPPER-FLAG                 PIC X.
                15 DN-UPPER-COUNT                PIC 9(4) COMP.
             10 DN-BRANCH-B.
                15 DN-BRANCH-B-FIELD1  PIC X(15).
                15 DN-BRANCH-B-DATA.
                   20 DN-BB-SUB1       PIC 9(8).
                   20 DN-BB-SUB2.
                      25 DN-BB-INNER1  PIC X(5).
                      25 DN-BB-INNER2  PIC S9(5)V99 COMP-3.
          05 DN-FLAT-FIELD              PIC X(30).
