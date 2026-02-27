      ****************************************************************
      * FILLER-HEAVY RECORD - LEGACY FIXED-LENGTH FORMAT
      * SYSTEM: MAINFRAME BATCH  COPYBOOK: FLRHEAVY
      * NOTE: EXTENSIVE USE OF FILLER FOR BYTE ALIGNMENT
      ****************************************************************
       01 FILLER-HEAVY-RECORD.
          05 FH-RECORD-TYPE            PIC XX.
          05 FILLER                    PIC X(3).
          05 FH-ACCOUNT-NO             PIC 9(10).
          05 FILLER                    PIC X(5).
          05 FH-NAME                   PIC X(30).
          05 FILLER                    PIC X(2).
          05 FH-ADDRESS-LINE1          PIC X(30).
          05 FILLER                    PIC X(2).
          05 FH-ADDRESS-LINE2          PIC X(30).
          05 FILLER                    PIC X(2).
          05 FH-CITY                   PIC X(20).
          05 FILLER                    PIC X.
          05 FH-STATE                  PIC XX.
          05 FILLER                    PIC X.
          05 FH-ZIP                    PIC 9(5).
          05 FILLER                    PIC X(4).
          05 FH-BALANCE                PIC S9(9)V99 COMP-3.
          05 FILLER                    PIC X(3).
          05 FH-STATUS                 PIC X.
          05 FILLER                    PIC X(10).
          05 FH-OPEN-DATE              PIC 9(8).
          05 FILLER                    PIC X(2).
          05 FH-CLOSE-DATE             PIC 9(8).
          05 FILLER                    PIC X(20).
          05 FH-LAST-ACTIVITY          PIC 9(8).
          05 FILLER                    PIC X(5).
          05 FH-RESERVED               PIC X(50).
          05 FILLER                    PIC X(27).
