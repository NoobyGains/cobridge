       IDENTIFICATION DIVISION.
       PROGRAM-ID. BALCHECK.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-DATE    PIC 9(8).

       LINKAGE SECTION.
       01  LS-ACCOUNT-NUM     PIC X(12).
       01  LS-BALANCE         PIC S9(13)V99 COMP-3.
       01  LS-AVAILABLE       PIC S9(13)V99 COMP-3.
       01  LS-RETURN-CODE     PIC 9(2).

       PROCEDURE DIVISION USING LS-ACCOUNT-NUM
                                LS-BALANCE
                                LS-AVAILABLE
                                LS-RETURN-CODE.

       MAIN-LOGIC.
           MOVE 52750.00 TO LS-BALANCE
           MOVE 48500.00 TO LS-AVAILABLE
           MOVE 0 TO LS-RETURN-CODE
           GOBACK.
