       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTLKUP.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-STATUS          PIC X(2).

       LINKAGE SECTION.
       01  LS-CUST-ID         PIC 9(10).
       01  LS-CUST-NAME       PIC X(60).
       01  LS-CUST-BALANCE    PIC S9(13)V99 COMP-3.
       01  LS-RETURN-CODE     PIC 9(2).

       PROCEDURE DIVISION USING LS-CUST-ID
                                LS-CUST-NAME
                                LS-CUST-BALANCE
                                LS-RETURN-CODE.

       MAIN-LOGIC.
           MOVE "John Doe" TO LS-CUST-NAME
           MOVE 15234.50 TO LS-CUST-BALANCE
           MOVE 0 TO LS-RETURN-CODE
           GOBACK.
