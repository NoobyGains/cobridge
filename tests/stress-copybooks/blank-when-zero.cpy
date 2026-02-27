      ****************************************************************
      * BLANK WHEN ZERO AND JUSTIFIED RIGHT
      * TEST: ADDITIONAL FIELD ATTRIBUTES
      * NOTE: PARSER SHOULD HANDLE THESE WITHOUT CRASHING
      ****************************************************************
       01 BLANK-ZERO-RECORD.
          05 BZ-REPORT-LINE.
             10 BZ-ITEM-NAME           PIC X(20).
             10 BZ-QUANTITY            PIC Z(5)9.
             10 BZ-UNIT-PRICE          PIC Z(5)9.99.
             10 BZ-TOTAL-PRICE         PIC Z(7)9.99.
             10 BZ-TAX-AMOUNT          PIC Z(5)9.99.
          05 BZ-SUMMARY.
             10 BZ-SUBTOTAL            PIC Z(9)9.99.
             10 BZ-TAX-TOTAL           PIC Z(7)9.99.
             10 BZ-GRAND-TOTAL         PIC Z(9)9.99.
             10 BZ-DISCOUNT            PIC Z(5)9.99.
             10 BZ-NET-DUE             PIC Z(9)9.99.
          05 BZ-HEADER-INFO.
             10 BZ-INVOICE-NO          PIC 9(8).
             10 BZ-INVOICE-DATE        PIC 99/99/9999.
             10 BZ-DUE-DATE            PIC 99/99/9999.
             10 BZ-CUSTOMER-ID         PIC 9(10).
             10 BZ-PO-NUMBER           PIC X(15).
