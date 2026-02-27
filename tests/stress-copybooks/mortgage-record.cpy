      ****************************************************************
      * MORTGAGE LOAN MASTER RECORD
      * SYSTEM: MTGSYS  COPYBOOK: MTGLOAN
      * CONFORMANCE: MISMO 3.4 DATA MAPPING
      ****************************************************************
       01 MORTGAGE-LOAN-RECORD.
          05 MLR-LOAN-IDENTITY.
             10 MLR-LOAN-NUMBER        PIC X(20).
             10 MLR-SERVICER-ID        PIC 9(10).
             10 MLR-INVESTOR-CODE      PIC X(5).
             10 MLR-LOAN-TYPE          PIC XX.
             10 MLR-LOAN-PURPOSE       PIC XX.
          05 MLR-BORROWER-INFO.
             10 MLR-PRIMARY-BORROWER.
                15 MLR-BORR-LAST-NAME  PIC X(35).
                15 MLR-BORR-FIRST-NAME PIC X(25).
                15 MLR-BORR-MI         PIC X.
                15 MLR-BORR-SSN        PIC 9(9).
                15 MLR-BORR-DOB        PIC 9(8).
                15 MLR-BORR-CREDIT-SCR PIC 9(3).
             10 MLR-PROPERTY-ADDR.
                15 MLR-PROP-STREET1    PIC X(40).
                15 MLR-PROP-STREET2    PIC X(40).
                15 MLR-PROP-CITY       PIC X(30).
                15 MLR-PROP-STATE      PIC XX.
                15 MLR-PROP-ZIP        PIC 9(5).
                15 MLR-PROP-ZIP4       PIC 9(4).
                15 MLR-PROP-COUNTY     PIC X(30).
          05 MLR-LOAN-TERMS.
             10 MLR-ORIG-AMOUNT        PIC S9(9)V99 COMP-3.
             10 MLR-CURRENT-BALANCE    PIC S9(9)V99 COMP-3.
             10 MLR-INTEREST-RATE      PIC 9V9(4) COMP-3.
             10 MLR-ORIG-RATE          PIC 9V9(4) COMP-3.
             10 MLR-TERM-MONTHS        PIC 9(3) COMP.
             10 MLR-REMAIN-MONTHS      PIC 9(3) COMP.
             10 MLR-PAYMENT-AMOUNT     PIC S9(7)V99 COMP-3.
             10 MLR-PI-AMOUNT          PIC S9(7)V99 COMP-3.
             10 MLR-ESCROW-AMOUNT      PIC S9(7)V99 COMP-3.
             10 MLR-LTV-RATIO          PIC 9V9(4) COMP-3.
             10 MLR-APPRAISAL-VALUE    PIC S9(9)V99 COMP-3.
             10 MLR-ORIGINATION-DATE   PIC 9(8).
             10 MLR-MATURITY-DATE      PIC 9(8).
             10 MLR-FIRST-PMT-DATE     PIC 9(8).
             10 MLR-NEXT-PMT-DUE       PIC 9(8).
          05 MLR-ESCROW-DATA.
             10 MLR-TAX-AMOUNT         PIC S9(7)V99 COMP-3.
             10 MLR-INSURANCE-AMT      PIC S9(7)V99 COMP-3.
             10 MLR-PMI-AMOUNT         PIC S9(5)V99 COMP-3.
             10 MLR-ESCROW-BALANCE     PIC S9(7)V99 COMP-3.
             10 MLR-ESCROW-SHORT       PIC S9(7)V99 COMP-3.
          05 MLR-DELINQUENCY.
             10 MLR-DAYS-DELINQUENT    PIC 9(4) COMP.
             10 MLR-TIMES-30-LATE      PIC 9(3) COMP.
             10 MLR-TIMES-60-LATE      PIC 9(3) COMP.
             10 MLR-TIMES-90-LATE      PIC 9(3) COMP.
             10 MLR-LAST-PAID-DATE     PIC 9(8).
             10 MLR-LAST-PAID-AMOUNT   PIC S9(7)V99 COMP-3.
          05 MLR-AMORT-SCHEDULE OCCURS 12 TIMES.
             10 MLR-AMORT-DATE         PIC 9(8).
             10 MLR-AMORT-PRINCIPAL    PIC S9(7)V99 COMP-3.
             10 MLR-AMORT-INTEREST     PIC S9(7)V99 COMP-3.
             10 MLR-AMORT-BALANCE      PIC S9(9)V99 COMP-3.
