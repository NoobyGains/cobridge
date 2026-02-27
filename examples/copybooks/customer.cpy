      *================================================================*
      * CUSTOMER MASTER RECORD - COBridge Example
      *================================================================*
       01  CUSTOMER-RECORD.
           05  CUST-ID                    PIC 9(10).
           05  CUST-PERSONAL-INFO.
               10  CUST-FIRST-NAME       PIC X(30).
               10  CUST-LAST-NAME        PIC X(30).
               10  CUST-DATE-OF-BIRTH.
                   15  CUST-DOB-YEAR     PIC 9(4).
                   15  CUST-DOB-MONTH    PIC 9(2).
                   15  CUST-DOB-DAY      PIC 9(2).
               10  CUST-SSN              PIC X(11).
           05  CUST-CONTACT-INFO.
               10  CUST-EMAIL            PIC X(50).
               10  CUST-PHONE            PIC X(15).
               10  CUST-ADDRESS.
                   15  CUST-STREET       PIC X(40).
                   15  CUST-CITY         PIC X(25).
                   15  CUST-STATE        PIC X(2).
                   15  CUST-ZIP          PIC X(10).
           05  CUST-ACCOUNT-SUMMARY.
               10  CUST-NUM-ACCOUNTS     PIC 9(3) COMP-3.
               10  CUST-TOTAL-BALANCE    PIC S9(13)V99 COMP-3.
               10  CUST-CREDIT-SCORE     PIC 9(3).
               10  CUST-RISK-RATING      PIC X(1).
               10  CUST-SINCE-DATE       PIC 9(8).
           05  CUST-STATUS               PIC X(1).
               88  CUST-ACTIVE           VALUE 'A'.
               88  CUST-INACTIVE         VALUE 'I'.
               88  CUST-SUSPENDED        VALUE 'S'.
