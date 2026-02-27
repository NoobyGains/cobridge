      ****************************************************************
      * LARGE RECORD - 100+ FIELDS, 2000+ BYTES
      * SYSTEM: CENSUS BUREAU DATA COLLECTION
      ****************************************************************
       01 CENSUS-HOUSEHOLD-RECORD.
          05 CH-RECORD-HEADER.
             10 CH-SURVEY-ID           PIC X(12).
             10 CH-FORM-TYPE           PIC X(4).
             10 CH-COLLECTION-DATE     PIC 9(8).
             10 CH-ENUMERATOR-ID       PIC 9(6).
             10 CH-REGION-CODE         PIC 9(2).
             10 CH-DISTRICT-CODE       PIC 9(4).
             10 CH-TRACT-CODE          PIC 9(6).
             10 CH-BLOCK-CODE          PIC 9(4).
          05 CH-LOCATION.
             10 CH-STREET-NUMBER       PIC X(10).
             10 CH-STREET-NAME         PIC X(40).
             10 CH-APARTMENT           PIC X(10).
             10 CH-CITY                PIC X(30).
             10 CH-COUNTY              PIC X(30).
             10 CH-STATE               PIC XX.
             10 CH-ZIP-CODE            PIC 9(5).
             10 CH-ZIP-PLUS4           PIC 9(4).
             10 CH-LATITUDE            PIC S9(3)V9(6) COMP-3.
             10 CH-LONGITUDE           PIC S9(3)V9(6) COMP-3.
          05 CH-HOUSEHOLD-INFO.
             10 CH-HOUSING-TYPE        PIC XX.
             10 CH-TENURE              PIC X.
             10 CH-YEAR-BUILT          PIC 9(4).
             10 CH-NUM-ROOMS           PIC 99.
             10 CH-NUM-BEDROOMS        PIC 99.
             10 CH-HEATING-FUEL        PIC X.
             10 CH-PLUMBING            PIC X.
             10 CH-KITCHEN             PIC X.
             10 CH-TELEPHONE           PIC X.
             10 CH-VEHICLES            PIC 9.
             10 CH-PROPERTY-VALUE      PIC S9(9)V99 COMP-3.
             10 CH-RENT-AMOUNT         PIC S9(5)V99 COMP-3.
             10 CH-INCOME-TOTAL        PIC S9(9)V99 COMP-3.
          05 CH-PERSON OCCURS 10 TIMES.
             10 CH-PERSON-NUMBER       PIC 99.
             10 CH-RELATIONSHIP        PIC XX.
             10 CH-LAST-NAME           PIC X(30).
             10 CH-FIRST-NAME          PIC X(20).
             10 CH-MIDDLE-INITIAL      PIC X.
             10 CH-SUFFIX              PIC X(4).
             10 CH-SEX                 PIC X.
             10 CH-AGE                 PIC 9(3).
             10 CH-DATE-OF-BIRTH       PIC 9(8).
             10 CH-MARITAL-STATUS      PIC X.
             10 CH-RACE-CODE1          PIC XX.
             10 CH-RACE-CODE2          PIC XX.
             10 CH-HISPANIC-ORIGIN     PIC X.
             10 CH-CITIZENSHIP         PIC X.
             10 CH-YEAR-OF-ENTRY       PIC 9(4).
             10 CH-EDUCATION-LEVEL     PIC XX.
             10 CH-SCHOOL-ENROLL       PIC X.
             10 CH-EMPLOYMENT-STATUS   PIC X.
             10 CH-OCCUPATION-CODE     PIC X(6).
             10 CH-INDUSTRY-CODE       PIC X(6).
             10 CH-HOURS-WORKED        PIC 99.
             10 CH-WEEKS-WORKED        PIC 99.
             10 CH-WAGE-INCOME         PIC S9(7)V99 COMP-3.
             10 CH-SELF-EMP-INCOME     PIC S9(7)V99 COMP-3.
             10 CH-INTEREST-INCOME     PIC S9(7)V99 COMP-3.
             10 CH-SS-INCOME           PIC S9(5)V99 COMP-3.
             10 CH-RETIREMENT-INCOME   PIC S9(7)V99 COMP-3.
             10 CH-OTHER-INCOME        PIC S9(7)V99 COMP-3.
             10 CH-TOTAL-INCOME        PIC S9(9)V99 COMP-3.
             10 CH-DISABILITY-STATUS   PIC X.
             10 CH-VETERAN-STATUS      PIC X.
             10 CH-HEALTH-INSURANCE    PIC X.
             10 CH-MIGRATION-STATE     PIC XX.
             10 CH-MIGRATION-COUNTY    PIC X(5).
             10 CH-LANGUAGE-HOME       PIC XX.
             10 CH-ENGLISH-ABILITY     PIC X.
             10 CH-ANCESTRY-CODE1      PIC X(3).
             10 CH-ANCESTRY-CODE2      PIC X(3).
          05 CH-RECORD-TRAILER.
             10 CH-PERSON-COUNT        PIC 99.
             10 CH-CHECKSUM            PIC 9(10).
             10 CH-FILLER-TRAIL        PIC X(20).
