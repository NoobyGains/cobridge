      ****************************************************************
      * PIC SHORTHAND - CLAUSES WITHOUT PARENTHESES
      * TEST: PIC XXXXX INSTEAD OF PIC X(5)
      ****************************************************************
       01 PIC-SHORTHAND-RECORD.
          05 PS-SHORT-ALPHA            PIC XXXXX.
          05 PS-SHORT-NUM              PIC 99999.
          05 PS-SHORT-SIGNED           PIC S999V99.
          05 PS-LONG-ALPHA             PIC XXXXXXXXXX.
          05 PS-LONG-NUM               PIC 9999999999.
          05 PS-MIXED-SHORT.
             10 PS-MIX-XX              PIC XX.
             10 PS-MIX-999             PIC 999.
             10 PS-MIX-S99V9           PIC S99V9.
          05 PS-WITH-PARENS.
             10 PS-PAR-X10             PIC X(10).
             10 PS-PAR-95              PIC 9(5).
             10 PS-PAR-S7V2            PIC S9(7)V9(2).
          05 PS-SINGLE-CHARS.
             10 PS-ONE-X               PIC X.
             10 PS-ONE-9               PIC 9.
             10 PS-ONE-A               PIC A.
