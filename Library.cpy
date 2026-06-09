       01  LK-BUECHER-LISTE.
       
      *>   Zähler für die aktuelle Anzahl der übergebenen Bücher
           05  LK-BUECHER-ANZAHL        PIC 9(04).

      *>   Die Haupttabelle für max. 500 Bücher
           05  LK-BUCH-EINTRAG          OCCURS 1 TO 500 TIMES
                                        DEPENDING ON LK-BUECHER-ANZAHL
                                        INDEXED BY BUCH-IDX.
               10  LK-BUCH-ID           PIC X(25).
               10  LK-BUCH-TITEL        PIC X(50).
               10  LK-BUCH-KATEGORIE    PIC X(20).
               10  LK-BUCH-VERLAG       PIC X(30).

      *>       Untertabelle: Feste Definition von max. 5 Autoren.
               10  LK-AUTOREN-ANZAHL    PIC 9(02).
               10  LK-AUTOR-EINTRAG     OCCURS 5 TIMES
                                        INDEXED BY AUTOR-IDX.
                   15  LK-AUTOR-NAME    PIC X(40).
                   