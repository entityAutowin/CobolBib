       IDENTIFICATION DIVISION.
       PROGRAM-ID. PrintMod.
       AUTHOR.     UNSERE-NAMEN.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-HEADER-1.
          05 FILLER       PIC X(60) VALUE 
          "============================================================".
       01 WS-HEADER-2.
          05 FILLER       PIC X(31)  VALUE "ID                            ".
          05 FILLER       PIC X(25) VALUE "TITEL                    ".
          05 FILLER       PIC X(15) VALUE "KATEGORIE      ".
          05 FILLER       PIC X(13) VALUE "VERLAG       ".
       01 WS-HEADER-3.
          05 FILLER       PIC X(60) VALUE 
          "------------------------------------------------------------".

       01 WS-BUCH-ZEILE.
          05 WS-DET-ID        PIC X(30).
          05 FILLER           PIC X(1)  VALUE " ".
          05 WS-DET-TITEL     PIC X(24).
          05 FILLER           PIC X(1)  VALUE " ".
          05 WS-DET-KAT       PIC X(14).
          05 FILLER           PIC X(1)  VALUE " ".
          05 WS-DET-VERLAG    PIC X(12).

       01 WS-AUTOR-ZEILE.
          05 FILLER           PIC X(10) VALUE "  Autoren:".
          05 WS-DET-AUTOR     PIC X(40).

       01 WS-MATCH-FOUND           PIC X VALUE 'N'.
       01 WS-I                     PIC 9(4).
       01 WS-J                     PIC 9(2).

       LINKAGE SECTION.
           COPY library.
           
       01 LK-SEARCH-TYPE           PIC 9.
       01 LK-SEARCH-TERM           PIC X(120).

       PROCEDURE DIVISION USING LK-BUECHER-LISTE LK-SEARCH-TYPE 
                                LK-SEARCH-TERM.
       MAIN-LOGIC.
           IF LK-BUECHER-ANZAHL = 0
               DISPLAY "Keine Buecher im System vorhanden."
               GOBACK
           END-IF.

           DISPLAY WS-HEADER-1
           DISPLAY WS-HEADER-2
           DISPLAY WS-HEADER-3

           PERFORM VARYING WS-I FROM 1 BY 1 
                     UNTIL WS-I > LK-BUECHER-ANZAHL
               
               MOVE 'N' TO WS-MATCH-FOUND
               EVALUATE LK-SEARCH-TYPE
                   WHEN
                       MOVE 'Y' TO WS-MATCH-FOUND
                   WHEN
                       IF LK-BUCH-ID(WS-I) = LK-SEARCH-TERM(1:25)
                           MOVE 'Y' TO WS-MATCH-FOUND
                       END-IF
                   WHEN
                       IF LK-BUCH-TITEL(WS-I) = LK-SEARCH-TERM
                           MOVE 'Y' TO WS-MATCH-FOUND
                       END-IF
                   WHEN
                       IF LK-BUCH-KATEGORIE(WS-I) = 
                          LK-SEARCH-TERM(1:20)
                           MOVE 'Y' TO WS-MATCH-FOUND
                       END-IF
                   WHEN
                       PERFORM VARYING WS-J FROM 1 BY 1
                           UNTIL WS-J > LK-AUTOREN-ANZAHL(WS-I)
                              OR WS-MATCH-FOUND = 'Y'
                           IF LK-AUTOR-NAME(WS-I, WS-J) = 
                              LK-SEARCH-TERM(1:40)
                               MOVE 'Y' TO WS-MATCH-FOUND
                           END-IF
                       END-PERFORM
               END-EVALUATE

               IF WS-MATCH-FOUND = 'Y'
                   MOVE LK-BUCH-ID(WS-I)        TO WS-DET-ID
                   MOVE LK-BUCH-TITEL(WS-I)     TO WS-DET-TITEL
                   MOVE LK-BUCH-KATEGORIE(WS-I) TO WS-DET-KAT
                   MOVE LK-BUCH-VERLAG(WS-I)    TO WS-DET-VERLAG
                   
                   DISPLAY WS-BUCH-ZEILE
                   
                   IF LK-AUTOREN-ANZAHL(WS-I) > 0
                       PERFORM VARYING WS-J FROM 1 BY 1
                                 UNTIL WS-J > 
                                       LK-AUTOREN-ANZAHL(WS-I)
                           
                           MOVE LK-AUTOR-NAME(WS-I, WS-J) 
                             TO WS-DET-AUTOR
                           DISPLAY WS-AUTOR-ZEILE
                           
                       END-PERFORM
                   ELSE
                       DISPLAY "  (Keine Autoren hinterlegt)"
                   END-IF
                   
                   DISPLAY 
                   "------------------------------------------------------------"
               
               END-IF
               
           END-PERFORM.

           GOBACK.
