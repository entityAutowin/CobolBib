       IDENTIFICATION DIVISION.
       PROGRAM-ID. CreateMod.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTPUT-FILE ASSIGN TO 'data/d.b'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  OUTPUT-FILE.
       01  WS-OUTPUT-RECORD     PIC X(250).

       WORKING-STORAGE SECTION.
       01  WS-AUTOREN-STR       PIC X(100).
       01  WS-TEMP-AUTOR        PIC X(40).
       01  K                    PIC 9(02).

       LINKAGE SECTION.
           COPY library.

       PROCEDURE DIVISION USING LK-BUECHER-LISTE.
       0000-MAIN.
           IF LK-BUECHER-ANZAHL >= 500
               DISPLAY "Fehler: Maximale Anzahl an Buechern erreicht."
               EXIT PROGRAM
           END-IF.

           ADD 1 TO LK-BUECHER-ANZAHL.
           SET BUCH-IDX TO LK-BUECHER-ANZAHL.

           DISPLAY "ISBN: " WITH NO ADVANCING.
           ACCEPT LK-BUCH-ID(BUCH-IDX).
           DISPLAY "Titel: " WITH NO ADVANCING.
           ACCEPT LK-BUCH-TITEL(BUCH-IDX).
           DISPLAY "Kategorie: " WITH NO ADVANCING.
           ACCEPT LK-BUCH-KATEGORIE(BUCH-IDX).
           DISPLAY "Verlag: " WITH NO ADVANCING.
           ACCEPT LK-BUCH-VERLAG(BUCH-IDX).
           
           DISPLAY "Anzahl Autoren (1-5): " WITH NO ADVANCING.
           ACCEPT LK-AUTOREN-ANZAHL(BUCH-IDX).
           
           IF LK-AUTOREN-ANZAHL(BUCH-IDX) > 5
               MOVE 5 TO LK-AUTOREN-ANZAHL(BUCH-IDX)
           END-IF.
           IF LK-AUTOREN-ANZAHL(BUCH-IDX) < 1
               MOVE 1 TO LK-AUTOREN-ANZAHL(BUCH-IDX)
           END-IF.

           PERFORM VARYING K FROM 1 BY 1 
                   UNTIL K > LK-AUTOREN-ANZAHL(BUCH-IDX)
               DISPLAY "Autor " K ": " WITH NO ADVANCING
               ACCEPT LK-AUTOR-NAME(BUCH-IDX K)
           END-PERFORM.

           PERFORM 2000-APPEND-TO-FILE.
           DISPLAY "Buch erfolgreich hinzugefuegt.".

           EXIT PROGRAM.

       2000-APPEND-TO-FILE.
           OPEN EXTEND OUTPUT-FILE.
           
           INITIALIZE WS-AUTOREN-STR.
           INITIALIZE WS-OUTPUT-RECORD.
           PERFORM VARYING K FROM 1 BY 1 
                   UNTIL K > LK-AUTOREN-ANZAHL(BUCH-IDX)
               MOVE LK-AUTOR-NAME(BUCH-IDX K) TO WS-TEMP-AUTOR
               IF K = 1
                   STRING WS-TEMP-AUTOR DELIMITED BY "  " 
                          INTO WS-AUTOREN-STR
                   END-STRING
               ELSE
                   STRING WS-AUTOREN-STR DELIMITED BY "  "
                          ";"            DELIMITED BY SIZE
                          WS-TEMP-AUTOR  DELIMITED BY "  "
                          INTO WS-AUTOREN-STR
                   END-STRING
               END-IF
           END-PERFORM.

           STRING LK-BUCH-ID(BUCH-IDX)       DELIMITED BY "  "
                  ", "                       DELIMITED BY SIZE
                  LK-BUCH-TITEL(BUCH-IDX)    DELIMITED BY "  "
                  ", "                       DELIMITED BY SIZE
                  WS-AUTOREN-STR             DELIMITED BY "  "
                  ", "                       DELIMITED BY SIZE
                  LK-BUCH-KATEGORIE(BUCH-IDX) DELIMITED BY "  "
                  ", "                       DELIMITED BY SIZE
                  LK-BUCH-VERLAG(BUCH-IDX)    DELIMITED BY "  "
                  INTO WS-OUTPUT-RECORD
           END-STRING.
           
           WRITE WS-OUTPUT-RECORD.
           CLOSE OUTPUT-FILE.
