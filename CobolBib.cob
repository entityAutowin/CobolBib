       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOLBIB.
       AUTHOR.     UNSERE-NAMEN.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-SELECTION PIC 9(1).
           01 PR-INPUT-RECORD     PIC X(250).
           01 PR-BUCH-NR          PIC 9(04).
           01 LK-SUCH-ID          PIC X(30) VALUE ZERO.
           COPY library.

       PROCEDURE DIVISION.
       0000-MAIN-PROCEDURE.
           CALL "ProcessRecord" USING PR-INPUT-RECORD
                                     PR-BUCH-NR
                                     LK-BUECHER-LISTE.
           PERFORM 1000-USER-MENU.
           PERFORM 2000-SELECTION-EVALUATION.

       1000-USER-MENU.
           DISPLAY "1 - Neues Buch eingeben".
           DISPLAY "2 - Alle Bücher anzeigen".
           DISPLAY "3 - Buch suchen".
           DISPLAY "4 - Buch löschen".
           DISPLAY "5 - Anzahl Bücher".
           DISPLAY "6 - Beenden".
           DISPLAY "Auswahl: " WITH NO ADVANCING.
           ACCEPT WS-SELECTION.

       2000-SELECTION-EVALUATION.
           EVALUATE WS-SELECTION
               WHEN 1 CONTINUE
               WHEN 2 CALL "PrintMod" USING LK-BUECHER-LISTE
                                                       LK-SUCH-ID
               WHEN 3 CONTINUE
               WHEN 4 CONTINUE
               WHEN 5 PERFORM 2500-ANZAHL-BUECHER
               WHEN 6 CONTINUE
           END-EVALUATE
           STOP RUN.

       2100-ALLE-BUECHER-ANZEIGEN.
           PERFORM VARYING BUCH-IDX FROM 1 BY 1
                   UNTIL BUCH-IDX > LK-BUECHER-ANZAHL
               DISPLAY "--- " LK-BUCH-ID(BUCH-IDX) " ---"
               DISPLAY "Titel    : " LK-BUCH-TITEL(BUCH-IDX)
               DISPLAY "Kategorie: " LK-BUCH-KATEGORIE(BUCH-IDX)
               DISPLAY "Verlag   : " LK-BUCH-VERLAG(BUCH-IDX)
               PERFORM VARYING AUTOR-IDX FROM 1 BY 1
                       UNTIL AUTOR-IDX > LK-AUTOREN-ANZAHL(BUCH-IDX)
                   DISPLAY "Autor    : "
                       LK-AUTOR-NAME(BUCH-IDX AUTOR-IDX)
               END-PERFORM
           END-PERFORM.

       2500-ANZAHL-BUECHER.
           DISPLAY "Anzahl Buecher: " LK-BUECHER-ANZAHL.
