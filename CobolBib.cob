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
           01 WS-SEARCH-TYPE      PIC 9 VALUE 0.
           01 WS-SEARCH-TERM      PIC X(120) VALUE SPACES.
           COPY library.

       PROCEDURE DIVISION.
       0000-MAIN-PROCEDURE.
           CALL "ProcessRecord" USING PR-INPUT-RECORD
                                     PR-BUCH-NR
                                     LK-BUECHER-LISTE.
           PERFORM UNTIL WS-SELECTION = 6
               PERFORM 1000-USER-MENU
               PERFORM 2000-SELECTION-EVALUATION
           END-PERFORM.
           STOP RUN.

       1000-USER-MENU.
           DISPLAY " "
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
               WHEN 1 CALL "CreateMod" USING LK-BUECHER-LISTE
               WHEN 2 MOVE 0 TO WS-SEARCH-TYPE
                      CALL "PrintMod" USING LK-BUECHER-LISTE
                                            WS-SEARCH-TYPE
                                            WS-SEARCH-TERM
               WHEN 3 CALL "SearchMod" USING LK-BUECHER-LISTE
               WHEN 4 CALL "DeleteMod" USING LK-BUECHER-LISTE
               WHEN 5 PERFORM 2500-ANZAHL-BUECHER
               WHEN 6 CONTINUE
           END-EVALUATE.

       2500-ANZAHL-BUECHER.
           DISPLAY "Anzahl Buecher: " LK-BUECHER-ANZAHL.
