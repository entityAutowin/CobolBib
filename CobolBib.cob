       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOLBIB.
       AUTHOR.     UNSERE-NAMEN.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-SELECTION PIC 9(1).
           01  PR-INPUT-RECORD     PIC X(250).
           01  PR-BUCH-NR          PIC 9(04).
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
               WHEN 1
               WHEN 2
               WHEN 3
               WHEN 4
               WHEN 5
               WHEN 6
           END-EVALUATE
           STOP RUN.
