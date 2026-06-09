       IDENTIFICATION DIVISION.
       PROGRAM-ID. SearchMod.
       AUTHOR.     UNSERE-NAMEN.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-SEARCH-CHOICE     PIC 9.
       01  WS-SEARCH-TERM       PIC X(120).
       01  WS-PRINT-TYPE        PIC 9.

       LINKAGE SECTION.
           COPY library.

       PROCEDURE DIVISION USING LK-BUECHER-LISTE.
       0000-MAIN.
           DISPLAY " "
           DISPLAY "--- Buch suchen ---"
           DISPLAY "Suchen nach:"
           DISPLAY "1 - Titel"
           DISPLAY "2 - Kategorie"
           DISPLAY "3 - Autor"
           DISPLAY "Auswahl: " WITH NO ADVANCING.
           ACCEPT WS-SEARCH-CHOICE.
           
           DISPLAY "Suchbegriff: " WITH NO ADVANCING.
           ACCEPT WS-SEARCH-TERM.
           
           EVALUATE WS-SEARCH-CHOICE
               WHEN 1 MOVE 2 TO WS-PRINT-TYPE
               WHEN 2 MOVE 3 TO WS-PRINT-TYPE
               WHEN 3 MOVE 4 TO WS-PRINT-TYPE
               WHEN OTHER 
                   DISPLAY "Ungueltige Auswahl."
                   EXIT PROGRAM
           END-EVALUATE.

           CALL "PrintMod" USING LK-BUECHER-LISTE
                                 WS-PRINT-TYPE
                                 WS-SEARCH-TERM.

           EXIT PROGRAM.
