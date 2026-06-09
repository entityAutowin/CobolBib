IDENTIFICATION DIVISION.
       PROGRAM-ID. BUECHER-PRINT-MODUL.
       AUTHOR.     UNSERE-NAMEN.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-HEADER-1.
          05 FILLER       PIC X(60) VALUE 
          "============================================================".
       01 WS-HEADER-2.
          05 FILLER       PIC X(7)  VALUE "ID     ".
          05 FILLER       PIC X(25) VALUE "TITEL                    ".
          05 FILLER       PIC X(15) VALUE "KATEGORIE      ".
          05 FILLER       PIC X(13) VALUE "VERLAG       ".
       01 WS-HEADER-3.
          05 FILLER       PIC X(60) VALUE 
          "------------------------------------------------------------".

       01 WS-BUCH-ZEILE.
          05 WS-DET-ID        PIC 9(06).
          05 FILLER           PIC X(1)  VALUE " ".
          05 WS-DET-TITEL     PIC X(24).
          05 FILLER           PIC X(1)  VALUE " ".
          05 WS-DET-KAT       PIC X(14).
          05 FILLER           PIC X(1)  VALUE " ".
          05 WS-DET-VERLAG    PIC X(12).

       01 WS-AUTOR-ZEILE.
          05 FILLER           PIC X(10) VALUE "  Autoren:".
          05 WS-DET-AUTOR     PIC X(40).

       LINKAGE SECTION.
           COPY Library
           
       01 LK-SUCH-ID               PIC X(30).

       PROCEDURE DIVISION USING LK-BUECHER-LISTE LK-SUCH-ID.
       MAIN-LOGIC.
           IF LK-BUECHER-ANZAHL = 0
               DISPLAY "Keine Buecher im System vorhanden."
               GOBACK
           END-IF.

           DISPLAY WS-HEADER-1
           DISPLAY WS-HEADER-2
           DISPLAY WS-HEADER-3

           PERFORM VARYING BUCH-IDX FROM 1 BY 1 
                     UNTIL BUCH-IDX > LK-BUECHER-ANZAHL
               
               IF LK-SUCH-ID = ZERO OR LK-BUCH-ID(BUCH-IDX) = LK-SUCH-ID
               
                   MOVE LK-BUCH-ID(BUCH-IDX)        TO WS-DET-ID
                   MOVE LK-BUCH-TITEL(BUCH-IDX)     TO WS-DET-TITEL
                   MOVE LK-BUCH-KATEGORIE(BUCH-IDX) TO WS-DET-KAT
                   MOVE LK-BUCH-VERLAG(BUCH-IDX)    TO WS-DET-VERLAG
                   
                   DISPLAY WS-BUCH-ZEILE
                   
                   IF LK-AUTOREN-ANZAHL(BUCH-IDX) > 0
                       PERFORM VARYING AUTOR-IDX FROM 1 BY 1
                                 UNTIL AUTOR-IDX > 
                                       LK-AUTOREN-ANZAHL(BUCH-IDX)
                           
                           MOVE LK-AUTOR-NAME(BUCH-IDX, AUTOR-IDX) 
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