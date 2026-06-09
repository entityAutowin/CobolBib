       IDENTIFICATION DIVISION.
       PROGRAM-ID. DeleteMod.

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
       01  WS-SEARCH-ID         PIC X(25).
       01  WS-FOUND             PIC X          VALUE 'N'.
       01  I                    PIC 9(04).
       01  J                    PIC 9(04).
       01  K                    PIC 9(02).
       01  WS-AUTOREN-STR       PIC X(100).
       01  WS-TEMP-AUTOR        PIC X(40).
       01  WS-RECORD-LEN        PIC 9(03).

       LINKAGE SECTION.
           COPY library.

       PROCEDURE DIVISION USING LK-BUECHER-LISTE.
       0000-MAIN.
           DISPLAY "ID (ISBN) des zu loeschenden Buches: " 
                   WITH NO ADVANCING.
           ACCEPT WS-SEARCH-ID.

           MOVE 'N' TO WS-FOUND.
           PERFORM VARYING I FROM 1 BY 1 
                   UNTIL I > LK-BUECHER-ANZAHL OR WS-FOUND = 'Y'
               IF LK-BUCH-ID(I) = WS-SEARCH-ID
                   MOVE 'Y' TO WS-FOUND
                   PERFORM 1000-DELETE-RECORD
               END-IF
           END-PERFORM.

           IF WS-FOUND = 'N'
               DISPLAY "Buch mit ID " WS-SEARCH-ID " nicht gefunden."
           ELSE
               PERFORM 2000-SAVE-TO-FILE
               DISPLAY "Buch erfolgreich geloescht."
           END-IF.

           EXIT PROGRAM.

       1000-DELETE-RECORD.
           PERFORM VARYING J FROM I BY 1 
                   UNTIL J >= LK-BUECHER-ANZAHL
               MOVE LK-BUCH-EINTRAG(J + 1) TO LK-BUCH-EINTRAG(J)
           END-PERFORM.
           SUBTRACT 1 FROM LK-BUECHER-ANZAHL.

       2000-SAVE-TO-FILE.
           OPEN OUTPUT OUTPUT-FILE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LK-BUECHER-ANZAHL
               INITIALIZE WS-AUTOREN-STR
               INITIALIZE WS-OUTPUT-RECORD
               
               PERFORM VARYING K FROM 1 BY 1 
                       UNTIL K > LK-AUTOREN-ANZAHL(I)
                   MOVE LK-AUTOR-NAME(I K) TO WS-TEMP-AUTOR
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
               END-PERFORM

               STRING LK-BUCH-ID(I)       DELIMITED BY "  "
                      ", "                DELIMITED BY SIZE
                      LK-BUCH-TITEL(I)    DELIMITED BY "  "
                      ", "                DELIMITED BY SIZE
                      WS-AUTOREN-STR      DELIMITED BY "  "
                      ", "                DELIMITED BY SIZE
                      LK-BUCH-KATEGORIE(I) DELIMITED BY "  "
                      ", "                DELIMITED BY SIZE
                      LK-BUCH-VERLAG(I)    DELIMITED BY "  "
                      INTO WS-OUTPUT-RECORD
               END-STRING
               
               WRITE WS-OUTPUT-RECORD
           END-PERFORM.
           CLOSE OUTPUT-FILE.
