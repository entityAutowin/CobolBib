       IDENTIFICATION DIVISION.
       PROGRAM-ID. ProcessRecord.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-AUTOR-NR         PIC 9(02) VALUE 0.
       01  WS-AUTOR-IDX        PIC 9(02) VALUE 0.

       01  WS-FELDER.
           05  WS-ISBN          PIC X(25).
           05  WS-TITEL         PIC X(120).
           05  WS-AUTOREN-STR   PIC X(100).
           05  WS-KATEGORIE     PIC X(20).
           05  WS-VERLAG        PIC X(30).

       01  WS-AUTOREN.
           05  WS-AUTOR         PIC X(40) OCCURS 5 TIMES.

       LINKAGE SECTION.
       01  PR-INPUT-RECORD     PIC X(250).
       01  PR-BUCH-NR          PIC 9(04).
           COPY library.

       PROCEDURE DIVISION USING PR-INPUT-RECORD
                                PR-BUCH-NR
                                LK-BUECHER-LISTE.
           ADD 1 TO PR-BUCH-NR
           MOVE PR-BUCH-NR TO LK-BUECHER-ANZAHL
           SET BUCH-IDX TO PR-BUCH-NR
           MOVE PR-BUCH-NR TO LK-BUCH-ID(BUCH-IDX)

           MOVE SPACES TO WS-FELDER

           UNSTRING PR-INPUT-RECORD DELIMITED BY ', '
               INTO WS-ISBN
                    WS-TITEL
                    WS-AUTOREN-STR
                    WS-KATEGORIE
                    WS-VERLAG
           END-UNSTRING

           MOVE WS-TITEL     TO LK-BUCH-TITEL(BUCH-IDX)
           MOVE WS-KATEGORIE TO LK-BUCH-KATEGORIE(BUCH-IDX)
           MOVE WS-VERLAG    TO LK-BUCH-VERLAG(BUCH-IDX)

           MOVE SPACES TO WS-AUTOREN
           MOVE 0 TO WS-AUTOR-NR

           UNSTRING WS-AUTOREN-STR DELIMITED BY ';'
               INTO WS-AUTOR(1) WS-AUTOR(2)
                    WS-AUTOR(3) WS-AUTOR(4) WS-AUTOR(5)
               TALLYING WS-AUTOR-NR
           END-UNSTRING

           MOVE WS-AUTOR-NR TO LK-AUTOREN-ANZAHL(BUCH-IDX)

           PERFORM VARYING WS-AUTOR-IDX FROM 1 BY 1
                   UNTIL WS-AUTOR-IDX > WS-AUTOR-NR
               SET AUTOR-IDX TO WS-AUTOR-IDX
               MOVE WS-AUTOR(WS-AUTOR-IDX) TO
                   LK-AUTOR-NAME(BUCH-IDX AUTOR-IDX)
           END-PERFORM

           EXIT PROGRAM.
