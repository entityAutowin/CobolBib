       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOLBIB.
       AUTHOR.     UNSERE-NAMEN.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       LINKAGE SECTION.
      *> Copy values from Library CopyBook
           COPY library.

       PROCEDURE DIVISION USING LK-BUECHER-LISTE.
       MAIN-PROCEDURE.
           
           STOP RUN.
           