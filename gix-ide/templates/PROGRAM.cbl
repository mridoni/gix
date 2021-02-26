       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. ${PGID}.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
       
       FILE-CONTROL.
       
       DATA DIVISION.
       
       FILE SECTION.
       
       WORKING-STORAGE SECTION.
       
       LINKAGE SECTION.
            
            01  PAR1      PIC X(255).
            01  PAR2      PIC 9(12).
      
       PROCEDURE DIVISION.
           DISPLAY 'HELLO WORLD'.
