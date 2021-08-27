       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. ${PGID}.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
       
       FILE-CONTROL.
       
       DATA DIVISION.
       
       FILE SECTION.
       
       WORKING-STORAGE SECTION.

            01  AA      PIC X(255).
            01  BB      PIC X(255).
            
       LINKAGE SECTION.
            
         COPY IO${PGID}.
            
       PROCEDURE DIVISION
          USING PAR-IN, PAR-OUT.
          
           DISPLAY 'GOT CALL FROM CLIENT'.
           
           MOVE OPCODE-IN TO OPCODE-OUT.
           MOVE FUNCTION CURRENT-DATE TO DATA-OUT-1.

           