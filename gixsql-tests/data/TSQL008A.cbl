       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL008A. 
       
       
       ENVIRONMENT DIVISION. 
       
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. IBM-AT. 
       OBJECT-COMPUTER. IBM-AT. 
       
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
       
       DATA DIVISION.  
       
       FILE SECTION.  
       
       WORKING-STORAGE SECTION. 
       
           01 DATASRC PIC X(64).
           01 DBUSR  PIC X(64).


           01 NUM1 SQL TYPE IS FLOAT(4,2).
           01 NUM2 SQL TYPE IS FLOAT(8).

           01 NUM3 PIC S9(8)V9(4).

           01 TOT           PIC 9(8).
           
           01 SDIGIT.
              03 SDIGIT01 PIC X(1).
              03 SDIGIT02 PIC X(1).
                
           01 NDIGIT REDEFINES SDIGIT PIC S9(04) COMP.


       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 

       EXEC SQL VAR NUM3 IS FLOAT END-EXEC .
       
       EXEC SQL VAR NUM4 IS FLOAT(6,2) END-EXEC.
         
       PROCEDURE DIVISION. 
 
       000-CONNECT.

           DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
           ACCEPT DATASRC FROM ENVIRONMENT-VALUE.
           DISPLAY "DATASRC_USR" UPON ENVIRONMENT-NAME.
           ACCEPT DBUSR FROM ENVIRONMENT-VALUE.
           
           DISPLAY '***************************************'.
           DISPLAY " DATASRC  : " DATASRC.
           DISPLAY " AUTH     : " DBUSR.
           DISPLAY '***************************************'.

           EXEC SQL
              CONNECT TO :DATASRC USER :DBUSR
           END-EXEC.      
           
           IF SQLCODE <> 0 THEN
              DISPLAY 'CONNECT SQLCODE. ' SQLCODE
              DISPLAY 'CONNECT SQLERRM. ' SQLERRM
              GO TO 100-EXIT
           END-IF.

       100-MAIN.

      *     EXEC SQL
      *        SELECT BFLD, VBFLD, CFLD, VCFLD, NUM1 
      *          INTO :BFLD, :VBFLD, :CFLD, :VCFLD, :NUM1 FROM BINTEST
      *     END-EXEC.
      *     
      *     IF SQLCODE <> 0 THEN
      *        DISPLAY 'SELECT SQLCODE. ' SQLCODE
      *        DISPLAY 'SELECT SQLERRMC. ' SQLERRMC
      *        GO TO 100-EXIT
      *     END-IF.
      *     
      *     MOVE LOW-VALUES TO SDIGIT01.
      *     MOVE LOW-VALUES TO SDIGIT02.
      * 
      *     MOVE VBFLD-DATA(1:1) TO SDIGIT02.
      *     DISPLAY 'DIGIT1 : ' NDIGIT.
      * 
      *     MOVE VBFLD-DATA(2:1) TO SDIGIT02.
      *     DISPLAY 'DIGIT2 : ' NDIGIT.
      * 
      *     MOVE VBFLD-DATA(83:1) TO SDIGIT02.
      *     DISPLAY 'DIGIT83: ' NDIGIT.
      * 
      *     DISPLAY 'BFLD        : [' BFLD ']'.
      *     DISPLAY 'BFLD LENGTH: ' LENGTH OF BFLD.
      * 
      *     DISPLAY 'VBFLD-DATA  : [' VBFLD-DATA(1:VBFLD-LENGTH) ']'.
      *     DISPLAY 'VBFLD-DATA  : [' VBFLD-DATA(1:88) ']'.
      *     DISPLAY 'VBFLD-LENGTH: ' VBFLD-LENGTH.
      * 
      *     DISPLAY 'CFLD        : [' CFLD ']'.
      *     DISPLAY 'CFLD LENGTH: ' LENGTH OF CFLD.
      * 
      *     DISPLAY 'VCFLD-DATA  : [' VCFLD-DATA(1:VCFLD-LENGTH) ']'.
      *     DISPLAY 'VCFLD-LENGTH: ' VCFLD-LENGTH.
      * 
      *     EXEC SQL
      *        CONNECT RESET
      *     END-EXEC.      
      *     
      *     IF SQLCODE <> 0 THEN
      *        DISPLAY 'DISCONNECT SQLCODE. ' SQLCODE
      *        DISPLAY 'DISCONNECT SQLERRM. ' SQLERRM
      *        GO TO 100-EXIT
      *     END-IF.
      * 
        100-EXIT. 
             STOP RUN.
