       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL003A. 
       
       
       ENVIRONMENT DIVISION. 
       
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. IBM-AT. 
       OBJECT-COMPUTER. IBM-AT. 
       
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
       
       DATA DIVISION.  
       
       FILE SECTION.  
       
       WORKING-STORAGE SECTION. 
       
           01 DATASRC-1 PIC X(64).
           01 DBUSR-1   PIC X(64).           
           
           01 DATASRC-2 PIC X(64).
           01 DBUSR-2   PIC X(64).
           
           01 T1     PIC 9(4) VALUE 0.  
           01 T2     PIC 9(4) VALUE 0.  
           01 TOT    PIC 9(4) VALUE 0.  
       
       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 
       
       PROCEDURE DIVISION. 
 
       000-CONNECT.
         DISPLAY "DATASRC1" UPON ENVIRONMENT-NAME.
         ACCEPT DATASRC-1 FROM ENVIRONMENT-VALUE.
         DISPLAY "DATASRC_USR1" UPON ENVIRONMENT-NAME.
         ACCEPT DBUSR-1 FROM ENVIRONMENT-VALUE.
         
         DISPLAY "DATASRC2" UPON ENVIRONMENT-NAME.
         ACCEPT DATASRC-2 FROM ENVIRONMENT-VALUE.
         DISPLAY "DATASRC_USR2" UPON ENVIRONMENT-NAME.
         ACCEPT DBUSR-2 FROM ENVIRONMENT-VALUE.         

         DISPLAY '***************************************'.
         DISPLAY " DATASRC1  : " DATASRC-1.
         DISPLAY " USER1     : " DBUSR-1.
         DISPLAY " DATASRC2  : " DATASRC-2.
         DISPLAY " USER2     : " DBUSR-2.         
         DISPLAY '***************************************'.

           EXEC SQL
              CONNECT TO :DATASRC-1 AS CONN1 USER :DBUSR-1
           END-EXEC.    
           
           DISPLAY 'CONNECT SQLCODE(1): ' SQLCODE
           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.         
           
           EXEC SQL
              CONNECT TO :DATASRC-2 AS CONN2 USER :DBUSR-2
           END-EXEC.   
           
           DISPLAY 'CONNECT SQLCODE(2): ' SQLCODE
           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.

       100-MAIN.       

           EXEC SQL AT CONN1 DROP TABLE IF EXISTS TAB1 END-EXEC.
           DISPLAY 'CONNECT DROP(1): ' SQLCODE

           EXEC SQL AT CONN2 DROP TABLE IF EXISTS TAB2 END-EXEC.
           DISPLAY 'CONNECT DROP(2): ' SQLCODE

           EXEC SQL AT CONN1 DROP TABLE IF EXISTS TAB2 END-EXEC.
           DISPLAY 'CONNECT DROP(3): ' SQLCODE

           EXEC SQL AT CONN2 DROP TABLE IF EXISTS TAB1 END-EXEC.
           DISPLAY 'CONNECT DROP(4): ' SQLCODE

           EXEC SQL AT CONN1 CREATE TABLE TAB1 (FLD1 INT) END-EXEC.
           DISPLAY 'CONNECT CREATE(1): ' SQLCODE

           EXEC SQL AT CONN2 CREATE TABLE TAB2 (FLD2 INT) END-EXEC.
           DISPLAY 'CONNECT CREATE(2): ' SQLCODE
           
           EXEC SQL AT CONN1 
                INSERT INTO TAB1 (FLD1) VALUES (1),(3),(5)
           END-EXEC.
           DISPLAY 'CONNECT INSERT(1): ' SQLCODE
 
           EXEC SQL AT CONN2 
                INSERT INTO TAB2 (FLD2) VALUES (100),(300),(500)
           END-EXEC.           
           DISPLAY 'CONNECT INSERT(2): ' SQLCODE
           
           EXEC SQL AT CONN1
               SELECT SUM(FLD1) INTO :T1 FROM TAB1
           END-EXEC. 
           DISPLAY 'CONNECT SUM(1): ' SQLCODE
           
           EXEC SQL AT CONN2
               SELECT SUM(FLD2) INTO :T2 FROM TAB2
           END-EXEC.      
           DISPLAY 'CONNECT SUM(2): ' SQLCODE
           
           COMPUTE TOT = T1 + T2.      

           DISPLAY 'T1   : ' T1.
           DISPLAY 'T2   : ' T2.
           DISPLAY 'TOTAL: ' TOT.
           
      *    THIS SHOULD FAIL
           EXEC SQL AT CONN2
               SELECT SUM(FLD1) INTO :T1 FROM TAB1
           END-EXEC. 
           DISPLAY 'SQLSTATE FAIL1 (OK IF <> 00000): ' SQLSTATE.
           
      *    THIS SHOULD FAIL
           EXEC SQL AT CONN1
               SELECT SUM(FLD2) INTO :T2 FROM TAB2
           END-EXEC. 
           DISPLAY 'SQLSTATE FAIL2 (OK IF <> 00000): ' SQLSTATE.  

       100-EXIT. 
             STOP RUN.
