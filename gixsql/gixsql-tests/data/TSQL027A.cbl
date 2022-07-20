﻿       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL027A. 
       
       
       ENVIRONMENT DIVISION. 
       
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. IBM-AT. 
       OBJECT-COMPUTER. IBM-AT. 
       
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
       
       DATA DIVISION.  

       FILE SECTION.
      
       WORKING-STORAGE SECTION. 
       
           01 DATASRC     PIC X(64).
           01 DBUSR       PIC X(64).
           01 DBPWD       PIC X(64).
           01 DBS         PIC X(64).

           01  SQLCOMMAND.
               49 SQLCOMMAND-LEN    PIC S9(8) COMP-5.
               49 SQLCOMMAND-ARR    PIC X(250).

           01 IDX         PIC 9(8).
           01 VAR-TMP     PIC 9(8).
           01 VAR-RES     PIC 9(8).
           
           01 CUR-STEP    PIC X(16).
               
       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 

       EXEC SQL AT :DBS
           DECLARE VM1 CURSOR FOR :SQLCOMMAND
       END-EXEC.
      
       EXEC SQL AT :DBS
           DECLARE VM2 CURSOR FOR PREPSTMT1
       END-EXEC.
      
       EXEC SQL AT :DBS
           DECLARE VM3 CURSOR FOR 
                SELECT MAX(VAR) - MIN(VAR) FROM TAB00
       END-EXEC.

       PROCEDURE DIVISION. 
 
       000-CONNECT.
           DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
           ACCEPT DATASRC FROM ENVIRONMENT-VALUE.
           DISPLAY "DATASRC_USR" UPON ENVIRONMENT-NAME.
           ACCEPT DBUSR FROM ENVIRONMENT-VALUE.
           DISPLAY "DATASRC_PWD" UPON ENVIRONMENT-NAME.
           ACCEPT DBPWD FROM ENVIRONMENT-VALUE.

           EXEC SQL WHENEVER SQLERROR GO TO 999-PRG-ERR END-EXEC.

           MOVE 'CONN1' TO DBS.

           MOVE 'CONNECT' TO CUR-STEP.
           EXEC SQL
             CONNECT :DBUSR
                     IDENTIFIED BY :DBPWD
                     AT            :DBS
                     USING         :DATASRC
           END-EXEC.

           MOVE 'START TRANSACTION' TO CUR-STEP.
           EXEC SQL AT :DBS
              START TRANSACTION
           END-EXEC.        
           
           MOVE 'DROP TABLE' TO CUR-STEP.
           EXEC SQL AT :DBS
                DROP TABLE IF EXISTS TAB00
           END-EXEC.

           MOVE 'CREATE TABLE' TO CUR-STEP.
           EXEC SQL AT :DBS
                CREATE TABLE TAB00 (VAR INT)
           END-EXEC.

           MOVE 1 TO IDX

           PERFORM UNTIL IDX > 10 OR SQLCODE < 0 OR SQLCODE = 100
               MOVE 'INSERT' TO CUR-STEP
               MOVE IDX TO VAR-TMP
               EXEC SQL AT :DBS
                   INSERT INTO TAB00 (VAR)
                       VALUES (:VAR-TMP)
               END-EXEC

               ADD 1 TO IDX
           END-PERFORM.

      * VM1 : CURSOR FROM FIELD CONTENT (EVALUATED AT OPEN)

           MOVE 'SELECT MAX(VAR) FROM TAB00' TO SQLCOMMAND-ARR
           MOVE FUNCTION LENGTH (FUNCTION TRIM (SQLCOMMAND-ARR))
             TO SQLCOMMAND-LEN
      
           MOVE 'OPEN-VM1' TO CUR-STEP. 
           EXEC SQL AT :DBS
               OPEN VM1 
           END-EXEC.
          
           EXEC SQL AT :DBS
               FETCH VM1 
               INTO
                 :VAR-RES
           END-EXEC.
           DISPLAY 'VM1 - VAR-RES:' VAR-RES.
          
           EXEC SQL AT :DBS
               CLOSE VM1 
           END-EXEC.

      * VM2 : CURSOR FROM PREPARED STATEMENT (FROM FIELD, EVAL. AT OPEN)

           MOVE 'SELECT MIN(VAR) FROM TAB00' TO SQLCOMMAND-ARR
           MOVE FUNCTION LENGTH (FUNCTION TRIM (SQLCOMMAND-ARR))
             TO SQLCOMMAND-LEN

           MOVE 'PREPARE-VM2' TO CUR-STEP. 
           EXEC SQL AT :DBS
               PREPARE PREPSTMT1 FROM :SQLCOMMAND
           END-EXEC.

           MOVE 'OPEN-VM2' TO CUR-STEP. 
           EXEC SQL AT :DBS
               OPEN VM2 
           END-EXEC.
          
           EXEC SQL AT :DBS
               FETCH VM2
               INTO
                 :VAR-RES
           END-EXEC.
           DISPLAY 'VM2 - VAR-RES:' VAR-RES.
          
           EXEC SQL AT :DBS
               CLOSE VM2 
           END-EXEC.

      * VM3 : CURSOR FROM DIRECT STATEMENT (EVAL. AT OPEN)

           MOVE 'OPEN-VM3' TO CUR-STEP. 
           EXEC SQL AT :DBS
               OPEN VM3 
           END-EXEC.
          
           EXEC SQL AT :DBS
               FETCH VM3
               INTO
                 :VAR-RES
           END-EXEC.
           DISPLAY 'VM3 - VAR-RES:' VAR-RES.
          
           EXEC SQL AT :DBS
               CLOSE VM3 
           END-EXEC.

      * DONE

           MOVE 'COMMIT' TO CUR-STEP.
           EXEC SQL AT :DBS
              COMMIT
           END-EXEC.        

           MOVE 'DISCONNECT' TO CUR-STEP.
           EXEC SQL
              CONNECT RESET :DBS
           END-EXEC.        

       200-EXIT.
           STOP RUN.

       999-PRG-ERR.
           DISPLAY 'ERR - ' CUR-STEP ' : ' SQLCODE.
           DISPLAY 'ERR - ' CUR-STEP ' : ' SQLERRMC(1:SQLERRML).
           MOVE -1 TO RETURN-CODE.