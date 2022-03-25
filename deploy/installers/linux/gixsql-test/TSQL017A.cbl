﻿       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL017A. 
       
       
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
           01 DBS         PIC X(64).
           01 DBUSR       PIC X(64).
           01 DBPWD       PIC X(64).

           01 KEY01       PIC 9(6).
           01 COL1        PIC 9(6).
           01 COL2        PIC 9(6).

           01 TOT-KEY01   PIC 9(6).
           01 TOT-COL1    PIC 9(6).
           01 TOT-COL2    PIC 9(6).


           01 IDX         PIC 9(6).

           01 STMT-1 SQL TYPE IS VARCHAR(100).
           01 STMT-2 SQL TYPE IS VARCHAR(100).
       
       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 

       PROCEDURE DIVISION. 
 
       000-CONNECT.
           DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
           ACCEPT DATASRC FROM ENVIRONMENT-VALUE.
           DISPLAY "DATASRC_USR" UPON ENVIRONMENT-NAME.
           ACCEPT DBUSR FROM ENVIRONMENT-VALUE.
           DISPLAY "DATASRC_PWD" UPON ENVIRONMENT-NAME.
           ACCEPT DBPWD FROM ENVIRONMENT-VALUE.
           
           MOVE 'CONN1' TO DBS.

           EXEC SQL
              CONNECT TO :DATASRC AS :DBS USER :DBUSR USING :DBPWD
           END-EXEC.      

           DISPLAY 'CONNECT SQLCODE: ' SQLCODE.
           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.

       100-MAIN.

           MOVE 1 TO IDX.
           MOVE 'INSERT INTO TAB_A (KEY01, COL1, COL2)
      -          ' VALUES ($1, $2, $3)' TO STMT-1

           EXEC SQL AT :DBS PREPARE ST1 FROM :STMT-1 END-EXEC.
           DISPLAY 'PREPARE SQLCODE: ' SQLCODE.
           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.

      *  loop until no more data
           PERFORM UNTIL IDX > 10 OR SQLCODE < 0 OR SQLCODE = 100

           MOVE IDX TO KEY01, COL1, COL2
           ADD 100 TO COL1
           ADD 200 TO COL2

           EXEC SQL AT :DBS 
                EXECUTE ST1 USING :KEY01, :COL1, :COL2 
           END-EXEC     
           DISPLAY 'EXECUTE SQLCODE: ' IDX SQLCODE
           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF

           ADD 1 TO IDX

           END-PERFORM.

           EXEC SQL AT :DBS
                SELECT SUM(KEY01), SUM(COL1), SUM(COL2)
                    INTO :TOT-KEY01, :TOT-COL1, :TOT-COL2
                        FROM TAB_A
           END-EXEC.

           DISPLAY 'SUM SQLCODE: ' SQLCODE.
           IF SQLCODE <> 0 THEN
              DISPLAY 'SUM SQLERRMC: ' SQLERRMC
              GO TO 100-EXIT
           END-IF.
       
      *  display the record

           DISPLAY 'TOT-KEY01: [' TOT-KEY01 ']'.
           DISPLAY 'TOT-COL1: [' TOT-COL1 ']'.
           DISPLAY 'TOT-COL2: [' TOT-COL2 ']'.
       
       CLOSE-LOOP.


       EXEC SQL CONNECT RESET :DBS END-EXEC.

       100-EXIT. 
             STOP RUN.

       200-END.
