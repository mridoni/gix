       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TEST001. 
       
       
       ENVIRONMENT DIVISION. 
       
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. IBM-AT. 
       OBJECT-COMPUTER. IBM-AT. 
       
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
       
       DATA DIVISION.  
       
       FILE SECTION.  
       
       WORKING-STORAGE SECTION. 
       
       EXEC SQL 
        INCLUDE EMPREC 
       END-EXEC. 
       
           01 DBNAME PIC X(64).
           01 DBAUTH PIC X(64).
           01 T1     PIC 9(3) VALUE 0.  
           01 DISP-RATE PIC 9(15). 
           01 DISP-COM PIC 9(3).  
           01 DISP-CODE PIC 9(8). 
           01 FAKE-CHAR PIC X.  
           01 ANSS PIC X. 
           01 COM-NULL-IND PIC S9(4) COMP. 
           
           01 VARC PIC X(20).
           01 VARD PIC X(20).
       
       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 
      *  declare cursor for select 
           EXEC SQL
               DECLARE EMPTBL CURSOR FOR
               SELECT                     
                    ENO,
                    LNAME,
                    FNAME,
                    STREET,
                    CITY,
                    ST,
                    ZIP,
                    DEPT,
                    PAYRATE,
                    COM,
                    MISCDATA
                 FROM EMPTABLE
               ORDER BY LNAME
           END-EXEC              
       PROCEDURE DIVISION. 
 
       000-CONNECT.
         DISPLAY "DBNAME" UPON ENVIRONMENT-NAME.
         ACCEPT DBNAME FROM ENVIRONMENT-VALUE.
         DISPLAY "DBAUTH" UPON ENVIRONMENT-NAME.
         ACCEPT DBAUTH FROM ENVIRONMENT-VALUE.
         
      *   DISPLAY '***************************************'.
      *   DISPLAY " DB  : " DBNAME.
      *   DISPLAY " USER: " DBAUTH.
      *   DISPLAY '***************************************'.

           EXEC SQL
              CONNECT TO :DBNAME USER :DBAUTH
           END-EXEC.      
           
           IF SQLCODE <> 0 THEN
              DISPLAY 'SQLCODE. ' SQLCODE
              DISPLAY 'SQLERRM. ' SQLERRM
              GO TO 100-EXIT
           END-IF.
       100-MAIN.

           EXEC SQL
              START TRANSACTION
	       END-EXEC.                                                    
       
      *  open cursor
           EXEC SQL
               OPEN EMPTBL
           END-EXEC 
           MOVE SQLCODE TO DISP-CODE
           DISPLAY 'open ' DISP-CODE.
           DISPLAY 'open ' SQLERRM.
       
      *  fetch a data item 
           EXEC SQL
               FETCH EMPTBL INTO 
                 :ENO,:LNAME,:FNAME,:STREET,:CITY, 
                 :ST,:ZIP,:DEPT,:PAYRATE, 
                 :COM,:MISCDATA
           END-EXEC. 
       
       100-test. 
           MOVE SQLCODE TO DISP-CODE
           DISPLAY 'fetch ' DISP-CODE
       
      *  loop until no more data
           PERFORM UNTIL SQLCODE < 0 OR SQLCODE = 100
       
      *  display the record
           MOVE PAYRATE TO DISP-RATE
           MOVE COM TO DISP-COM
           DISPLAY 'employee #: [' ENO ']'
           
           DISPLAY 'last name : [' LNAME ']'
           DISPLAY 'first name: [' FNAME ']'
           DISPLAY 'street    : [' STREET ']'
           DISPLAY 'city      : [' CITY ']'
           DISPLAY 'state     : [' ST ']'
           DISPLAY 'zip code  : [' ZIP ']'
           DISPLAY 'department: [' DEPT ']'
           DISPLAY 'payrate   : [' PAYRATE ']'
           DISPLAY 'commission: [' COM ']'
           DISPLAY 'misc      : [' MISCDATA-TEXT ']'
           DISPLAY 'misc (len): [' MISCDATA-LEN ']'
           
           IF COM-NULL-IND < 0 
               DISPLAY 'commission is null' 
           ELSE 
               DISPLAY 'commission ' DISP-COM 
           END-IF 
      *     DISPLAY 'Do you want to see the next record? (y/n)' 
      *     ACCEPT ANSS 
      *     IF ANSS = 'Y' OR 'y' 
               EXEC SQL 
                 FETCH EMPTBL INTO 
                   :ENO,:LNAME,:FNAME,:STREET,:CITY, 
                   :ST,:ZIP,:DEPT,:PAYRATE, 
                   :COM,:MISCDATA
               END-EXEC 
      *     ELSE 
      *         GO TO CLOSE-LOOP 
      *     END-IF 
           MOVE SQLCODE TO DISP-CODE 
           DISPLAY 'fetch ' DISP-CODE 
           DISPLAY 'fetch ' SQLCODE 
           END-PERFORM  
       
           DISPLAY 'All records in this table have been selected'. 
       
       CLOSE-LOOP.
      *  close the cursor 
           EXEC SQL 
               CLOSE EMPTBL 
           END-EXEC. 
       
       100-EXIT. 
             STOP RUN.