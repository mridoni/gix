﻿       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL015A. 
       
       
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

           01 Z-MY-ELEMENTS PIC 9(8).
       
           78 MY-CONSTANT   VALUE 16.

       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 

       LINKAGE SECTION.

           01 L-DYNBUFFER-LEN PIC 9(4).

           01 DYNBUFFER.
              05 FILLER     OCCURS UNBOUNDED
                            DEPENDING ON L-DYNBUFFER-LEN
                            PIC X.       
      
           01 L-DYNBUFFER.
              05 FILLER     OCCURS 0 TO MY-CONSTANT TIMES
                            DEPENDING ON L-DYNBUFFER-LEN
                            PIC X.

           01 MY-TAB.
              05 MY-NO         PIC  9(009) COMP-5 VALUE ZERO.
              05 MY-TAB-CACHE  OCCURS 100
                 DEPENDING ON Z-MY-ELEMENTS
                 ASCENDING KEY IS MY-ID
                 INDEXED BY I-TAB.
                07 MY-ELEMENT.
                  10 MY-ID         PIC  9(009) COMP-5 VALUE ZERO.
                  10 MY-DATA       PIC  X(02189)      VALUE SPACE.
       
       PROCEDURE DIVISION. 
 
       000-CONNECT.
         DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
         ACCEPT DATASRC FROM ENVIRONMENT-VALUE.
         DISPLAY "DATASRC_USR" UPON ENVIRONMENT-NAME.
         ACCEPT DBUSR FROM ENVIRONMENT-VALUE.
         DISPLAY "DATASRC_PWD" UPON ENVIRONMENT-NAME.
         ACCEPT DBPWD FROM ENVIRONMENT-VALUE.

         EXEC SQL
            CONNECT TO :DATASRC USER :DBUSR USING :DBPWD
         END-EXEC.      
         
         DISPLAY 'CONNECT SQLCODE: ' SQLCODE

         IF SQLCODE <> 0 THEN
            GO TO 100-EXIT
         END-IF.

       100-MAIN.

         EXEC SQL CONNECT RESET END-EXEC.

       100-EXIT. 
             STOP RUN.

       200-END.
