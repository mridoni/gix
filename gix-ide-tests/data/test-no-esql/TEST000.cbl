       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TEST000.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
       
       FILE-CONTROL.
       
       DATA DIVISION.
       
       WORKING-STORAGE SECTION.
           COPY COPY1.
      *     COPY COPY2.
           
       LINKAGE SECTION.
            01  PAR1      PIC X(255).
            01  PAR2      PIC 9(12).
                       
       PROCEDURE DIVISION.
           DISPLAY 'HELLO WORLD 1'.
           DISPLAY 'HELLO WORLD 2'.
           DISPLAY 'HELLO WORLD 3'.
           DISPLAY 'HELLO WORLD 4'.
           
           COPY COPY3.
           
           PERFORM MYSUB THRU MYSUB-EX.
           
           DISPLAY 'HELLO WORLD 5'.
           
           MOVE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' TO A.
           MOVE '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ' TO E.
           MOVE 18 TO LRED.
           
           MOVE 'DATA-G1' TO G1.
           MOVE 'DATA-GG1' TO GG1.
           
      *     ACCEPT G1 FROM CONSOLE.
            DISPLAY A.
            DISPLAY E.
            DISPLAY 'G1 IS NOW ' G1.
           
           STOP RUN.
           
       MYSUB.
           DISPLAY 'MYSUB'.
       
       MYSUB-EX.
