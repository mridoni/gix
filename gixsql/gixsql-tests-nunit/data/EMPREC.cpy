        01 EMP-TABLE.
              03 ENO PIC S9(4) COMP.   
              03 LNAME PIC X(10). 
              03 FNAME PIC X(10). 
              03 STREET PIC X(20).
              03 CITY PIC X(15).
              03 ST PIC XX.
              03 ZIP PIC X(5).
              03 DEPT PIC X(4).
              03 PAYRATE PIC S9(13)V99 COMP-3 VALUE 0.
              03 COM PIC S9V99 COMP-3.
              03 DNUM1 PIC S99V99 COMP-3.
              03 DNUM2 PIC S99V99 COMP-3.
              03 DNUM3 PIC S99V99 COMP-3.

        01 MISCDATA.
              49 MISCDATA-LEN
                 PIC 9(8) COMP-5.
              49 MISCDATA-TEXT
                 PIC X(128).
