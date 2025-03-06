       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEAP.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-YEAR    PIC 9(4).
       01 WS-RESULT  PIC 9(20).

       PROCEDURE DIVISION.
       LEAP SECTION.
           IF WS-YEAR / 4 = 0
              IF WS-YEAR / 100 = 0 
                 IF WS-YEAR / 400 = 0
                    MOVE 1 TO WS-RESULT 
                 ELSE
                    MOVE 0 TO WS-RESULT  
                 END-IF 
              ELSE 
                 MOVE 1 TO WS-RESULT 
              END-IF 
           ELSE
              MOVE 0 TO WS-RESULT  
           END-IF 
           STOP RUN.