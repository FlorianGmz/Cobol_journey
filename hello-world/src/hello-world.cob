      *Sample COBOL program
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RESULT  PIC X(15).
       PROCEDURE DIVISION.
       HELLO-WORLD SECTION. 
           MOVE "Hello, World! " TO WS-RESULT.
           DISPLAY WS-RESULT.