        IDENTIFICATION DIVISION.
        PROGRAM-ID. ISOGRAM.
        ENVIRONMENT DIVISION.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
       01 WS-PHRASE        PIC X(60).
       01 WS-PRASE-TABLE REDEFINES WS-PHRASE.
          05 WS-CHAR       PIC X(1) OCCURS 60 TIMES. 
       01 WS-CURRENT-CHAR  PIC X(1).
       01 WS-CHAR-DUP      PIC 99.
       01 I                PIC 99.
       01 J                PIC 99.
       01 WS-RESULT        PIC 99    VALUE 1.
        PROCEDURE DIVISION.
       ISOGRAM SECTION.
           PERFORM VARYING I FROM 1 BY 1
              UNTIL I > 60
                   MOVE WS-CHAR(I) TO WS-CURRENT-CHAR
                   INSPECT WS-PHRASE TALLYING WS-CHAR-DUP FOR
                      ALL WS-CURRENT-CHAR
                   IF WS-CHAR-DUP > 2
                      MOVE 0 TO WS-RESULT
                   END-IF
           END-PERFORM.
           STOP RUN.