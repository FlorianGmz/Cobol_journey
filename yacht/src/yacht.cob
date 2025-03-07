       IDENTIFICATION DIVISION.
       PROGRAM-ID. YACHT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RESULT       PIC 99    VALUE 0.
       01 WS-CATEGORY     PIC X(15).
       01 WS-DICE         PIC X(5).
       01 WS-DICE-NUM.
          05 WS-DIE       PIC 9 OCCURS 5 TIMES.
       01 WS-TWO-KINDS.
          05 WS-KIND-NUM  PIC 9 OCCURS 5 TIMES.
          05 WS-KIND-VAL  PIC 9 OCCURS 5 TIMES.
       PROCEDURE DIVISION.
       YACHT.
           MOVE ZERO TO WS-RESULT
           EVALUATE WS-CATEGORY
           WHEN 'ones'
                INSPECT WS-DICE TALLYING WS-RESULT FOR ALL '1'
           WHEN 'twos'
                INSPECT WS-DICE TALLYING WS-RESULT FOR ALL '2'
                COMPUTE WS-RESULT = WS-RESULT * 2
           WHEN 'threes'
                INSPECT WS-DICE TALLYING WS-RESULT FOR ALL '3'
                COMPUTE WS-RESULT = WS-RESULT * 3
           WHEN 'fours'
                INSPECT WS-DICE TALLYING WS-RESULT FOR ALL '4'
                COMPUTE WS-RESULT = WS-RESULT * 4
           WHEN 'fives'
                INSPECT WS-DICE TALLYING WS-RESULT FOR ALL '5'
                COMPUTE WS-RESULT = WS-RESULT * 5
           WHEN 'sixes'
                INSPECT WS-DICE TALLYING WS-RESULT FOR ALL '6'
                COMPUTE WS-RESULT = WS-RESULT * 6
           WHEN 'full house'
                PERFORM TWO-KINDS
                IF WS-DICE EQUAL SPACES AND FUNCTION ABS
                   (WS-KIND-NUM(1) - WS-KIND-NUM(2)) = 1
                   COMPUTE WS-RESULT = WS-KIND-NUM(1) * WS-KIND-VAL(1)
                      + WS-KIND-NUM(2) * WS-KIND-VAL(2)
           WHEN 'four of a kind'
                PERFORM TWO-KINDS
                IF WS-KIND-NUM(1) >= 4 AND WS-KIND-VAL(1) NOT ZERO
                   COMPUTE WS-RESULT = 4 * WS-KIND-VAL(1)
                   IF WS-KIND-NUM(2) >= 4 AND WS-KIND-VAL(2) NOT ZERO
                      COMPUTE WS-RESULT = 4 * WS-KIND-VAL(2)
           WHEN 'little straight'
                PERFORM STRAIGHT
           WHEN 'big straight'
                PERFORM STRAIGHT
           WHEN 'choice'
                MOVE WS-DICE TO WS-DICE-NUM
                COMPUTE WS-RESULT = WS-DIE(1) + WS-DIE(2) + WS-DIE(3) +
                   WS-DIE(4) + WS-DIE(5)
           WHEN 'yacht'
                PERFORM TWO-KINDS
                IF WS-DICE EQUAL SPACES
                   MOVE 50 TO WS-RESULT
           WHEN OTHER
                DISPLAY 'INVALID VALUE OF WS-CATEGORY'
           END-EVALUATE.

       TWO-KINDS.
           MOVE ZERO TO WS-TWO-KINDS
           MOVE WS-DICE(1:1) TO WS-KIND-VAL(1)
           INSPECT WS-DICE TALLYING WS-KIND-NUM(1) FOR ALL WS-DICE(1:1)
              REPLACING ALL WS-DICE(1:1) BY SPACES
           MOVE FUNCTION TRIM(WS-DICE) TO WS-DICE
           MOVE WS-DICE(1:1) TO WS-KIND-VAL(2)
           INSPECT WS-DICE TALLYING WS-KIND-NUM(2) FOR ALL WS-DICE(1:1)
              REPLACING ALL WS-DICE(1:1) BY SPACES.

       STRAIGHT.
           IF WS-CATEGORY = 'little straight'
              INSPECT WS-DICE REPLACING FIRST '1' BY SPACES.
           INSPECT WS-DICE REPLACING FIRST '2' BY SPACES
           INSPECT WS-DICE REPLACING FIRST '3' BY SPACES
           INSPECT WS-DICE REPLACING FIRST '4' BY SPACES
           INSPECT WS-DICE REPLACING FIRST '5' BY SPACES
           IF WS-CATEGORY = 'big straight'
              INSPECT WS-DICE REPLACING FIRST '6' BY SPACES.
           IF WS-DICE EQUAL SPACES
              MOVE 30 TO WS-RESULT.