      ******************************************************************
      * INPUT.COB - Input Handling Module
      * 
      * Handles keyboard input and validates user entries.
      * Provides type-safe input for different data types.
      *
      * Design decisions:
      * - Validates input format before returning
      * - Provides retry mechanism for invalid input
      * - Handles special keys (Enter, Escape)
      * - Cleans and normalizes input data
      *
      * Input types supported:
      * - Numeric (integers, account IDs)
      * - Decimal (amounts with 2 decimal places)
      * - Text (descriptions, names)
      * - Menu choices
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INPUT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  WS-INPUT-BUFFER             PIC X(80).
       01  WS-INPUT-LENGTH             PIC 99.
       01  WS-INPUT-VALID              PIC 9.
           88  INPUT-VALID             VALUE 1.
           88  INPUT-INVALID           VALUE 0.
       
       01  WS-INPUT-TYPE               PIC X(10).
           88  INPUT-NUMERIC           VALUE 'NUMERIC'.
           88  INPUT-DECIMAL           VALUE 'DECIMAL'.
           88  INPUT-TEXT              VALUE 'TEXT'.
           88  INPUT-MENU              VALUE 'MENU'.
       
      * Output values
       01  WS-NUMERIC-OUTPUT           PIC 9(12).
       01  WS-DECIMAL-OUTPUT           PIC S9(13)V99.
       01  WS-TEXT-OUTPUT              PIC X(80).
       01  WS-MENU-OUTPUT              PIC 99.
       
      * Validation working storage
       01  WS-CHAR-INDEX               PIC 99.
       01  WS-CURRENT-CHAR             PIC X.
       01  WS-DECIMAL-POINT-COUNT      PIC 9.
       01  WS-DECIMAL-POINT-POS        PIC 99.
       
       PROCEDURE DIVISION.
       
      ******************************************************************
      * BASIC INPUT OPERATIONS
      ******************************************************************
       
       READ-LINE.
      *    Read line from terminal
      *    Output: WS-INPUT-BUFFER, WS-INPUT-LENGTH
           ACCEPT WS-INPUT-BUFFER.
           
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-INPUT-BUFFER)) TO WS-INPUT-LENGTH.
       
       CLEAR-INPUT-BUFFER.
      *    Clear input buffer
           MOVE SPACES TO WS-INPUT-BUFFER.
           MOVE 0 TO WS-INPUT-LENGTH.
       
      ******************************************************************
      * NUMERIC INPUT
      ******************************************************************
       
       GET-NUMERIC-INPUT.
      *    Get validated numeric input (no decimals)
      *    Output: WS-NUMERIC-OUTPUT, WS-INPUT-VALID
           
           PERFORM CLEAR-INPUT-BUFFER.
           PERFORM READ-LINE.
           
           IF WS-INPUT-LENGTH = 0
               MOVE 0 TO WS-INPUT-VALID
               GO TO GET-NUMERIC-INPUT-END
           END-IF.
           
           PERFORM VALIDATE-NUMERIC-FORMAT.
           
           IF INPUT-VALID
               MOVE FUNCTION NUMVAL(WS-INPUT-BUFFER) 
                   TO WS-NUMERIC-OUTPUT
           ELSE
               MOVE 0 TO WS-NUMERIC-OUTPUT
           END-IF.
           
       GET-NUMERIC-INPUT-END.
           EXIT.
       
       VALIDATE-NUMERIC-FORMAT.
      *    Validate that input contains only digits
      *    Input: WS-INPUT-BUFFER
      *    Output: WS-INPUT-VALID
           
           MOVE 1 TO WS-INPUT-VALID.
           
           PERFORM VARYING WS-CHAR-INDEX FROM 1 BY 1
                   UNTIL WS-CHAR-INDEX > WS-INPUT-LENGTH
               
               MOVE WS-INPUT-BUFFER(WS-CHAR-INDEX:1) 
                   TO WS-CURRENT-CHAR
               
               IF WS-CURRENT-CHAR < '0' OR WS-CURRENT-CHAR > '9'
                   MOVE 0 TO WS-INPUT-VALID
                   EXIT PERFORM
               END-IF
           END-PERFORM.
       
      ******************************************************************
      * DECIMAL INPUT
      ******************************************************************
       
       GET-DECIMAL-INPUT.
      *    Get validated decimal input (with 2 decimal places)
      *    Output: WS-DECIMAL-OUTPUT, WS-INPUT-VALID
           
           PERFORM CLEAR-INPUT-BUFFER.
           PERFORM READ-LINE.
           
           IF WS-INPUT-LENGTH = 0
               MOVE 0 TO WS-INPUT-VALID
               GO TO GET-DECIMAL-INPUT-END
           END-IF.
           
           PERFORM VALIDATE-DECIMAL-FORMAT.
           
           IF INPUT-VALID
               MOVE FUNCTION NUMVAL(WS-INPUT-BUFFER) 
                   TO WS-DECIMAL-OUTPUT
           ELSE
               MOVE 0 TO WS-DECIMAL-OUTPUT
           END-IF.
           
       GET-DECIMAL-INPUT-END.
           EXIT.
       
       VALIDATE-DECIMAL-FORMAT.
      *    Validate decimal format (digits with optional decimal point)
      *    Input: WS-INPUT-BUFFER
      *    Output: WS-INPUT-VALID
           
           MOVE 1 TO WS-INPUT-VALID.
           MOVE 0 TO WS-DECIMAL-POINT-COUNT.
           MOVE 0 TO WS-DECIMAL-POINT-POS.
           
           PERFORM VARYING WS-CHAR-INDEX FROM 1 BY 1
                   UNTIL WS-CHAR-INDEX > WS-INPUT-LENGTH
               
               MOVE WS-INPUT-BUFFER(WS-CHAR-INDEX:1) 
                   TO WS-CURRENT-CHAR
               
               IF WS-CURRENT-CHAR = '.' OR WS-CURRENT-CHAR = ','
                   ADD 1 TO WS-DECIMAL-POINT-COUNT
                   MOVE WS-CHAR-INDEX TO WS-DECIMAL-POINT-POS
                   IF WS-DECIMAL-POINT-COUNT > 1
                       MOVE 0 TO WS-INPUT-VALID
                       EXIT PERFORM
                   END-IF
               ELSE IF WS-CURRENT-CHAR < '0' 
                       OR WS-CURRENT-CHAR > '9'
                   MOVE 0 TO WS-INPUT-VALID
                   EXIT PERFORM
               END-IF
           END-PERFORM.
           
      *    Check decimal places if decimal point exists
           IF INPUT-VALID AND WS-DECIMAL-POINT-COUNT = 1
               COMPUTE WS-CHAR-INDEX = 
                   WS-INPUT-LENGTH - WS-DECIMAL-POINT-POS
               IF WS-CHAR-INDEX > 2
                   MOVE 0 TO WS-INPUT-VALID
               END-IF
           END-IF.
           
      *    Replace comma with period for NUMVAL
           IF INPUT-VALID AND WS-DECIMAL-POINT-COUNT = 1
               INSPECT WS-INPUT-BUFFER 
                   REPLACING ALL ',' BY '.'
           END-IF.
       
      ******************************************************************
      * TEXT INPUT
      ******************************************************************
       
       GET-TEXT-INPUT.
      *    Get text input (alphanumeric)
      *    Output: WS-TEXT-OUTPUT, WS-INPUT-VALID
           
           PERFORM CLEAR-INPUT-BUFFER.
           PERFORM READ-LINE.
           
           IF WS-INPUT-LENGTH = 0
               MOVE 0 TO WS-INPUT-VALID
               MOVE SPACES TO WS-TEXT-OUTPUT
           ELSE
               MOVE 1 TO WS-INPUT-VALID
               MOVE FUNCTION TRIM(WS-INPUT-BUFFER) TO WS-TEXT-OUTPUT
           END-IF.
       
      ******************************************************************
      * MENU CHOICE INPUT
      ******************************************************************
       
       GET-MENU-CHOICE.
      *    Get menu choice (1-99)
      *    Output: WS-MENU-OUTPUT, WS-INPUT-VALID
           
           PERFORM GET-NUMERIC-INPUT.
           
           IF INPUT-VALID
               IF WS-NUMERIC-OUTPUT >= 1 AND WS-NUMERIC-OUTPUT <= 99
                   MOVE WS-NUMERIC-OUTPUT TO WS-MENU-OUTPUT
               ELSE
                   MOVE 0 TO WS-INPUT-VALID
                   MOVE 0 TO WS-MENU-OUTPUT
               END-IF
           ELSE
               MOVE 0 TO WS-MENU-OUTPUT
           END-IF.
       
      ******************************************************************
      * ACCOUNT ID INPUT
      ******************************************************************
       
       GET-ACCOUNT-ID.
      *    Get validated account ID (8 digits)
      *    Output: WS-NUMERIC-OUTPUT, WS-INPUT-VALID
           
           PERFORM GET-NUMERIC-INPUT.
           
           IF INPUT-VALID
               IF WS-INPUT-LENGTH = 8
                   IF WS-NUMERIC-OUTPUT >= 10000000 
                      AND WS-NUMERIC-OUTPUT <= 99999999
                       CONTINUE
                   ELSE
                       MOVE 0 TO WS-INPUT-VALID
                   END-IF
               ELSE
                   MOVE 0 TO WS-INPUT-VALID
               END-IF
           END-IF.
       
      ******************************************************************
      * AMOUNT INPUT
      ******************************************************************
       
       GET-AMOUNT.
      *    Get validated amount (positive decimal)
      *    Output: WS-DECIMAL-OUTPUT, WS-INPUT-VALID
           
           PERFORM GET-DECIMAL-INPUT.
           
           IF INPUT-VALID
               IF WS-DECIMAL-OUTPUT <= 0
                   MOVE 0 TO WS-INPUT-VALID
               ELSE IF WS-DECIMAL-OUTPUT > 999999999999.99
                   MOVE 0 TO WS-INPUT-VALID
               END-IF
           END-IF.
       
      ******************************************************************
      * CONFIRMATION INPUT
      ******************************************************************
       
       GET-CONFIRMATION.
      *    Get yes/no confirmation
      *    Output: WS-INPUT-VALID (1=yes, 0=no)
           
           PERFORM CLEAR-INPUT-BUFFER.
           DISPLAY 'Confirm (Y/N)? ' WITH NO ADVANCING.
           PERFORM READ-LINE.
           
           MOVE FUNCTION UPPER-CASE(WS-INPUT-BUFFER(1:1)) 
               TO WS-CURRENT-CHAR.
           
           IF WS-CURRENT-CHAR = 'Y'
               MOVE 1 TO WS-INPUT-VALID
           ELSE
               MOVE 0 TO WS-INPUT-VALID
           END-IF.
       
      ******************************************************************
      * WAIT FOR ENTER
      ******************************************************************
       
       WAIT-FOR-ENTER.
      *    Wait for user to press Enter
           PERFORM CLEAR-INPUT-BUFFER.
           ACCEPT WS-INPUT-BUFFER.
       
       END PROGRAM INPUT.
