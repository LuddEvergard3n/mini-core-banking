      ******************************************************************
      * VALIDATION.COB - Business Rules Validation
      * 
      * Implements all business rules and validations for banking ops.
      * 
      * Design decisions:
      * - All validations return boolean flags
      * - Error messages stored in working storage
      * - No side effects - pure validation logic
      * - Separates policy from execution
      *
      * Rules implemented:
      * - Minimum/maximum transaction amounts
      * - Account status checks
      * - Sufficient balance checks
      * - Account number format validation
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDATION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  WS-VALIDATION-FLAGS.
           05  WS-IS-VALID             PIC 9 VALUE 0.
               88  VALID               VALUE 1.
               88  INVALID             VALUE 0.
           05  WS-ERROR-CODE           PIC 99.
           05  WS-ERROR-MESSAGE        PIC X(80).
       
       01  WS-BUSINESS-RULES.
           05  MIN-CREDIT-AMOUNT       PIC 9(7)V99 VALUE 0.01.
           05  MAX-CREDIT-AMOUNT       PIC 9(13)V99 VALUE 999999999999.99.
           05  MIN-DEBIT-AMOUNT        PIC 9(7)V99 VALUE 0.01.
           05  MAX-DEBIT-AMOUNT        PIC 9(13)V99 VALUE 999999999999.99.
           05  MIN-ACCOUNT-ID          PIC 9(8) VALUE 10000000.
           05  MAX-ACCOUNT-ID          PIC 9(8) VALUE 99999999.
       
       01  WS-AMOUNT-TO-VALIDATE       PIC S9(13)V99.
       01  WS-BALANCE-TO-CHECK         PIC S9(13)V99.
       01  WS-STATUS-TO-CHECK          PIC X(8).
       01  WS-ACCOUNT-ID-TO-CHECK      PIC 9(8).
       
       PROCEDURE DIVISION.
       
      ******************************************************************
      * AMOUNT VALIDATIONS
      ******************************************************************
       
       VALIDATE-CREDIT-AMOUNT.
      *    Validate credit transaction amount
      *    Input: WS-AMOUNT-TO-VALIDATE
      *    Output: WS-IS-VALID, WS-ERROR-MESSAGE
           MOVE 0 TO WS-IS-VALID.
           MOVE SPACES TO WS-ERROR-MESSAGE.
           
           IF WS-AMOUNT-TO-VALIDATE <= 0
               MOVE 'Credit amount must be positive' 
                   TO WS-ERROR-MESSAGE
               MOVE 01 TO WS-ERROR-CODE
               GO TO VALIDATE-CREDIT-AMOUNT-END
           END-IF.
           
           IF WS-AMOUNT-TO-VALIDATE < MIN-CREDIT-AMOUNT
               MOVE 'Credit amount below minimum allowed' 
                   TO WS-ERROR-MESSAGE
               MOVE 02 TO WS-ERROR-CODE
               GO TO VALIDATE-CREDIT-AMOUNT-END
           END-IF.
           
           IF WS-AMOUNT-TO-VALIDATE > MAX-CREDIT-AMOUNT
               MOVE 'Credit amount exceeds maximum allowed' 
                   TO WS-ERROR-MESSAGE
               MOVE 03 TO WS-ERROR-CODE
               GO TO VALIDATE-CREDIT-AMOUNT-END
           END-IF.
           
           MOVE 1 TO WS-IS-VALID.
           
       VALIDATE-CREDIT-AMOUNT-END.
           EXIT.
       
       VALIDATE-DEBIT-AMOUNT.
      *    Validate debit transaction amount
      *    Input: WS-AMOUNT-TO-VALIDATE
      *    Output: WS-IS-VALID, WS-ERROR-MESSAGE
           MOVE 0 TO WS-IS-VALID.
           MOVE SPACES TO WS-ERROR-MESSAGE.
           
           IF WS-AMOUNT-TO-VALIDATE <= 0
               MOVE 'Debit amount must be positive' 
                   TO WS-ERROR-MESSAGE
               MOVE 11 TO WS-ERROR-CODE
               GO TO VALIDATE-DEBIT-AMOUNT-END
           END-IF.
           
           IF WS-AMOUNT-TO-VALIDATE < MIN-DEBIT-AMOUNT
               MOVE 'Debit amount below minimum allowed' 
                   TO WS-ERROR-MESSAGE
               MOVE 12 TO WS-ERROR-CODE
               GO TO VALIDATE-DEBIT-AMOUNT-END
           END-IF.
           
           IF WS-AMOUNT-TO-VALIDATE > MAX-DEBIT-AMOUNT
               MOVE 'Debit amount exceeds maximum allowed' 
                   TO WS-ERROR-MESSAGE
               MOVE 13 TO WS-ERROR-CODE
               GO TO VALIDATE-DEBIT-AMOUNT-END
           END-IF.
           
           MOVE 1 TO WS-IS-VALID.
           
       VALIDATE-DEBIT-AMOUNT-END.
           EXIT.
       
      ******************************************************************
      * BALANCE VALIDATIONS
      ******************************************************************
       
       VALIDATE-SUFFICIENT-BALANCE.
      *    Check if balance is sufficient for debit
      *    Input: WS-BALANCE-TO-CHECK, WS-AMOUNT-TO-VALIDATE
      *    Output: WS-IS-VALID, WS-ERROR-MESSAGE
           MOVE 0 TO WS-IS-VALID.
           MOVE SPACES TO WS-ERROR-MESSAGE.
           
           IF WS-BALANCE-TO-CHECK < WS-AMOUNT-TO-VALIDATE
               MOVE 'Insufficient funds for operation' 
                   TO WS-ERROR-MESSAGE
               MOVE 21 TO WS-ERROR-CODE
           ELSE
               MOVE 1 TO WS-IS-VALID
           END-IF.
       
      ******************************************************************
      * ACCOUNT STATUS VALIDATIONS
      ******************************************************************
       
       VALIDATE-ACCOUNT-ACTIVE.
      *    Check if account is active for operations
      *    Input: WS-STATUS-TO-CHECK
      *    Output: WS-IS-VALID, WS-ERROR-MESSAGE
           MOVE 0 TO WS-IS-VALID.
           MOVE SPACES TO WS-ERROR-MESSAGE.
           
           IF WS-STATUS-TO-CHECK = 'ACTIVE'
               MOVE 1 TO WS-IS-VALID
           ELSE IF WS-STATUS-TO-CHECK = 'BLOCKED'
               MOVE 'Account is blocked' TO WS-ERROR-MESSAGE
               MOVE 31 TO WS-ERROR-CODE
           ELSE IF WS-STATUS-TO-CHECK = 'CLOSED'
               MOVE 'Account is closed' TO WS-ERROR-MESSAGE
               MOVE 32 TO WS-ERROR-CODE
           ELSE
               MOVE 'Invalid account status' TO WS-ERROR-MESSAGE
               MOVE 33 TO WS-ERROR-CODE
           END-IF.
       
       VALIDATE-ACCOUNT-FOR-DEBIT.
      *    Comprehensive validation for debit operations
      *    Checks status and balance
      *    Input: WS-STATUS-TO-CHECK, WS-BALANCE-TO-CHECK, 
      *           WS-AMOUNT-TO-VALIDATE
      *    Output: WS-IS-VALID, WS-ERROR-MESSAGE
           PERFORM VALIDATE-ACCOUNT-ACTIVE.
           IF NOT VALID
               GO TO VALIDATE-ACCOUNT-FOR-DEBIT-END
           END-IF.
           
           PERFORM VALIDATE-DEBIT-AMOUNT.
           IF NOT VALID
               GO TO VALIDATE-ACCOUNT-FOR-DEBIT-END
           END-IF.
           
           PERFORM VALIDATE-SUFFICIENT-BALANCE.
           
       VALIDATE-ACCOUNT-FOR-DEBIT-END.
           EXIT.
       
       VALIDATE-ACCOUNT-FOR-CREDIT.
      *    Comprehensive validation for credit operations
      *    Input: WS-STATUS-TO-CHECK, WS-AMOUNT-TO-VALIDATE
      *    Output: WS-IS-VALID, WS-ERROR-MESSAGE
           PERFORM VALIDATE-ACCOUNT-ACTIVE.
           IF NOT VALID
               GO TO VALIDATE-ACCOUNT-FOR-CREDIT-END
           END-IF.
           
           PERFORM VALIDATE-CREDIT-AMOUNT.
           
       VALIDATE-ACCOUNT-FOR-CREDIT-END.
           EXIT.
       
      ******************************************************************
      * ACCOUNT ID VALIDATIONS
      ******************************************************************
       
       VALIDATE-ACCOUNT-ID-FORMAT.
      *    Validate account ID is in valid range
      *    Input: WS-ACCOUNT-ID-TO-CHECK
      *    Output: WS-IS-VALID, WS-ERROR-MESSAGE
           MOVE 0 TO WS-IS-VALID.
           MOVE SPACES TO WS-ERROR-MESSAGE.
           
           IF WS-ACCOUNT-ID-TO-CHECK < MIN-ACCOUNT-ID
               MOVE 'Account ID below minimum range' 
                   TO WS-ERROR-MESSAGE
               MOVE 41 TO WS-ERROR-CODE
           ELSE IF WS-ACCOUNT-ID-TO-CHECK > MAX-ACCOUNT-ID
               MOVE 'Account ID exceeds maximum range' 
                   TO WS-ERROR-MESSAGE
               MOVE 42 TO WS-ERROR-CODE
           ELSE
               MOVE 1 TO WS-IS-VALID
           END-IF.
       
       END PROGRAM VALIDATION.
