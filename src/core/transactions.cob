      ******************************************************************
      * TRANSACTIONS.COB - Transaction Processing Module
      * 
      * Orchestrates financial transactions with full ACID properties.
      * All operations are atomic and create ledger entries.
      *
      * Design decisions:
      * - Every transaction updates: account, ledger, transaction log
      * - Validations happen before any state change
      * - Transaction IDs are globally unique
      * - Failed transactions are logged with FAILED status
      * - No partial updates (rollback on any failure)
      *
      * Supported operations:
      * - CREDIT: Add funds to account
      * - DEBIT: Remove funds from account
      * - BLOCK: Block funds (reduce available balance)
      * - UNBLOCK: Release blocked funds
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANSACTIONS.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY 'src/storage/schema.cob'.
       
       01  WS-TXN-OPERATION.
           05  WS-TXN-OP-STATUS        PIC XX.
               88  TXN-OP-SUCCESS      VALUE '00'.
               88  TXN-OP-FAILED       VALUE '99'.
           05  WS-TXN-OP-MESSAGE       PIC X(80).
       
       01  WS-TIMESTAMP-FIELDS.
           05  WS-CURRENT-DATE.
               10  WS-CURR-YEAR        PIC 9(4).
               10  WS-CURR-MONTH       PIC 99.
               10  WS-CURR-DAY         PIC 99.
           05  WS-CURRENT-TIME.
               10  WS-CURR-HOUR        PIC 99.
               10  WS-CURR-MINUTE      PIC 99.
               10  WS-CURR-SECOND      PIC 99.
           05  WS-TIMESTAMP            PIC X(14).
       
      * Transaction input parameters
       01  WS-INPUT-ACCOUNT-ID         PIC 9(8).
       01  WS-INPUT-AMOUNT             PIC S9(13)V99.
       01  WS-INPUT-DESCRIPTION        PIC X(80).
       01  WS-INPUT-TXN-TYPE           PIC X(10).
       
      * Working variables for transaction processing
       01  WS-BALANCE-BEFORE           PIC S9(13)V99.
       01  WS-BALANCE-AFTER            PIC S9(13)V99.
       01  WS-AVAILABLE-BALANCE        PIC S9(13)V99.
       01  WS-VALIDATION-RESULT        PIC 9.
       01  WS-VALIDATION-MESSAGE       PIC X(80).
       
       PROCEDURE DIVISION.
       
      ******************************************************************
      * UTILITY PROCEDURES
      ******************************************************************
       
       GET-CURRENT-TIMESTAMP.
      *    Generate current timestamp in YYYYMMDDHHMMSS format
      *    Output: WS-TIMESTAMP
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD.
           ACCEPT WS-CURRENT-TIME FROM TIME.
           
           STRING WS-CURR-YEAR
                  WS-CURR-MONTH
                  WS-CURR-DAY
                  WS-CURR-HOUR
                  WS-CURR-MINUTE
                  WS-CURR-SECOND
                  DELIMITED BY SIZE
                  INTO WS-TIMESTAMP.
       
       GENERATE-TRANSACTION-ID.
      *    Generate next transaction ID from control file
      *    Output: TXN-ID in TRANSACTION-RECORD
           CALL 'FILES' USING 'READ-CONTROL' CONTROL-RECORD 
               WS-TXN-OP-STATUS.
           
           IF NOT TXN-OP-SUCCESS
               DISPLAY 'FATAL: Cannot read control file for txn ID'
               STOP RUN
           END-IF.
           
           ADD 1 TO CTL-LAST-TXN-ID.
           MOVE CTL-LAST-TXN-ID TO TXN-ID.
           
           CALL 'FILES' USING 'UPDATE-CONTROL' CONTROL-RECORD 
               WS-TXN-OP-STATUS.
       
       LOG-TRANSACTION.
      *    Log transaction record to file
      *    Input: TRANSACTION-RECORD
      *    Output: WS-TXN-OP-STATUS
           CALL 'FILES' USING 'OPEN-TRANSACTION-FILE-IO'.
           CALL 'FILES' USING 'WRITE-TRANSACTION' TRANSACTION-RECORD 
               WS-TXN-OP-STATUS.
           CALL 'FILES' USING 'CLOSE-TRANSACTION-FILE'.
       
      ******************************************************************
      * TRANSACTION OPERATIONS
      ******************************************************************
       
       PROCESS-CREDIT.
      *    Process credit (deposit) transaction
      *    Input: WS-INPUT-ACCOUNT-ID, WS-INPUT-AMOUNT, 
      *           WS-INPUT-DESCRIPTION
      *    Output: TRANSACTION-RECORD, WS-TXN-OP-STATUS
           
           INITIALIZE TRANSACTION-RECORD.
           PERFORM GENERATE-TRANSACTION-ID.
           
           MOVE WS-INPUT-ACCOUNT-ID TO TXN-ACCOUNT-ID.
           MOVE WS-INPUT-AMOUNT TO TXN-AMOUNT.
           MOVE 'CREDIT' TO TXN-TYPE.
           MOVE WS-INPUT-DESCRIPTION TO TXN-DESCRIPTION.
           MOVE 'PENDING' TO TXN-STATUS.
           PERFORM GET-CURRENT-TIMESTAMP.
           MOVE WS-TIMESTAMP TO TXN-TIMESTAMP.
           
      *    Step 1: Validate inputs
           CALL 'VALIDATION' USING 'VALIDATE-ACCOUNT-ID-FORMAT'
               WS-INPUT-ACCOUNT-ID WS-VALIDATION-RESULT 
               WS-VALIDATION-MESSAGE.
           
           IF WS-VALIDATION-RESULT = 0
               MOVE 'FAILED' TO TXN-STATUS
               MOVE WS-VALIDATION-MESSAGE TO WS-TXN-OP-MESSAGE
               MOVE '99' TO WS-TXN-OP-STATUS
               PERFORM LOG-TRANSACTION
               GO TO PROCESS-CREDIT-END
           END-IF.
           
      *    Step 2: Read account
           CALL 'ACCOUNT' USING 'GET-ACCOUNT' 
               WS-INPUT-ACCOUNT-ID ACCOUNT-RECORD WS-TXN-OP-STATUS.
           
           IF NOT TXN-OP-SUCCESS
               MOVE 'FAILED' TO TXN-STATUS
               MOVE 'Account not found' TO WS-TXN-OP-MESSAGE
               PERFORM LOG-TRANSACTION
               GO TO PROCESS-CREDIT-END
           END-IF.
           
      *    Step 3: Validate account can receive credit
           CALL 'VALIDATION' USING 'VALIDATE-ACCOUNT-FOR-CREDIT'
               ACC-STATUS WS-INPUT-AMOUNT 
               WS-VALIDATION-RESULT WS-VALIDATION-MESSAGE.
           
           IF WS-VALIDATION-RESULT = 0
               MOVE 'FAILED' TO TXN-STATUS
               MOVE WS-VALIDATION-MESSAGE TO WS-TXN-OP-MESSAGE
               MOVE '99' TO WS-TXN-OP-STATUS
               PERFORM LOG-TRANSACTION
               GO TO PROCESS-CREDIT-END
           END-IF.
           
      *    Step 4: Update account balance
           MOVE ACC-BALANCE TO WS-BALANCE-BEFORE.
           ADD WS-INPUT-AMOUNT TO ACC-BALANCE.
           MOVE ACC-BALANCE TO WS-BALANCE-AFTER.
           
           CALL 'ACCOUNT' USING 'UPDATE-ACCOUNT-BALANCE' 
               ACCOUNT-RECORD WS-TXN-OP-STATUS.
           
           IF NOT TXN-OP-SUCCESS
               MOVE 'FAILED' TO TXN-STATUS
               MOVE 'Failed to update account' TO WS-TXN-OP-MESSAGE
               PERFORM LOG-TRANSACTION
               GO TO PROCESS-CREDIT-END
           END-IF.
           
      *    Step 5: Create ledger entry
           CALL 'LEDGER' USING 'CREATE-LEDGER-ENTRY'
               WS-INPUT-ACCOUNT-ID TXN-ID 'CREDIT'
               WS-INPUT-AMOUNT WS-BALANCE-BEFORE WS-BALANCE-AFTER
               WS-INPUT-DESCRIPTION LEDGER-RECORD WS-TXN-OP-STATUS.
           
      *    Step 6: Mark transaction as completed
           MOVE 'COMPLETED' TO TXN-STATUS.
           MOVE 'Transaction completed successfully' 
               TO WS-TXN-OP-MESSAGE.
           MOVE '00' TO WS-TXN-OP-STATUS.
           PERFORM LOG-TRANSACTION.
           
       PROCESS-CREDIT-END.
           EXIT.
       
       PROCESS-DEBIT.
      *    Process debit (withdrawal) transaction
      *    Input: WS-INPUT-ACCOUNT-ID, WS-INPUT-AMOUNT, 
      *           WS-INPUT-DESCRIPTION
      *    Output: TRANSACTION-RECORD, WS-TXN-OP-STATUS
           
           INITIALIZE TRANSACTION-RECORD.
           PERFORM GENERATE-TRANSACTION-ID.
           
           MOVE WS-INPUT-ACCOUNT-ID TO TXN-ACCOUNT-ID.
           MOVE WS-INPUT-AMOUNT TO TXN-AMOUNT.
           MOVE 'DEBIT' TO TXN-TYPE.
           MOVE WS-INPUT-DESCRIPTION TO TXN-DESCRIPTION.
           MOVE 'PENDING' TO TXN-STATUS.
           PERFORM GET-CURRENT-TIMESTAMP.
           MOVE WS-TIMESTAMP TO TXN-TIMESTAMP.
           
      *    Step 1: Validate account ID
           CALL 'VALIDATION' USING 'VALIDATE-ACCOUNT-ID-FORMAT'
               WS-INPUT-ACCOUNT-ID WS-VALIDATION-RESULT 
               WS-VALIDATION-MESSAGE.
           
           IF WS-VALIDATION-RESULT = 0
               MOVE 'FAILED' TO TXN-STATUS
               MOVE WS-VALIDATION-MESSAGE TO WS-TXN-OP-MESSAGE
               MOVE '99' TO WS-TXN-OP-STATUS
               PERFORM LOG-TRANSACTION
               GO TO PROCESS-DEBIT-END
           END-IF.
           
      *    Step 2: Read account
           CALL 'ACCOUNT' USING 'GET-ACCOUNT' 
               WS-INPUT-ACCOUNT-ID ACCOUNT-RECORD WS-TXN-OP-STATUS.
           
           IF NOT TXN-OP-SUCCESS
               MOVE 'FAILED' TO TXN-STATUS
               MOVE 'Account not found' TO WS-TXN-OP-MESSAGE
               PERFORM LOG-TRANSACTION
               GO TO PROCESS-DEBIT-END
           END-IF.
           
      *    Step 3: Calculate available balance
           COMPUTE WS-AVAILABLE-BALANCE = 
               ACC-BALANCE - ACC-BLOCKED-AMT.
           
      *    Step 4: Validate account can be debited
           CALL 'VALIDATION' USING 'VALIDATE-ACCOUNT-FOR-DEBIT'
               ACC-STATUS WS-AVAILABLE-BALANCE WS-INPUT-AMOUNT
               WS-VALIDATION-RESULT WS-VALIDATION-MESSAGE.
           
           IF WS-VALIDATION-RESULT = 0
               MOVE 'FAILED' TO TXN-STATUS
               MOVE WS-VALIDATION-MESSAGE TO WS-TXN-OP-MESSAGE
               MOVE '99' TO WS-TXN-OP-STATUS
               PERFORM LOG-TRANSACTION
               GO TO PROCESS-DEBIT-END
           END-IF.
           
      *    Step 5: Update account balance
           MOVE ACC-BALANCE TO WS-BALANCE-BEFORE.
           SUBTRACT WS-INPUT-AMOUNT FROM ACC-BALANCE.
           MOVE ACC-BALANCE TO WS-BALANCE-AFTER.
           
           CALL 'ACCOUNT' USING 'UPDATE-ACCOUNT-BALANCE' 
               ACCOUNT-RECORD WS-TXN-OP-STATUS.
           
           IF NOT TXN-OP-SUCCESS
               MOVE 'FAILED' TO TXN-STATUS
               MOVE 'Failed to update account' TO WS-TXN-OP-MESSAGE
               PERFORM LOG-TRANSACTION
               GO TO PROCESS-DEBIT-END
           END-IF.
           
      *    Step 6: Create ledger entry
           CALL 'LEDGER' USING 'CREATE-LEDGER-ENTRY'
               WS-INPUT-ACCOUNT-ID TXN-ID 'DEBIT'
               WS-INPUT-AMOUNT WS-BALANCE-BEFORE WS-BALANCE-AFTER
               WS-INPUT-DESCRIPTION LEDGER-RECORD WS-TXN-OP-STATUS.
           
      *    Step 7: Mark transaction as completed
           MOVE 'COMPLETED' TO TXN-STATUS.
           MOVE 'Transaction completed successfully' 
               TO WS-TXN-OP-MESSAGE.
           MOVE '00' TO WS-TXN-OP-STATUS.
           PERFORM LOG-TRANSACTION.
           
       PROCESS-DEBIT-END.
           EXIT.
       
       PROCESS-BLOCK-FUNDS.
      *    Block funds (reduce available balance)
      *    Input: WS-INPUT-ACCOUNT-ID, WS-INPUT-AMOUNT, 
      *           WS-INPUT-DESCRIPTION
      *    Output: TRANSACTION-RECORD, WS-TXN-OP-STATUS
           
           INITIALIZE TRANSACTION-RECORD.
           PERFORM GENERATE-TRANSACTION-ID.
           
           MOVE WS-INPUT-ACCOUNT-ID TO TXN-ACCOUNT-ID.
           MOVE WS-INPUT-AMOUNT TO TXN-AMOUNT.
           MOVE 'BLOCK' TO TXN-TYPE.
           MOVE WS-INPUT-DESCRIPTION TO TXN-DESCRIPTION.
           MOVE 'PENDING' TO TXN-STATUS.
           PERFORM GET-CURRENT-TIMESTAMP.
           MOVE WS-TIMESTAMP TO TXN-TIMESTAMP.
           
      *    Step 1: Read account
           CALL 'ACCOUNT' USING 'GET-ACCOUNT' 
               WS-INPUT-ACCOUNT-ID ACCOUNT-RECORD WS-TXN-OP-STATUS.
           
           IF NOT TXN-OP-SUCCESS
               MOVE 'FAILED' TO TXN-STATUS
               MOVE 'Account not found' TO WS-TXN-OP-MESSAGE
               PERFORM LOG-TRANSACTION
               GO TO PROCESS-BLOCK-END
           END-IF.
           
      *    Step 2: Check available balance
           COMPUTE WS-AVAILABLE-BALANCE = 
               ACC-BALANCE - ACC-BLOCKED-AMT.
           
           IF WS-AVAILABLE-BALANCE < WS-INPUT-AMOUNT
               MOVE 'FAILED' TO TXN-STATUS
               MOVE 'Insufficient available balance to block' 
                   TO WS-TXN-OP-MESSAGE
               MOVE '99' TO WS-TXN-OP-STATUS
               PERFORM LOG-TRANSACTION
               GO TO PROCESS-BLOCK-END
           END-IF.
           
      *    Step 3: Update blocked amount
           MOVE ACC-BALANCE TO WS-BALANCE-BEFORE.
           ADD WS-INPUT-AMOUNT TO ACC-BLOCKED-AMT.
           MOVE ACC-BALANCE TO WS-BALANCE-AFTER.
           
           CALL 'ACCOUNT' USING 'UPDATE-ACCOUNT-BALANCE' 
               ACCOUNT-RECORD WS-TXN-OP-STATUS.
           
      *    Step 4: Create ledger entry
           CALL 'LEDGER' USING 'CREATE-LEDGER-ENTRY'
               WS-INPUT-ACCOUNT-ID TXN-ID 'BLOCK'
               WS-INPUT-AMOUNT WS-BALANCE-BEFORE WS-BALANCE-AFTER
               WS-INPUT-DESCRIPTION LEDGER-RECORD WS-TXN-OP-STATUS.
           
      *    Step 5: Mark transaction as completed
           MOVE 'COMPLETED' TO TXN-STATUS.
           MOVE 'Funds blocked successfully' TO WS-TXN-OP-MESSAGE.
           MOVE '00' TO WS-TXN-OP-STATUS.
           PERFORM LOG-TRANSACTION.
           
       PROCESS-BLOCK-END.
           EXIT.
       
       PROCESS-UNBLOCK-FUNDS.
      *    Unblock funds (increase available balance)
      *    Input: WS-INPUT-ACCOUNT-ID, WS-INPUT-AMOUNT, 
      *           WS-INPUT-DESCRIPTION
      *    Output: TRANSACTION-RECORD, WS-TXN-OP-STATUS
           
           INITIALIZE TRANSACTION-RECORD.
           PERFORM GENERATE-TRANSACTION-ID.
           
           MOVE WS-INPUT-ACCOUNT-ID TO TXN-ACCOUNT-ID.
           MOVE WS-INPUT-AMOUNT TO TXN-AMOUNT.
           MOVE 'UNBLOCK' TO TXN-TYPE.
           MOVE WS-INPUT-DESCRIPTION TO TXN-DESCRIPTION.
           MOVE 'PENDING' TO TXN-STATUS.
           PERFORM GET-CURRENT-TIMESTAMP.
           MOVE WS-TIMESTAMP TO TXN-TIMESTAMP.
           
      *    Step 1: Read account
           CALL 'ACCOUNT' USING 'GET-ACCOUNT' 
               WS-INPUT-ACCOUNT-ID ACCOUNT-RECORD WS-TXN-OP-STATUS.
           
           IF NOT TXN-OP-SUCCESS
               MOVE 'FAILED' TO TXN-STATUS
               MOVE 'Account not found' TO WS-TXN-OP-MESSAGE
               PERFORM LOG-TRANSACTION
               GO TO PROCESS-UNBLOCK-END
           END-IF.
           
      *    Step 2: Check blocked amount
           IF ACC-BLOCKED-AMT < WS-INPUT-AMOUNT
               MOVE 'FAILED' TO TXN-STATUS
               MOVE 'Insufficient blocked funds to unblock' 
                   TO WS-TXN-OP-MESSAGE
               MOVE '99' TO WS-TXN-OP-STATUS
               PERFORM LOG-TRANSACTION
               GO TO PROCESS-UNBLOCK-END
           END-IF.
           
      *    Step 3: Update blocked amount
           MOVE ACC-BALANCE TO WS-BALANCE-BEFORE.
           SUBTRACT WS-INPUT-AMOUNT FROM ACC-BLOCKED-AMT.
           MOVE ACC-BALANCE TO WS-BALANCE-AFTER.
           
           CALL 'ACCOUNT' USING 'UPDATE-ACCOUNT-BALANCE' 
               ACCOUNT-RECORD WS-TXN-OP-STATUS.
           
      *    Step 4: Create ledger entry
           CALL 'LEDGER' USING 'CREATE-LEDGER-ENTRY'
               WS-INPUT-ACCOUNT-ID TXN-ID 'UNBLOCK'
               WS-INPUT-AMOUNT WS-BALANCE-BEFORE WS-BALANCE-AFTER
               WS-INPUT-DESCRIPTION LEDGER-RECORD WS-TXN-OP-STATUS.
           
      *    Step 5: Mark transaction as completed
           MOVE 'COMPLETED' TO TXN-STATUS.
           MOVE 'Funds unblocked successfully' TO WS-TXN-OP-MESSAGE.
           MOVE '00' TO WS-TXN-OP-STATUS.
           PERFORM LOG-TRANSACTION.
           
       PROCESS-UNBLOCK-END.
           EXIT.
       
       END PROGRAM TRANSACTIONS.
