      ******************************************************************
      * ACCOUNT.COB - Account Management Module
      * 
      * Handles account creation, retrieval, and updates.
      * Enforces business rules through validation module.
      *
      * Design decisions:
      * - Account IDs generated sequentially from control file
      * - All balance changes must go through ledger
      * - Account status changes are audited
      * - Timestamps in YYYYMMDDHHMMSS format (UTC)
      *
      * Operations:
      * - CREATE-ACCOUNT: Creates new account with initial balance 0
      * - GET-ACCOUNT: Retrieves account by ID
      * - UPDATE-ACCOUNT-BALANCE: Updates balance (via transactions)
      * - BLOCK-ACCOUNT: Changes status to BLOCKED
      * - UNBLOCK-ACCOUNT: Changes status to ACTIVE
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCOUNT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY 'src/storage/schema.cob'.
       
       01  WS-ACCOUNT-OPERATION.
           05  WS-ACC-OP-STATUS        PIC XX.
               88  ACC-OP-SUCCESS      VALUE '00'.
               88  ACC-OP-NOT-FOUND    VALUE '23'.
               88  ACC-OP-DUPLICATE    VALUE '22'.
               88  ACC-OP-INVALID      VALUE '99'.
           05  WS-ACC-OP-MESSAGE       PIC X(80).
       
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
       
      * Input parameters for operations
       01  WS-INPUT-HOLDER-NAME        PIC X(30).
       01  WS-INPUT-ACCOUNT-TYPE       PIC X(10).
       01  WS-INPUT-ACCOUNT-ID         PIC 9(8).
       01  WS-INPUT-AMOUNT             PIC S9(13)V99.
       
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
       
       GENERATE-ACCOUNT-ID.
      *    Generate next account ID from control file
      *    Output: ACC-ID in ACCOUNT-RECORD
           CALL 'FILES' USING 'READ-CONTROL' CONTROL-RECORD 
               WS-ACC-OP-STATUS.
           
           IF NOT ACC-OP-SUCCESS
               DISPLAY 'FATAL: Cannot read control file'
               STOP RUN
           END-IF.
           
           ADD 1 TO CTL-LAST-ACCOUNT-ID.
           MOVE CTL-LAST-ACCOUNT-ID TO ACC-ID.
           
           CALL 'FILES' USING 'UPDATE-CONTROL' CONTROL-RECORD 
               WS-ACC-OP-STATUS.
       
      ******************************************************************
      * ACCOUNT OPERATIONS
      ******************************************************************
       
       CREATE-ACCOUNT.
      *    Create new account with zero initial balance
      *    Input: WS-INPUT-HOLDER-NAME, WS-INPUT-ACCOUNT-TYPE
      *    Output: ACCOUNT-RECORD, WS-ACC-OP-STATUS
           
           INITIALIZE ACCOUNT-RECORD.
           
           PERFORM GENERATE-ACCOUNT-ID.
           
           MOVE WS-INPUT-HOLDER-NAME TO ACC-HOLDER-NAME.
           MOVE WS-INPUT-ACCOUNT-TYPE TO ACC-TYPE.
           MOVE 'ACTIVE' TO ACC-STATUS.
           MOVE 0 TO ACC-BALANCE.
           MOVE 0 TO ACC-BLOCKED-AMT.
           
           PERFORM GET-CURRENT-TIMESTAMP.
           MOVE WS-TIMESTAMP TO ACC-CREATED-TS.
           MOVE WS-TIMESTAMP TO ACC-UPDATED-TS.
           
      *    Open file and write account
           CALL 'FILES' USING 'OPEN-ACCOUNT-FILE-IO'.
           CALL 'FILES' USING 'WRITE-ACCOUNT' ACCOUNT-RECORD 
               WS-ACC-OP-STATUS.
           CALL 'FILES' USING 'CLOSE-ACCOUNT-FILE'.
           
           IF ACC-OP-SUCCESS
               MOVE 'Account created successfully' TO WS-ACC-OP-MESSAGE
           ELSE IF ACC-OP-DUPLICATE
               MOVE 'Account ID already exists' TO WS-ACC-OP-MESSAGE
           ELSE
               MOVE 'Failed to create account' TO WS-ACC-OP-MESSAGE
           END-IF.
       
       GET-ACCOUNT.
      *    Retrieve account by ID
      *    Input: WS-INPUT-ACCOUNT-ID
      *    Output: ACCOUNT-RECORD, WS-ACC-OP-STATUS
           
           MOVE WS-INPUT-ACCOUNT-ID TO ACC-ID.
           
           CALL 'FILES' USING 'OPEN-ACCOUNT-FILE-IO'.
           CALL 'FILES' USING 'READ-ACCOUNT' ACCOUNT-RECORD 
               WS-ACC-OP-STATUS.
           CALL 'FILES' USING 'CLOSE-ACCOUNT-FILE'.
           
           IF ACC-OP-SUCCESS
               MOVE 'Account retrieved successfully' 
                   TO WS-ACC-OP-MESSAGE
           ELSE IF ACC-OP-NOT-FOUND
               MOVE 'Account not found' TO WS-ACC-OP-MESSAGE
           ELSE
               MOVE 'Failed to retrieve account' TO WS-ACC-OP-MESSAGE
           END-IF.
       
       UPDATE-ACCOUNT-BALANCE.
      *    Update account balance and blocked amount
      *    This is called by transaction processing only
      *    Input: ACCOUNT-RECORD with updated balances
      *    Output: WS-ACC-OP-STATUS
           
           PERFORM GET-CURRENT-TIMESTAMP.
           MOVE WS-TIMESTAMP TO ACC-UPDATED-TS.
           
           CALL 'FILES' USING 'OPEN-ACCOUNT-FILE-IO'.
           CALL 'FILES' USING 'UPDATE-ACCOUNT' ACCOUNT-RECORD 
               WS-ACC-OP-STATUS.
           CALL 'FILES' USING 'CLOSE-ACCOUNT-FILE'.
           
           IF ACC-OP-SUCCESS
               MOVE 'Account balance updated' TO WS-ACC-OP-MESSAGE
           ELSE
               MOVE 'Failed to update account balance' 
                   TO WS-ACC-OP-MESSAGE
           END-IF.
       
       BLOCK-ACCOUNT.
      *    Block account (prevent transactions)
      *    Input: WS-INPUT-ACCOUNT-ID
      *    Output: WS-ACC-OP-STATUS
           
           MOVE WS-INPUT-ACCOUNT-ID TO ACC-ID.
           
           CALL 'FILES' USING 'OPEN-ACCOUNT-FILE-IO'.
           CALL 'FILES' USING 'READ-ACCOUNT' ACCOUNT-RECORD 
               WS-ACC-OP-STATUS.
           
           IF NOT ACC-OP-SUCCESS
               CALL 'FILES' USING 'CLOSE-ACCOUNT-FILE'
               MOVE 'Account not found' TO WS-ACC-OP-MESSAGE
               GO TO BLOCK-ACCOUNT-END
           END-IF.
           
           IF ACC-STATUS = 'BLOCKED'
               CALL 'FILES' USING 'CLOSE-ACCOUNT-FILE'
               MOVE 'Account already blocked' TO WS-ACC-OP-MESSAGE
               MOVE '00' TO WS-ACC-OP-STATUS
               GO TO BLOCK-ACCOUNT-END
           END-IF.
           
           MOVE 'BLOCKED' TO ACC-STATUS.
           PERFORM GET-CURRENT-TIMESTAMP.
           MOVE WS-TIMESTAMP TO ACC-UPDATED-TS.
           
           CALL 'FILES' USING 'UPDATE-ACCOUNT' ACCOUNT-RECORD 
               WS-ACC-OP-STATUS.
           CALL 'FILES' USING 'CLOSE-ACCOUNT-FILE'.
           
           MOVE 'Account blocked successfully' TO WS-ACC-OP-MESSAGE.
           
       BLOCK-ACCOUNT-END.
           EXIT.
       
       UNBLOCK-ACCOUNT.
      *    Unblock account (allow transactions)
      *    Input: WS-INPUT-ACCOUNT-ID
      *    Output: WS-ACC-OP-STATUS
           
           MOVE WS-INPUT-ACCOUNT-ID TO ACC-ID.
           
           CALL 'FILES' USING 'OPEN-ACCOUNT-FILE-IO'.
           CALL 'FILES' USING 'READ-ACCOUNT' ACCOUNT-RECORD 
               WS-ACC-OP-STATUS.
           
           IF NOT ACC-OP-SUCCESS
               CALL 'FILES' USING 'CLOSE-ACCOUNT-FILE'
               MOVE 'Account not found' TO WS-ACC-OP-MESSAGE
               GO TO UNBLOCK-ACCOUNT-END
           END-IF.
           
           IF ACC-STATUS = 'ACTIVE'
               CALL 'FILES' USING 'CLOSE-ACCOUNT-FILE'
               MOVE 'Account already active' TO WS-ACC-OP-MESSAGE
               MOVE '00' TO WS-ACC-OP-STATUS
               GO TO UNBLOCK-ACCOUNT-END
           END-IF.
           
           IF ACC-STATUS = 'CLOSED'
               CALL 'FILES' USING 'CLOSE-ACCOUNT-FILE'
               MOVE 'Cannot unblock closed account' TO WS-ACC-OP-MESSAGE
               MOVE '99' TO WS-ACC-OP-STATUS
               GO TO UNBLOCK-ACCOUNT-END
           END-IF.
           
           MOVE 'ACTIVE' TO ACC-STATUS.
           PERFORM GET-CURRENT-TIMESTAMP.
           MOVE WS-TIMESTAMP TO ACC-UPDATED-TS.
           
           CALL 'FILES' USING 'UPDATE-ACCOUNT' ACCOUNT-RECORD 
               WS-ACC-OP-STATUS.
           CALL 'FILES' USING 'CLOSE-ACCOUNT-FILE'.
           
           MOVE 'Account unblocked successfully' TO WS-ACC-OP-MESSAGE.
           
       UNBLOCK-ACCOUNT-END.
           EXIT.
       
       GET-AVAILABLE-BALANCE.
      *    Calculate available balance (balance - blocked)
      *    Input: ACCOUNT-RECORD
      *    Output: WS-INPUT-AMOUNT (available balance)
           COMPUTE WS-INPUT-AMOUNT = 
               ACC-BALANCE - ACC-BLOCKED-AMT.
       
       END PROGRAM ACCOUNT.
