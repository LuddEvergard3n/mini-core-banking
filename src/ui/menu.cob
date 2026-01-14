      ******************************************************************
      * MENU.COB - Menu Logic and Navigation
      * 
      * Handles menu display and navigation flow.
      * Coordinates between UI, input, and core modules.
      *
      * Design decisions:
      * - State-based menu system
      * - Clear separation of concerns
      * - Error handling at each step
      * - Confirmation for destructive operations
      *
      * Menu structure:
      * - Main menu (account selection)
      * - Operations menu (once account selected)
      * - Transaction submenu (for each operation)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENU.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY 'src/storage/schema.cob'.
       
       01  WS-MENU-STATE.
           05  WS-CURRENT-STATE        PIC X(20).
               88  STATE-MAIN          VALUE 'MAIN'.
               88  STATE-OPERATIONS    VALUE 'OPERATIONS'.
               88  STATE-CREATE-ACC    VALUE 'CREATE_ACCOUNT'.
               88  STATE-CREDIT        VALUE 'CREDIT'.
               88  STATE-DEBIT         VALUE 'DEBIT'.
               88  STATE-BLOCK         VALUE 'BLOCK'.
               88  STATE-VIEW-LEDGER   VALUE 'VIEW_LEDGER'.
               88  STATE-EXIT          VALUE 'EXIT'.
           05  WS-PREVIOUS-STATE       PIC X(20).
       
       01  WS-CURRENT-ACCOUNT-ID       PIC 9(8) VALUE 0.
       01  WS-ACCOUNT-LOADED           PIC 9 VALUE 0.
           88  ACCOUNT-IS-LOADED       VALUE 1.
           88  NO-ACCOUNT-LOADED       VALUE 0.
       
       01  WS-USER-CHOICE              PIC 99.
       01  WS-USER-INPUT-VALID         PIC 9.
       01  WS-OPERATION-STATUS         PIC XX.
       01  WS-STATUS-MESSAGE           PIC X(80).
       
      * Transaction input data
       01  WS-TXN-INPUT.
           05  WS-TXN-AMOUNT           PIC S9(13)V99.
           05  WS-TXN-DESCRIPTION      PIC X(80).
       
      * Formatted display values
       01  WS-DISPLAY-BALANCE          PIC -ZZZ,ZZZ,ZZ9.99.
       01  WS-DISPLAY-BLOCKED          PIC -ZZZ,ZZZ,ZZ9.99.
       01  WS-DISPLAY-AVAILABLE        PIC -ZZZ,ZZZ,ZZ9.99.
       
       PROCEDURE DIVISION.
       
      ******************************************************************
      * MAIN MENU LOOP
      ******************************************************************
       
       MAIN-MENU-LOOP.
      *    Main application loop
           MOVE 'MAIN' TO WS-CURRENT-STATE.
           
           PERFORM UNTIL STATE-EXIT
               
               EVALUATE TRUE
                   WHEN STATE-MAIN
                       PERFORM DISPLAY-MAIN-MENU
                       PERFORM HANDLE-MAIN-MENU-CHOICE
                   
                   WHEN STATE-OPERATIONS
                       PERFORM DISPLAY-OPERATIONS-MENU
                       PERFORM HANDLE-OPERATIONS-CHOICE
                   
                   WHEN STATE-CREATE-ACC
                       PERFORM HANDLE-CREATE-ACCOUNT
                   
                   WHEN STATE-CREDIT
                       PERFORM HANDLE-CREDIT-TRANSACTION
                   
                   WHEN STATE-DEBIT
                       PERFORM HANDLE-DEBIT-TRANSACTION
                   
                   WHEN STATE-BLOCK
                       PERFORM HANDLE-BLOCK-FUNDS
                   
                   WHEN STATE-VIEW-LEDGER
                       PERFORM HANDLE-VIEW-LEDGER
               END-EVALUATE
           END-PERFORM.
           
           DISPLAY 'System shutting down...'.
           STOP RUN.
       
      ******************************************************************
      * MAIN MENU
      ******************************************************************
       
       DISPLAY-MAIN-MENU.
      *    Display main menu screen
           CALL 'SCREENS' USING 'CLEAR-SCREEN'.
           CALL 'SCREENS' USING 'RENDER-HEADER'.
           
           DISPLAY ' '.
           DISPLAY 'MAIN MENU'.
           DISPLAY ' '.
           DISPLAY '  1 - Load Account'.
           DISPLAY '  2 - Create New Account'.
           DISPLAY '  3 - Exit System'.
           DISPLAY ' '.
           DISPLAY 'Enter option: ' WITH NO ADVANCING.
       
       HANDLE-MAIN-MENU-CHOICE.
      *    Handle main menu choice
           CALL 'INPUT' USING 'GET-MENU-CHOICE' 
               WS-USER-CHOICE WS-USER-INPUT-VALID.
           
           IF WS-USER-INPUT-VALID = 0
               DISPLAY 'Invalid input. Press ENTER to continue...'
               CALL 'INPUT' USING 'WAIT-FOR-ENTER'
               GO TO HANDLE-MAIN-MENU-CHOICE-END
           END-IF.
           
           EVALUATE WS-USER-CHOICE
               WHEN 1
                   PERFORM LOAD-ACCOUNT
                   IF ACCOUNT-IS-LOADED
                       MOVE 'OPERATIONS' TO WS-CURRENT-STATE
                   END-IF
               
               WHEN 2
                   MOVE 'CREATE_ACCOUNT' TO WS-CURRENT-STATE
               
               WHEN 3
                   MOVE 'EXIT' TO WS-CURRENT-STATE
               
               WHEN OTHER
                   DISPLAY 'Invalid option. Press ENTER to continue...'
                   CALL 'INPUT' USING 'WAIT-FOR-ENTER'
           END-EVALUATE.
           
       HANDLE-MAIN-MENU-CHOICE-END.
           EXIT.
       
      ******************************************************************
      * ACCOUNT LOADING
      ******************************************************************
       
       LOAD-ACCOUNT.
      *    Load account for operations
           DISPLAY ' '.
           DISPLAY 'Load Account'.
           DISPLAY '------------'.
           DISPLAY 'Enter Account ID: ' WITH NO ADVANCING.
           
           CALL 'INPUT' USING 'GET-ACCOUNT-ID' 
               WS-CURRENT-ACCOUNT-ID WS-USER-INPUT-VALID.
           
           IF WS-USER-INPUT-VALID = 0
               DISPLAY 'Invalid account ID format.'
               DISPLAY 'Press ENTER to continue...'
               CALL 'INPUT' USING 'WAIT-FOR-ENTER'
               MOVE 0 TO WS-ACCOUNT-LOADED
               GO TO LOAD-ACCOUNT-END
           END-IF.
           
      *    Try to load account from storage
           CALL 'ACCOUNT' USING 'GET-ACCOUNT' 
               WS-CURRENT-ACCOUNT-ID ACCOUNT-RECORD 
               WS-OPERATION-STATUS.
           
           IF WS-OPERATION-STATUS = '00'
               MOVE 1 TO WS-ACCOUNT-LOADED
               DISPLAY 'Account loaded successfully.'
               DISPLAY 'Press ENTER to continue...'
               CALL 'INPUT' USING 'WAIT-FOR-ENTER'
           ELSE
               MOVE 0 TO WS-ACCOUNT-LOADED
               DISPLAY 'Account not found.'
               DISPLAY 'Press ENTER to continue...'
               CALL 'INPUT' USING 'WAIT-FOR-ENTER'
           END-IF.
           
       LOAD-ACCOUNT-END.
           EXIT.
       
      ******************************************************************
      * OPERATIONS MENU
      ******************************************************************
       
       DISPLAY-OPERATIONS-MENU.
      *    Display operations menu with account info
           CALL 'SCREENS' USING 'CLEAR-SCREEN'.
           CALL 'SCREENS' USING 'RENDER-HEADER'.
           
      *    Reload account to get current balances
           CALL 'ACCOUNT' USING 'GET-ACCOUNT' 
               WS-CURRENT-ACCOUNT-ID ACCOUNT-RECORD 
               WS-OPERATION-STATUS.
           
      *    Format balances for display
           MOVE ACC-BALANCE TO WS-DISPLAY-BALANCE.
           MOVE ACC-BLOCKED-AMT TO WS-DISPLAY-BLOCKED.
           COMPUTE WS-DISPLAY-AVAILABLE = 
               ACC-BALANCE - ACC-BLOCKED-AMT.
           
      *    Display account info
           DISPLAY ' '.
           DISPLAY 'OPERATIONS MENU'.
           DISPLAY ' '.
           DISPLAY 'Account ID:      ' ACC-ID.
           DISPLAY 'Holder:          ' ACC-HOLDER-NAME.
           
           IF ACC-STATUS = 'ACTIVE'
               DISPLAY 'Status:          ACTIVE'
           ELSE IF ACC-STATUS = 'BLOCKED'
               DISPLAY 'Status:          BLOCKED'
           ELSE
               DISPLAY 'Status:          ' ACC-STATUS
           END-IF.
           
           DISPLAY 'Balance:      R$ ' WS-DISPLAY-BALANCE.
           DISPLAY 'Blocked:      R$ ' WS-DISPLAY-BLOCKED.
           DISPLAY 'Available:    R$ ' WS-DISPLAY-AVAILABLE.
           DISPLAY ' '.
           DISPLAY '  1 - Credit Account'.
           DISPLAY '  2 - Debit Account'.
           DISPLAY '  3 - Block Funds'.
           DISPLAY '  4 - View Ledger'.
           DISPLAY '  5 - Back to Main Menu'.
           DISPLAY ' '.
           DISPLAY 'Enter option: ' WITH NO ADVANCING.
       
       HANDLE-OPERATIONS-CHOICE.
      *    Handle operations menu choice
           CALL 'INPUT' USING 'GET-MENU-CHOICE' 
               WS-USER-CHOICE WS-USER-INPUT-VALID.
           
           IF WS-USER-INPUT-VALID = 0
               DISPLAY 'Invalid input. Press ENTER to continue...'
               CALL 'INPUT' USING 'WAIT-FOR-ENTER'
               GO TO HANDLE-OPERATIONS-CHOICE-END
           END-IF.
           
           EVALUATE WS-USER-CHOICE
               WHEN 1
                   MOVE 'CREDIT' TO WS-CURRENT-STATE
               
               WHEN 2
                   MOVE 'DEBIT' TO WS-CURRENT-STATE
               
               WHEN 3
                   MOVE 'BLOCK' TO WS-CURRENT-STATE
               
               WHEN 4
                   MOVE 'VIEW_LEDGER' TO WS-CURRENT-STATE
               
               WHEN 5
                   MOVE 0 TO WS-ACCOUNT-LOADED
                   MOVE 'MAIN' TO WS-CURRENT-STATE
               
               WHEN OTHER
                   DISPLAY 'Invalid option. Press ENTER to continue...'
                   CALL 'INPUT' USING 'WAIT-FOR-ENTER'
           END-EVALUATE.
           
       HANDLE-OPERATIONS-CHOICE-END.
           EXIT.
       
      ******************************************************************
      * CREATE ACCOUNT
      ******************************************************************
       
       HANDLE-CREATE-ACCOUNT.
      *    Handle account creation
           CALL 'SCREENS' USING 'CLEAR-SCREEN'.
           CALL 'SCREENS' USING 'RENDER-HEADER'.
           
           DISPLAY ' '.
           DISPLAY 'CREATE NEW ACCOUNT'.
           DISPLAY '------------------'.
           DISPLAY ' '.
           
           DISPLAY 'Enter holder name: ' WITH NO ADVANCING.
           CALL 'INPUT' USING 'GET-TEXT-INPUT' 
               WS-TXN-DESCRIPTION WS-USER-INPUT-VALID.
           
           IF WS-USER-INPUT-VALID = 0
               DISPLAY 'Invalid name. Operation cancelled.'
               DISPLAY 'Press ENTER to continue...'
               CALL 'INPUT' USING 'WAIT-FOR-ENTER'
               MOVE 'MAIN' TO WS-CURRENT-STATE
               GO TO HANDLE-CREATE-ACCOUNT-END
           END-IF.
           
      *    Create account with CHECKING type
           CALL 'ACCOUNT' USING 'CREATE-ACCOUNT' 
               WS-TXN-DESCRIPTION 'CHECKING' 
               ACCOUNT-RECORD WS-OPERATION-STATUS.
           
           IF WS-OPERATION-STATUS = '00'
               DISPLAY ' '
               DISPLAY 'Account created successfully!'
               DISPLAY 'Account ID: ' ACC-ID
               DISPLAY ' '
               DISPLAY 'Press ENTER to continue...'
               CALL 'INPUT' USING 'WAIT-FOR-ENTER'
           ELSE
               DISPLAY ' '
               DISPLAY 'Failed to create account.'
               DISPLAY 'Press ENTER to continue...'
               CALL 'INPUT' USING 'WAIT-FOR-ENTER'
           END-IF.
           
           MOVE 'MAIN' TO WS-CURRENT-STATE.
           
       HANDLE-CREATE-ACCOUNT-END.
           EXIT.
       
      ******************************************************************
      * CREDIT TRANSACTION
      ******************************************************************
       
       HANDLE-CREDIT-TRANSACTION.
      *    Handle credit transaction
           CALL 'SCREENS' USING 'CLEAR-SCREEN'.
           CALL 'SCREENS' USING 'RENDER-HEADER'.
           
           DISPLAY ' '.
           DISPLAY 'CREDIT ACCOUNT'.
           DISPLAY '--------------'.
           DISPLAY ' '.
           
           DISPLAY 'Enter amount: ' WITH NO ADVANCING.
           CALL 'INPUT' USING 'GET-AMOUNT' 
               WS-TXN-AMOUNT WS-USER-INPUT-VALID.
           
           IF WS-USER-INPUT-VALID = 0
               DISPLAY 'Invalid amount. Operation cancelled.'
               DISPLAY 'Press ENTER to continue...'
               CALL 'INPUT' USING 'WAIT-FOR-ENTER'
               MOVE 'OPERATIONS' TO WS-CURRENT-STATE
               GO TO HANDLE-CREDIT-TRANSACTION-END
           END-IF.
           
           DISPLAY 'Enter description: ' WITH NO ADVANCING.
           CALL 'INPUT' USING 'GET-TEXT-INPUT' 
               WS-TXN-DESCRIPTION WS-USER-INPUT-VALID.
           
      *    Process transaction
           CALL 'TRANSACTIONS' USING 'PROCESS-CREDIT'
               WS-CURRENT-ACCOUNT-ID WS-TXN-AMOUNT 
               WS-TXN-DESCRIPTION TRANSACTION-RECORD 
               WS-OPERATION-STATUS.
           
           IF WS-OPERATION-STATUS = '00'
               DISPLAY ' '
               DISPLAY 'Credit processed successfully!'
               DISPLAY 'Transaction ID: ' TXN-ID
           ELSE
               DISPLAY ' '
               DISPLAY 'Credit failed: ' TXN-DESCRIPTION
           END-IF.
           
           DISPLAY ' '
           DISPLAY 'Press ENTER to continue...'
           CALL 'INPUT' USING 'WAIT-FOR-ENTER'.
           
           MOVE 'OPERATIONS' TO WS-CURRENT-STATE.
           
       HANDLE-CREDIT-TRANSACTION-END.
           EXIT.
       
      ******************************************************************
      * DEBIT TRANSACTION
      ******************************************************************
       
       HANDLE-DEBIT-TRANSACTION.
      *    Handle debit transaction
           CALL 'SCREENS' USING 'CLEAR-SCREEN'.
           CALL 'SCREENS' USING 'RENDER-HEADER'.
           
           DISPLAY ' '.
           DISPLAY 'DEBIT ACCOUNT'.
           DISPLAY '-------------'.
           DISPLAY ' '.
           
           DISPLAY 'Enter amount: ' WITH NO ADVANCING.
           CALL 'INPUT' USING 'GET-AMOUNT' 
               WS-TXN-AMOUNT WS-USER-INPUT-VALID.
           
           IF WS-USER-INPUT-VALID = 0
               DISPLAY 'Invalid amount. Operation cancelled.'
               DISPLAY 'Press ENTER to continue...'
               CALL 'INPUT' USING 'WAIT-FOR-ENTER'
               MOVE 'OPERATIONS' TO WS-CURRENT-STATE
               GO TO HANDLE-DEBIT-TRANSACTION-END
           END-IF.
           
           DISPLAY 'Enter description: ' WITH NO ADVANCING.
           CALL 'INPUT' USING 'GET-TEXT-INPUT' 
               WS-TXN-DESCRIPTION WS-USER-INPUT-VALID.
           
      *    Process transaction
           CALL 'TRANSACTIONS' USING 'PROCESS-DEBIT'
               WS-CURRENT-ACCOUNT-ID WS-TXN-AMOUNT 
               WS-TXN-DESCRIPTION TRANSACTION-RECORD 
               WS-OPERATION-STATUS.
           
           IF WS-OPERATION-STATUS = '00'
               DISPLAY ' '
               DISPLAY 'Debit processed successfully!'
               DISPLAY 'Transaction ID: ' TXN-ID
           ELSE
               DISPLAY ' '
               DISPLAY 'Debit failed: ' TXN-DESCRIPTION
           END-IF.
           
           DISPLAY ' '
           DISPLAY 'Press ENTER to continue...'
           CALL 'INPUT' USING 'WAIT-FOR-ENTER'.
           
           MOVE 'OPERATIONS' TO WS-CURRENT-STATE.
           
       HANDLE-DEBIT-TRANSACTION-END.
           EXIT.
       
      ******************************************************************
      * BLOCK FUNDS
      ******************************************************************
       
       HANDLE-BLOCK-FUNDS.
      *    Handle fund blocking
           CALL 'SCREENS' USING 'CLEAR-SCREEN'.
           CALL 'SCREENS' USING 'RENDER-HEADER'.
           
           DISPLAY ' '.
           DISPLAY 'BLOCK FUNDS'.
           DISPLAY '-----------'.
           DISPLAY ' '.
           
           DISPLAY 'Enter amount to block: ' WITH NO ADVANCING.
           CALL 'INPUT' USING 'GET-AMOUNT' 
               WS-TXN-AMOUNT WS-USER-INPUT-VALID.
           
           IF WS-USER-INPUT-VALID = 0
               DISPLAY 'Invalid amount. Operation cancelled.'
               DISPLAY 'Press ENTER to continue...'
               CALL 'INPUT' USING 'WAIT-FOR-ENTER'
               MOVE 'OPERATIONS' TO WS-CURRENT-STATE
               GO TO HANDLE-BLOCK-FUNDS-END
           END-IF.
           
           DISPLAY 'Enter description: ' WITH NO ADVANCING.
           CALL 'INPUT' USING 'GET-TEXT-INPUT' 
               WS-TXN-DESCRIPTION WS-USER-INPUT-VALID.
           
      *    Process block
           CALL 'TRANSACTIONS' USING 'PROCESS-BLOCK-FUNDS'
               WS-CURRENT-ACCOUNT-ID WS-TXN-AMOUNT 
               WS-TXN-DESCRIPTION TRANSACTION-RECORD 
               WS-OPERATION-STATUS.
           
           IF WS-OPERATION-STATUS = '00'
               DISPLAY ' '
               DISPLAY 'Funds blocked successfully!'
               DISPLAY 'Transaction ID: ' TXN-ID
           ELSE
               DISPLAY ' '
               DISPLAY 'Block failed: ' TXN-DESCRIPTION
           END-IF.
           
           DISPLAY ' '
           DISPLAY 'Press ENTER to continue...'
           CALL 'INPUT' USING 'WAIT-FOR-ENTER'.
           
           MOVE 'OPERATIONS' TO WS-CURRENT-STATE.
           
       HANDLE-BLOCK-FUNDS-END.
           EXIT.
       
      ******************************************************************
      * VIEW LEDGER
      ******************************************************************
       
       HANDLE-VIEW-LEDGER.
      *    Display ledger entries for current account
           CALL 'SCREENS' USING 'CLEAR-SCREEN'.
           CALL 'SCREENS' USING 'RENDER-HEADER'.
           
           DISPLAY ' '.
           DISPLAY 'TRANSACTION LEDGER'.
           DISPLAY '------------------'.
           DISPLAY ' '.
           DISPLAY 'Feature not yet implemented.'.
           DISPLAY ' '.
           DISPLAY 'Press ENTER to continue...'
           CALL 'INPUT' USING 'WAIT-FOR-ENTER'.
           
           MOVE 'OPERATIONS' TO WS-CURRENT-STATE.
       
       END PROGRAM MENU.
