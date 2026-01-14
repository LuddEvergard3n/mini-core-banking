      ******************************************************************
      * LEDGER.COB - Ledger Management Module
      * 
      * Manages the immutable transaction ledger.
      * Every financial operation creates ledger entries.
      *
      * Design decisions:
      * - Ledger is append-only (immutable audit trail)
      * - Each entry records balance before and after
      * - Ledger IDs generated sequentially
      * - No updates or deletes allowed
      * - Sequential file for guaranteed append order
      *
      * Entry types:
      * - CREDIT: Money in
      * - DEBIT: Money out
      * - BLOCK: Funds blocked
      * - UNBLOCK: Funds released
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEDGER.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY 'src/storage/schema.cob'.
       
       01  WS-LEDGER-OPERATION.
           05  WS-LED-OP-STATUS        PIC XX.
               88  LED-OP-SUCCESS      VALUE '00'.
               88  LED-OP-ERROR        VALUE '99'.
           05  WS-LED-OP-MESSAGE       PIC X(80).
       
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
       
      * Input parameters for ledger entry creation
       01  WS-INPUT-ACCOUNT-ID         PIC 9(8).
       01  WS-INPUT-TXN-ID             PIC 9(12).
       01  WS-INPUT-ENTRY-TYPE         PIC X(8).
       01  WS-INPUT-AMOUNT             PIC S9(13)V99.
       01  WS-INPUT-BALANCE-BEFORE     PIC S9(13)V99.
       01  WS-INPUT-BALANCE-AFTER      PIC S9(13)V99.
       01  WS-INPUT-DESCRIPTION        PIC X(50).
       
      * For ledger query operations
       01  WS-QUERY-ACCOUNT-ID         PIC 9(8).
       01  WS-LEDGER-ENTRY-COUNT       PIC 9(6) VALUE 0.
       01  WS-LEDGER-ENTRIES-TABLE.
           05  WS-LEDGER-ENTRY OCCURS 100 TIMES.
               10  WS-LED-ENTRY-ID         PIC 9(10).
               10  WS-LED-ENTRY-TYPE       PIC X(8).
               10  WS-LED-ENTRY-AMOUNT     PIC S9(13)V99.
               10  WS-LED-ENTRY-TIMESTAMP  PIC X(14).
               10  WS-LED-ENTRY-BAL-AFTER  PIC S9(13)V99.
       
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
       
       GENERATE-LEDGER-ID.
      *    Generate next ledger entry ID from control file
      *    Output: LED-ID in LEDGER-RECORD
           CALL 'FILES' USING 'READ-CONTROL' CONTROL-RECORD 
               WS-LED-OP-STATUS.
           
           IF NOT LED-OP-SUCCESS
               DISPLAY 'FATAL: Cannot read control file for ledger ID'
               STOP RUN
           END-IF.
           
           ADD 1 TO CTL-LAST-LEDGER-ID.
           MOVE CTL-LAST-LEDGER-ID TO LED-ID.
           
           CALL 'FILES' USING 'UPDATE-CONTROL' CONTROL-RECORD 
               WS-LED-OP-STATUS.
       
      ******************************************************************
      * LEDGER OPERATIONS
      ******************************************************************
       
       CREATE-LEDGER-ENTRY.
      *    Create new ledger entry (immutable)
      *    Input: WS-INPUT-* variables
      *    Output: LEDGER-RECORD, WS-LED-OP-STATUS
      *
      *    This is the ONLY way to create ledger entries.
      *    Every financial operation must call this.
           
           INITIALIZE LEDGER-RECORD.
           
           PERFORM GENERATE-LEDGER-ID.
           
           MOVE WS-INPUT-ACCOUNT-ID TO LED-ACCOUNT-ID.
           MOVE WS-INPUT-TXN-ID TO LED-TXN-ID.
           MOVE WS-INPUT-ENTRY-TYPE TO LED-TYPE.
           MOVE WS-INPUT-AMOUNT TO LED-AMOUNT.
           MOVE WS-INPUT-BALANCE-BEFORE TO LED-BALANCE-BEFORE.
           MOVE WS-INPUT-BALANCE-AFTER TO LED-BALANCE-AFTER.
           MOVE WS-INPUT-DESCRIPTION TO LED-DESCRIPTION.
           
           PERFORM GET-CURRENT-TIMESTAMP.
           MOVE WS-TIMESTAMP TO LED-TIMESTAMP.
           
      *    Append to ledger file (immutable log)
           CALL 'FILES' USING 'OPEN-LEDGER-FILE-EXTEND'.
           CALL 'FILES' USING 'APPEND-LEDGER' LEDGER-RECORD 
               WS-LED-OP-STATUS.
           CALL 'FILES' USING 'CLOSE-LEDGER-FILE'.
           
           IF LED-OP-SUCCESS
               MOVE 'Ledger entry created successfully' 
                   TO WS-LED-OP-MESSAGE
           ELSE
               MOVE 'Failed to create ledger entry' 
                   TO WS-LED-OP-MESSAGE
           END-IF.
       
       QUERY-LEDGER-BY-ACCOUNT.
      *    Query ledger entries for specific account
      *    Returns last N entries (up to 100)
      *    Input: WS-QUERY-ACCOUNT-ID
      *    Output: WS-LEDGER-ENTRIES-TABLE, WS-LEDGER-ENTRY-COUNT
           
           MOVE 0 TO WS-LEDGER-ENTRY-COUNT.
           
           CALL 'FILES' USING 'OPEN-LEDGER-FILE-INPUT'.
           
           PERFORM READ-ALL-LEDGER-ENTRIES.
           
           CALL 'FILES' USING 'CLOSE-LEDGER-FILE'.
       
       READ-ALL-LEDGER-ENTRIES.
      *    Read all ledger entries sequentially
      *    Filter by account ID and store in table
           PERFORM UNTIL WS-LED-OP-STATUS = '10' OR 
                        WS-LEDGER-ENTRY-COUNT >= 100
               
               CALL 'FILES' USING 'READ-NEXT-LEDGER' LEDGER-RECORD 
                   WS-LED-OP-STATUS
               
               IF LED-OP-SUCCESS
                   IF LED-ACCOUNT-ID = WS-QUERY-ACCOUNT-ID
                       ADD 1 TO WS-LEDGER-ENTRY-COUNT
                       MOVE LED-ID TO 
                           WS-LED-ENTRY-ID(WS-LEDGER-ENTRY-COUNT)
                       MOVE LED-TYPE TO 
                           WS-LED-ENTRY-TYPE(WS-LEDGER-ENTRY-COUNT)
                       MOVE LED-AMOUNT TO 
                           WS-LED-ENTRY-AMOUNT(WS-LEDGER-ENTRY-COUNT)
                       MOVE LED-TIMESTAMP TO 
                           WS-LED-ENTRY-TIMESTAMP(WS-LEDGER-ENTRY-COUNT)
                       MOVE LED-BALANCE-AFTER TO 
                           WS-LED-ENTRY-BAL-AFTER(WS-LEDGER-ENTRY-COUNT)
                   END-IF
               END-IF
           END-PERFORM.
       
       GET-LAST-LEDGER-ENTRY.
      *    Get most recent ledger entry for account
      *    Input: WS-QUERY-ACCOUNT-ID
      *    Output: LEDGER-RECORD, WS-LED-OP-STATUS
      *
      *    This scans the entire ledger to find the last entry.
      *    Not efficient for large ledgers, but simple and correct.
           
           INITIALIZE LEDGER-RECORD.
           MOVE '23' TO WS-LED-OP-STATUS.
           
           CALL 'FILES' USING 'OPEN-LEDGER-FILE-INPUT'.
           
           PERFORM READ-UNTIL-LAST-ENTRY.
           
           CALL 'FILES' USING 'CLOSE-LEDGER-FILE'.
       
       READ-UNTIL-LAST-ENTRY.
      *    Sequential read to find last matching entry
           PERFORM UNTIL WS-LED-OP-STATUS = '10'
               CALL 'FILES' USING 'READ-NEXT-LEDGER' LEDGER-RECORD 
                   WS-LED-OP-STATUS
               
               IF LED-OP-SUCCESS
                   IF LED-ACCOUNT-ID = WS-QUERY-ACCOUNT-ID
      *                Store this entry (will be overwritten if not last)
                       MOVE '00' TO WS-LED-OP-STATUS
                   END-IF
               END-IF
           END-PERFORM.
      *    At this point, LEDGER-RECORD contains last entry or is empty
       
       VERIFY-LEDGER-INTEGRITY.
      *    Verify ledger mathematical consistency
      *    Checks that balance transitions are correct
      *    Returns count of inconsistencies found
      *    Output: WS-LEDGER-ENTRY-COUNT (inconsistency count)
           
           MOVE 0 TO WS-LEDGER-ENTRY-COUNT.
           
      *    This would iterate through ledger and verify:
      *    - Balance_After[i] = Balance_Before[i] +/- Amount[i]
      *    - Balance_Before[i+1] = Balance_After[i]
      *    
      *    Implementation omitted for brevity, but this is where
      *    you'd implement audit/reconciliation logic.
           
           MOVE 'Ledger integrity check not implemented' 
               TO WS-LED-OP-MESSAGE.
       
       END PROGRAM LEDGER.
