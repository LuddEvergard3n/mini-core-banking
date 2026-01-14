      ******************************************************************
      * FILES.COB - File-Based Persistence Layer
      * 
      * Handles all file I/O operations for the banking system.
      * Uses sequential and indexed files for different data types.
      *
      * Design decisions:
      * - Accounts: indexed by account ID for fast lookup
      * - Ledger: sequential append-only (immutable log)
      * - Transactions: indexed for query performance
      * - Control: single record for sequence generation
      *
      * All file operations return status codes:
      * 00 = success, 10 = end of file, 23 = not found, others = error
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILES.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE
               ASSIGN TO "data/accounts.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACC-ID
               FILE STATUS IS ACCOUNT-FILE-STATUS.
           
           SELECT LEDGER-FILE
               ASSIGN TO "data/ledger.dat"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS LEDGER-FILE-STATUS.
           
           SELECT TRANSACTION-FILE
               ASSIGN TO "data/transactions.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TXN-ID
               ALTERNATE RECORD KEY IS TXN-ACCOUNT-ID WITH DUPLICATES
               FILE STATUS IS TRANSACTION-FILE-STATUS.
           
           SELECT CONTROL-FILE
               ASSIGN TO "data/control.dat"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS CONTROL-FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       01  ACCOUNT-FILE-REC            PIC X(100).
       
       FD  LEDGER-FILE.
       01  LEDGER-FILE-REC             PIC X(120).
       
       FD  TRANSACTION-FILE.
       01  TRANSACTION-FILE-REC        PIC X(150).
       
       FD  CONTROL-FILE.
       01  CONTROL-FILE-REC            PIC X(80).
       
       WORKING-STORAGE SECTION.
       COPY 'src/storage/schema.cob'.
       
       01  ACCOUNT-FILE-STATUS         PIC XX.
       01  LEDGER-FILE-STATUS          PIC XX.
       01  TRANSACTION-FILE-STATUS     PIC XX.
       01  CONTROL-FILE-STATUS         PIC XX.
       
       01  WS-OPERATION-STATUS         PIC XX.
           88  OP-SUCCESS              VALUE '00'.
           88  OP-EOF                  VALUE '10'.
           88  OP-NOT-FOUND            VALUE '23'.
           88  OP-DUPLICATE            VALUE '22'.
       
       PROCEDURE DIVISION.
       
      ******************************************************************
      * ACCOUNT FILE OPERATIONS
      ******************************************************************
       
       INIT-ACCOUNT-FILE.
      *    Initialize account file if it doesn't exist
           OPEN OUTPUT ACCOUNT-FILE.
           IF ACCOUNT-FILE-STATUS NOT = '00'
               DISPLAY 'ERROR: Cannot initialize account file'
               STOP RUN
           END-IF.
           CLOSE ACCOUNT-FILE.
       
       OPEN-ACCOUNT-FILE-IO.
      *    Open account file for read/write
           OPEN I-O ACCOUNT-FILE.
           IF ACCOUNT-FILE-STATUS = '35'
      *        File doesn't exist, create it
               PERFORM INIT-ACCOUNT-FILE
               OPEN I-O ACCOUNT-FILE
           END-IF.
           MOVE ACCOUNT-FILE-STATUS TO WS-OPERATION-STATUS.
       
       CLOSE-ACCOUNT-FILE.
           CLOSE ACCOUNT-FILE.
       
       WRITE-ACCOUNT.
      *    Write account record to file
      *    Input: ACCOUNT-RECORD
      *    Output: WS-OPERATION-STATUS
           WRITE ACCOUNT-FILE-REC FROM ACCOUNT-RECORD
               INVALID KEY
                   MOVE '22' TO WS-OPERATION-STATUS
               NOT INVALID KEY
                   MOVE '00' TO WS-OPERATION-STATUS
           END-WRITE.
       
       READ-ACCOUNT.
      *    Read account by ID
      *    Input: ACC-ID in ACCOUNT-RECORD
      *    Output: ACCOUNT-RECORD, WS-OPERATION-STATUS
           READ ACCOUNT-FILE INTO ACCOUNT-RECORD
               KEY IS ACC-ID
               INVALID KEY
                   MOVE '23' TO WS-OPERATION-STATUS
               NOT INVALID KEY
                   MOVE '00' TO WS-OPERATION-STATUS
           END-READ.
       
       UPDATE-ACCOUNT.
      *    Update existing account record
      *    Input: ACCOUNT-RECORD
      *    Output: WS-OPERATION-STATUS
           REWRITE ACCOUNT-FILE-REC FROM ACCOUNT-RECORD
               INVALID KEY
                   MOVE '23' TO WS-OPERATION-STATUS
               NOT INVALID KEY
                   MOVE '00' TO WS-OPERATION-STATUS
           END-REWRITE.
       
      ******************************************************************
      * LEDGER FILE OPERATIONS
      ******************************************************************
       
       INIT-LEDGER-FILE.
           OPEN OUTPUT LEDGER-FILE.
           IF LEDGER-FILE-STATUS NOT = '00'
               DISPLAY 'ERROR: Cannot initialize ledger file'
               STOP RUN
           END-IF.
           CLOSE LEDGER-FILE.
       
       OPEN-LEDGER-FILE-EXTEND.
      *    Open ledger for append (immutable log)
           OPEN EXTEND LEDGER-FILE.
           IF LEDGER-FILE-STATUS = '35'
               PERFORM INIT-LEDGER-FILE
               OPEN EXTEND LEDGER-FILE
           END-IF.
           MOVE LEDGER-FILE-STATUS TO WS-OPERATION-STATUS.
       
       OPEN-LEDGER-FILE-INPUT.
      *    Open ledger for reading
           OPEN INPUT LEDGER-FILE.
           IF LEDGER-FILE-STATUS = '35'
               PERFORM INIT-LEDGER-FILE
               OPEN INPUT LEDGER-FILE
           END-IF.
           MOVE LEDGER-FILE-STATUS TO WS-OPERATION-STATUS.
       
       CLOSE-LEDGER-FILE.
           CLOSE LEDGER-FILE.
       
       APPEND-LEDGER.
      *    Append ledger entry (immutable)
      *    Input: LEDGER-RECORD
      *    Output: WS-OPERATION-STATUS
           WRITE LEDGER-FILE-REC FROM LEDGER-RECORD.
           MOVE LEDGER-FILE-STATUS TO WS-OPERATION-STATUS.
       
       READ-NEXT-LEDGER.
      *    Read next ledger entry sequentially
      *    Output: LEDGER-RECORD, WS-OPERATION-STATUS
           READ LEDGER-FILE INTO LEDGER-RECORD
               AT END
                   MOVE '10' TO WS-OPERATION-STATUS
               NOT AT END
                   MOVE '00' TO WS-OPERATION-STATUS
           END-READ.
       
      ******************************************************************
      * TRANSACTION FILE OPERATIONS
      ******************************************************************
       
       INIT-TRANSACTION-FILE.
           OPEN OUTPUT TRANSACTION-FILE.
           IF TRANSACTION-FILE-STATUS NOT = '00'
               DISPLAY 'ERROR: Cannot initialize transaction file'
               STOP RUN
           END-IF.
           CLOSE TRANSACTION-FILE.
       
       OPEN-TRANSACTION-FILE-IO.
           OPEN I-O TRANSACTION-FILE.
           IF TRANSACTION-FILE-STATUS = '35'
               PERFORM INIT-TRANSACTION-FILE
               OPEN I-O TRANSACTION-FILE
           END-IF.
           MOVE TRANSACTION-FILE-STATUS TO WS-OPERATION-STATUS.
       
       CLOSE-TRANSACTION-FILE.
           CLOSE TRANSACTION-FILE.
       
       WRITE-TRANSACTION.
      *    Write transaction record
      *    Input: TRANSACTION-RECORD
      *    Output: WS-OPERATION-STATUS
           WRITE TRANSACTION-FILE-REC FROM TRANSACTION-RECORD
               INVALID KEY
                   MOVE '22' TO WS-OPERATION-STATUS
               NOT INVALID KEY
                   MOVE '00' TO WS-OPERATION-STATUS
           END-WRITE.
       
       READ-TRANSACTION.
      *    Read transaction by ID
      *    Input: TXN-ID in TRANSACTION-RECORD
      *    Output: TRANSACTION-RECORD, WS-OPERATION-STATUS
           READ TRANSACTION-FILE INTO TRANSACTION-RECORD
               KEY IS TXN-ID
               INVALID KEY
                   MOVE '23' TO WS-OPERATION-STATUS
               NOT INVALID KEY
                   MOVE '00' TO WS-OPERATION-STATUS
           END-READ.
       
      ******************************************************************
      * CONTROL FILE OPERATIONS
      ******************************************************************
       
       INIT-CONTROL-FILE.
      *    Initialize control file with default values
           OPEN OUTPUT CONTROL-FILE.
           IF CONTROL-FILE-STATUS NOT = '00'
               DISPLAY 'ERROR: Cannot initialize control file'
               STOP RUN
           END-IF.
           MOVE 10000000 TO CTL-LAST-ACCOUNT-ID.
           MOVE 1000000000 TO CTL-LAST-LEDGER-ID.
           MOVE 100000000000 TO CTL-LAST-TXN-ID.
           WRITE CONTROL-FILE-REC FROM CONTROL-RECORD.
           CLOSE CONTROL-FILE.
       
       READ-CONTROL.
      *    Read control record
      *    Output: CONTROL-RECORD, WS-OPERATION-STATUS
           OPEN INPUT CONTROL-FILE.
           IF CONTROL-FILE-STATUS = '35'
               PERFORM INIT-CONTROL-FILE
               OPEN INPUT CONTROL-FILE
           END-IF.
           READ CONTROL-FILE INTO CONTROL-RECORD.
           MOVE CONTROL-FILE-STATUS TO WS-OPERATION-STATUS.
           CLOSE CONTROL-FILE.
       
       UPDATE-CONTROL.
      *    Update control record
      *    Input: CONTROL-RECORD
      *    Output: WS-OPERATION-STATUS
           OPEN OUTPUT CONTROL-FILE.
           WRITE CONTROL-FILE-REC FROM CONTROL-RECORD.
           MOVE CONTROL-FILE-STATUS TO WS-OPERATION-STATUS.
           CLOSE CONTROL-FILE.
       
       END PROGRAM FILES.
