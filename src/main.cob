      ******************************************************************
      * MAIN.COB - Main Program Entry Point
      * 
      * Mini Core Banking System
      * A realistic terminal-based core banking system.
      *
      * Design philosophy:
      * - Serious, professional interface
      * - Real banking domain logic
      * - Deterministic behavior
      * - File-based persistence
      * - Full audit trail (ledger)
      *
      * This is a portfolio/educational project demonstrating:
      * - COBOL programming skills
      * - Banking domain knowledge
      * - System design capabilities
      * - TUI development
      *
      * Architecture:
      * - Core: Banking logic (accounts, transactions, ledger)
      * - Storage: File-based persistence
      * - UI: Terminal user interface
      *
      * Author: Portfolio Project
      * Date: 2026
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.
       AUTHOR. PORTFOLIO.
       DATE-WRITTEN. 2026-01-13.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. LINUX.
       OBJECT-COMPUTER. LINUX.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  WS-SYSTEM-INFO.
           05  WS-SYSTEM-NAME          PIC X(40) 
               VALUE 'MINI CORE BANKING SYSTEM'.
           05  WS-VERSION              PIC X(10) VALUE 'v1.0.0'.
           05  WS-BUILD-DATE           PIC X(10) VALUE '2026-01-13'.
       
       01  WS-INITIALIZATION-STATUS    PIC XX.
           88  INIT-SUCCESS            VALUE '00'.
           88  INIT-FAILED             VALUE '99'.
       
       01  WS-ERROR-MESSAGE            PIC X(80).
       
       PROCEDURE DIVISION.
       
      ******************************************************************
      * MAIN PROGRAM
      ******************************************************************
       
       MAIN-PROGRAM.
      *    Entry point for the banking system
           
           PERFORM DISPLAY-BANNER.
           PERFORM INITIALIZE-SYSTEM.
           
           IF NOT INIT-SUCCESS
               DISPLAY 'FATAL: System initialization failed.'
               DISPLAY 'Error: ' WS-ERROR-MESSAGE
               STOP RUN
           END-IF.
           
           PERFORM RUN-APPLICATION.
           
           PERFORM SHUTDOWN-SYSTEM.
           
           STOP RUN.
       
      ******************************************************************
      * INITIALIZATION
      ******************************************************************
       
       DISPLAY-BANNER.
      *    Display startup banner
           DISPLAY '================================================'.
           DISPLAY WS-SYSTEM-NAME.
           DISPLAY 'Version: ' WS-VERSION.
           DISPLAY 'Build: ' WS-BUILD-DATE.
           DISPLAY '================================================'.
           DISPLAY ' '.
           DISPLAY 'Initializing system...'.
       
       INITIALIZE-SYSTEM.
      *    Initialize system components and data files
           
           MOVE '00' TO WS-INITIALIZATION-STATUS.
           
      *    Check if data directory exists
           PERFORM CHECK-DATA-DIRECTORY.
           
           IF NOT INIT-SUCCESS
               GO TO INITIALIZE-SYSTEM-END
           END-IF.
           
      *    Initialize control file if needed
           PERFORM INITIALIZE-CONTROL-FILE.
           
           IF NOT INIT-SUCCESS
               GO TO INITIALIZE-SYSTEM-END
           END-IF.
           
           DISPLAY 'System initialized successfully.'.
           DISPLAY ' '.
           
       INITIALIZE-SYSTEM-END.
           EXIT.
       
       CHECK-DATA-DIRECTORY.
      *    Verify data directory exists
      *    In production, this would check file system
      *    For this implementation, we assume it exists
           MOVE '00' TO WS-INITIALIZATION-STATUS.
       
       INITIALIZE-CONTROL-FILE.
      *    Initialize control file with default sequence numbers
      *    This is called only on first run
           
      *    Try to read existing control file
           CALL 'FILES' USING 'READ-CONTROL' CONTROL-RECORD 
               WS-INITIALIZATION-STATUS.
           
           IF WS-INITIALIZATION-STATUS = '35' OR 
              WS-INITIALIZATION-STATUS = '05'
      *        File doesn't exist, initialize it
               DISPLAY 'Creating control file...'
               CALL 'FILES' USING 'INIT-CONTROL-FILE'
               MOVE '00' TO WS-INITIALIZATION-STATUS
           END-IF.
       
      ******************************************************************
      * APPLICATION EXECUTION
      ******************************************************************
       
       RUN-APPLICATION.
      *    Run the main application (menu system)
           DISPLAY 'Starting application...'.
           DISPLAY ' '.
           CALL 'MENU' USING 'MAIN-MENU-LOOP'.
       
      ******************************************************************
      * SHUTDOWN
      ******************************************************************
       
       SHUTDOWN-SYSTEM.
      *    Clean shutdown
           DISPLAY ' '.
           DISPLAY 'System shutdown complete.'.
           DISPLAY 'Thank you for using ' WS-SYSTEM-NAME.
       
      * Copy data structure definitions
       COPY 'src/storage/schema.cob'.
       
       END PROGRAM MAIN.
