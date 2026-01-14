      ******************************************************************
      * SCREENS.COB - Screen Rendering Module
      * 
      * Handles all screen rendering using ncurses-like interface.
      * Provides a serious, professional banking terminal UI.
      *
      * Design decisions:
      * - Fixed layout for consistency
      * - Minimal colors (only for status indicators)
      * - Dense information display
      * - Clear visual hierarchy
      * - No animations or decorations
      *
      * Screen areas:
      * - Header: System title and current time
      * - Info panel: Account information
      * - Main area: Menu or data display
      * - Footer: Status messages and help
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCREENS.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * Screen dimensions (assuming 80x24 terminal)
       01  SCREEN-DIMENSIONS.
           05  SCREEN-WIDTH            PIC 99 VALUE 80.
           05  SCREEN-HEIGHT           PIC 99 VALUE 24.
           05  HEADER-HEIGHT           PIC 9 VALUE 3.
           05  FOOTER-HEIGHT           PIC 9 VALUE 2.
       
      * Current screen state
       01  CURRENT-SCREEN              PIC X(20).
           88  SCREEN-MAIN-MENU        VALUE 'MAIN_MENU'.
           88  SCREEN-ACCOUNT-INFO     VALUE 'ACCOUNT_INFO'.
           88  SCREEN-LEDGER-VIEW      VALUE 'LEDGER_VIEW'.
           88  SCREEN-TRANSACTION      VALUE 'TRANSACTION'.
       
      * Display buffers
       01  WS-HEADER-LINE-1            PIC X(80).
       01  WS-HEADER-LINE-2            PIC X(80).
       01  WS-HEADER-LINE-3            PIC X(80).
       01  WS-FOOTER-LINE-1            PIC X(80).
       01  WS-FOOTER-LINE-2            PIC X(80).
       01  WS-STATUS-MESSAGE           PIC X(80).
       
      * Account display data
       01  WS-DISPLAY-ACCOUNT.
           05  WS-DISP-ACCOUNT-ID      PIC 9(8).
           05  WS-DISP-HOLDER-NAME     PIC X(30).
           05  WS-DISP-STATUS          PIC X(8).
           05  WS-DISP-BALANCE         PIC -ZZZ,ZZZ,ZZ9.99.
           05  WS-DISP-BLOCKED         PIC -ZZZ,ZZZ,ZZ9.99.
           05  WS-DISP-AVAILABLE       PIC -ZZZ,ZZZ,ZZ9.99.
       
      * Menu items
       01  WS-MENU-ITEMS.
           05  WS-MENU-ITEM OCCURS 10 TIMES.
               10  WS-MENU-NUMBER      PIC 9.
               10  WS-MENU-TEXT        PIC X(40).
               10  WS-MENU-ENABLED     PIC 9.
       
       01  WS-CURRENT-MENU-SIZE        PIC 99.
       
      * ANSI escape sequences (if ncurses not available)
       01  ANSI-CLEAR-SCREEN           PIC X(10) VALUE X'1B5B324A'.
       01  ANSI-RESET-CURSOR           PIC X(10) VALUE X'1B5B483B'.
       01  ANSI-COLOR-RESET            PIC X(10) VALUE X'1B5B306D'.
       01  ANSI-COLOR-RED              PIC X(10) VALUE X'1B5B33316D'.
       01  ANSI-COLOR-GREEN            PIC X(10) VALUE X'1B5B33326D'.
       01  ANSI-COLOR-YELLOW           PIC X(10) VALUE X'1B5B33336D'.
       
       PROCEDURE DIVISION.
       
      ******************************************************************
      * SCREEN INITIALIZATION
      ******************************************************************
       
       INIT-SCREEN.
      *    Initialize screen for TUI display
      *    Clear screen and set up terminal
           DISPLAY ANSI-CLEAR-SCREEN.
           DISPLAY ANSI-RESET-CURSOR.
       
       CLEAR-SCREEN.
      *    Clear entire screen
           DISPLAY ANSI-CLEAR-SCREEN.
           DISPLAY ANSI-RESET-CURSOR.
       
      ******************************************************************
      * HEADER RENDERING
      ******************************************************************
       
       RENDER-HEADER.
      *    Render top header with system title and timestamp
           MOVE SPACES TO WS-HEADER-LINE-1.
           MOVE SPACES TO WS-HEADER-LINE-2.
           MOVE SPACES TO WS-HEADER-LINE-3.
           
           STRING '='
                  DELIMITED BY SIZE
                  INTO WS-HEADER-LINE-1.
           PERFORM VARYING WS-INDEX FROM 2 BY 1 
                   UNTIL WS-INDEX > 80
               STRING WS-HEADER-LINE-1(1:WS-INDEX - 1)
                      '='
                      DELIMITED BY SIZE
                      INTO WS-HEADER-LINE-1
           END-PERFORM.
           
           MOVE 
           '          MINI CORE BANKING SYSTEM - OPERATIONS TERMINAL' 
               TO WS-HEADER-LINE-2.
           
           MOVE WS-HEADER-LINE-1 TO WS-HEADER-LINE-3.
           
           DISPLAY WS-HEADER-LINE-1.
           DISPLAY WS-HEADER-LINE-2.
           DISPLAY WS-HEADER-LINE-3.
           DISPLAY ' '.
       
      ******************************************************************
      * FOOTER RENDERING
      ******************************************************************
       
       RENDER-FOOTER.
      *    Render bottom footer with status and help
           MOVE SPACES TO WS-FOOTER-LINE-1.
           MOVE SPACES TO WS-FOOTER-LINE-2.
           
           STRING '='
                  DELIMITED BY SIZE
                  INTO WS-FOOTER-LINE-1.
           PERFORM VARYING WS-INDEX FROM 2 BY 1 
                   UNTIL WS-INDEX > 80
               STRING WS-FOOTER-LINE-1(1:WS-INDEX - 1)
                      '='
                      DELIMITED BY SIZE
                      INTO WS-FOOTER-LINE-1
           END-PERFORM.
           
           IF WS-STATUS-MESSAGE NOT = SPACES
               MOVE WS-STATUS-MESSAGE TO WS-FOOTER-LINE-2
           ELSE
               MOVE 'Ready' TO WS-FOOTER-LINE-2
           END-IF.
           
           DISPLAY WS-FOOTER-LINE-1.
           DISPLAY WS-FOOTER-LINE-2.
       
      ******************************************************************
      * ACCOUNT INFO PANEL
      ******************************************************************
       
       RENDER-ACCOUNT-INFO.
      *    Render account information panel
      *    Input: WS-DISPLAY-ACCOUNT
           DISPLAY 'Account Information:'.
           DISPLAY '-------------------------------------------'.
           
           STRING 'Account ID:      ' WS-DISP-ACCOUNT-ID
                  DELIMITED BY SIZE
                  INTO WS-WORK-LINE.
           DISPLAY WS-WORK-LINE.
           
           STRING 'Holder:          ' WS-DISP-HOLDER-NAME
                  DELIMITED BY SIZE
                  INTO WS-WORK-LINE.
           DISPLAY WS-WORK-LINE.
           
           STRING 'Status:          ' WS-DISP-STATUS
                  DELIMITED BY SIZE
                  INTO WS-WORK-LINE.
           
           IF WS-DISP-STATUS = 'ACTIVE'
               DISPLAY ANSI-COLOR-GREEN WS-WORK-LINE ANSI-COLOR-RESET
           ELSE IF WS-DISP-STATUS = 'BLOCKED'
               DISPLAY ANSI-COLOR-RED WS-WORK-LINE ANSI-COLOR-RESET
           ELSE
               DISPLAY WS-WORK-LINE
           END-IF.
           
           STRING 'Balance:      R$ ' WS-DISP-BALANCE
                  DELIMITED BY SIZE
                  INTO WS-WORK-LINE.
           DISPLAY WS-WORK-LINE.
           
           STRING 'Blocked:      R$ ' WS-DISP-BLOCKED
                  DELIMITED BY SIZE
                  INTO WS-WORK-LINE.
           DISPLAY WS-WORK-LINE.
           
           STRING 'Available:    R$ ' WS-DISP-AVAILABLE
                  DELIMITED BY SIZE
                  INTO WS-WORK-LINE.
           DISPLAY WS-WORK-LINE.
           
           DISPLAY '-------------------------------------------'.
           DISPLAY ' '.
       
      ******************************************************************
      * MENU RENDERING
      ******************************************************************
       
       RENDER-MENU.
      *    Render menu items
      *    Input: WS-MENU-ITEMS, WS-CURRENT-MENU-SIZE
           DISPLAY 'Operations:'.
           DISPLAY ' '.
           
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-CURRENT-MENU-SIZE
               
               IF WS-MENU-ENABLED(WS-INDEX) = 1
                   STRING '  ' 
                          WS-MENU-NUMBER(WS-INDEX) 
                          ' - ' 
                          WS-MENU-TEXT(WS-INDEX)
                          DELIMITED BY SIZE
                          INTO WS-WORK-LINE
                   DISPLAY WS-WORK-LINE
               END-IF
           END-PERFORM.
           
           DISPLAY ' '.
           DISPLAY 'Enter option: ' WITH NO ADVANCING.
       
      ******************************************************************
      * LEDGER DISPLAY
      ******************************************************************
       
       RENDER-LEDGER-HEADER.
      *    Render ledger table header
           DISPLAY 'Transaction Ledger:'.
           DISPLAY '-------------------------------------------'
                   '-----------------------------------'.
           DISPLAY 'ID         Timestamp      Type      Amount'
                   '           Balance'.
           DISPLAY '-------------------------------------------'
                   '-----------------------------------'.
       
       RENDER-LEDGER-ENTRY.
      *    Render single ledger entry
      *    Input: Ledger entry data in working storage
           STRING WS-LED-ENTRY-ID ' '
                  WS-LED-ENTRY-TIMESTAMP ' '
                  WS-LED-ENTRY-TYPE ' '
                  WS-LED-ENTRY-AMOUNT ' '
                  WS-LED-ENTRY-BAL-AFTER
                  DELIMITED BY SIZE
                  INTO WS-WORK-LINE.
           DISPLAY WS-WORK-LINE.
       
      ******************************************************************
      * MESSAGE DISPLAY
      ******************************************************************
       
       DISPLAY-ERROR-MESSAGE.
      *    Display error message in red
      *    Input: WS-STATUS-MESSAGE
           DISPLAY ANSI-COLOR-RED.
           DISPLAY 'ERROR: ' WS-STATUS-MESSAGE.
           DISPLAY ANSI-COLOR-RESET.
       
       DISPLAY-SUCCESS-MESSAGE.
      *    Display success message in green
      *    Input: WS-STATUS-MESSAGE
           DISPLAY ANSI-COLOR-GREEN.
           DISPLAY 'SUCCESS: ' WS-STATUS-MESSAGE.
           DISPLAY ANSI-COLOR-RESET.
       
       DISPLAY-INFO-MESSAGE.
      *    Display info message
      *    Input: WS-STATUS-MESSAGE
           DISPLAY 'INFO: ' WS-STATUS-MESSAGE.
       
      ******************************************************************
      * INPUT PROMPTS
      ******************************************************************
       
       PROMPT-ACCOUNT-ID.
      *    Prompt for account ID input
      *    Output: WS-INPUT-VALUE
           DISPLAY 'Enter Account ID (8 digits): ' 
                   WITH NO ADVANCING.
       
       PROMPT-AMOUNT.
      *    Prompt for amount input
      *    Output: WS-INPUT-VALUE
           DISPLAY 'Enter Amount (R$): ' WITH NO ADVANCING.
       
       PROMPT-DESCRIPTION.
      *    Prompt for description input
      *    Output: WS-INPUT-VALUE
           DISPLAY 'Enter Description: ' WITH NO ADVANCING.
       
       PROMPT-CONTINUE.
      *    Prompt to continue
           DISPLAY ' '.
           DISPLAY 'Press ENTER to continue...' WITH NO ADVANCING.
       
      * Working variables (needed for rendering)
       01  WS-INDEX                    PIC 99.
       01  WS-WORK-LINE                PIC X(80).
       
       END PROGRAM SCREENS.
