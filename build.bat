@echo off
REM build.bat - Build script for Windows

echo ==============================================
echo Mini Core Banking System - Build Script
echo ==============================================
echo.

where cobc >nul 2>nul
if errorlevel 1 (
    echo ERROR: GnuCOBOL not found.
    echo.
    echo Install from: https://sourceforge.net/projects/gnucobol/
    echo Or use: wsl --install
    echo.
    pause
    exit /b 1
)

echo GnuCOBOL found!
echo.

if not exist bin mkdir bin
if not exist data mkdir data

echo Creating source file...

echo        IDENTIFICATION DIVISION. > bin\banking_system.cob
echo        PROGRAM-ID. BANKING-SYSTEM. >> bin\banking_system.cob
echo        DATA DIVISION. >> bin\banking_system.cob
echo        WORKING-STORAGE SECTION. >> bin\banking_system.cob
echo        01  WS-MENU-CHOICE PIC 9. >> bin\banking_system.cob
echo        01  WS-ACCOUNT-ID PIC 9(8) VALUE 10000001. >> bin\banking_system.cob
echo        01  WS-BALANCE PIC 9(13)V99 VALUE 0. >> bin\banking_system.cob
echo        01  WS-AMOUNT PIC 9(13)V99. >> bin\banking_system.cob
echo        01  WS-CONTINUE PIC X. >> bin\banking_system.cob
echo        PROCEDURE DIVISION. >> bin\banking_system.cob
echo        MAIN-PROGRAM. >> bin\banking_system.cob
echo            DISPLAY '============================================='. >> bin\banking_system.cob
echo            DISPLAY '   MINI CORE BANKING SYSTEM - DEMO'. >> bin\banking_system.cob
echo            DISPLAY '============================================='. >> bin\banking_system.cob
echo            DISPLAY ' '. >> bin\banking_system.cob
echo            DISPLAY 'Simplified demo version'. >> bin\banking_system.cob
echo            DISPLAY 'Full source in src/ directory'. >> bin\banking_system.cob
echo            DISPLAY ' '. >> bin\banking_system.cob
echo            DISPLAY 'Press ENTER to continue...'. >> bin\banking_system.cob
echo            ACCEPT WS-CONTINUE. >> bin\banking_system.cob
echo            PERFORM MAIN-MENU-LOOP. >> bin\banking_system.cob
echo            DISPLAY 'System shutdown.'. >> bin\banking_system.cob
echo            STOP RUN. >> bin\banking_system.cob
echo        MAIN-MENU-LOOP. >> bin\banking_system.cob
echo            PERFORM UNTIL WS-MENU-CHOICE = 9 >> bin\banking_system.cob
echo                PERFORM DISPLAY-MENU >> bin\banking_system.cob
echo                ACCEPT WS-MENU-CHOICE >> bin\banking_system.cob
echo                PERFORM PROCESS-CHOICE >> bin\banking_system.cob
echo            END-PERFORM. >> bin\banking_system.cob
echo        DISPLAY-MENU. >> bin\banking_system.cob
echo            DISPLAY ' '. >> bin\banking_system.cob
echo            DISPLAY '=== MAIN MENU ==='. >> bin\banking_system.cob
echo            DISPLAY '  1 - View Account'. >> bin\banking_system.cob
echo            DISPLAY '  2 - Credit'. >> bin\banking_system.cob
echo            DISPLAY '  3 - Debit'. >> bin\banking_system.cob
echo            DISPLAY '  9 - Exit'. >> bin\banking_system.cob
echo            DISPLAY ' '. >> bin\banking_system.cob
echo            DISPLAY 'Option: ' WITH NO ADVANCING. >> bin\banking_system.cob
echo        PROCESS-CHOICE. >> bin\banking_system.cob
echo            EVALUATE WS-MENU-CHOICE >> bin\banking_system.cob
echo                WHEN 1 PERFORM VIEW-ACCOUNT >> bin\banking_system.cob
echo                WHEN 2 PERFORM CREDIT >> bin\banking_system.cob
echo                WHEN 3 PERFORM DEBIT >> bin\banking_system.cob
echo                WHEN 9 DISPLAY 'Exiting...' >> bin\banking_system.cob
echo                WHEN OTHER DISPLAY 'Invalid option' >> bin\banking_system.cob
echo            END-EVALUATE. >> bin\banking_system.cob
echo        VIEW-ACCOUNT. >> bin\banking_system.cob
echo            DISPLAY ' '. >> bin\banking_system.cob
echo            DISPLAY 'Account: ' WS-ACCOUNT-ID. >> bin\banking_system.cob
echo            DISPLAY 'Balance: R$ ' WS-BALANCE. >> bin\banking_system.cob
echo            DISPLAY 'Press ENTER...'. >> bin\banking_system.cob
echo            ACCEPT WS-CONTINUE. >> bin\banking_system.cob
echo        CREDIT. >> bin\banking_system.cob
echo            DISPLAY ' '. >> bin\banking_system.cob
echo            DISPLAY 'Amount: ' WITH NO ADVANCING. >> bin\banking_system.cob
echo            ACCEPT WS-AMOUNT. >> bin\banking_system.cob
echo            ADD WS-AMOUNT TO WS-BALANCE. >> bin\banking_system.cob
echo            DISPLAY 'Success! New balance: R$ ' WS-BALANCE. >> bin\banking_system.cob
echo            DISPLAY 'Press ENTER...'. >> bin\banking_system.cob
echo            ACCEPT WS-CONTINUE. >> bin\banking_system.cob
echo        DEBIT. >> bin\banking_system.cob
echo            DISPLAY ' '. >> bin\banking_system.cob
echo            DISPLAY 'Amount: ' WITH NO ADVANCING. >> bin\banking_system.cob
echo            ACCEPT WS-AMOUNT. >> bin\banking_system.cob
echo            IF WS-AMOUNT GREATER THAN WS-BALANCE >> bin\banking_system.cob
echo                DISPLAY 'ERROR: Insufficient funds' >> bin\banking_system.cob
echo            ELSE >> bin\banking_system.cob
echo                SUBTRACT WS-AMOUNT FROM WS-BALANCE >> bin\banking_system.cob
echo                DISPLAY 'Success! New balance: R$ ' WS-BALANCE >> bin\banking_system.cob
echo            END-IF. >> bin\banking_system.cob
echo            DISPLAY 'Press ENTER...'. >> bin\banking_system.cob
echo            ACCEPT WS-CONTINUE. >> bin\banking_system.cob
echo        END PROGRAM BANKING-SYSTEM. >> bin\banking_system.cob

echo Compiling...
cobc -x -o bin\banking_system.exe bin\banking_system.cob

if errorlevel 1 (
    echo.
    echo BUILD FAILED!
    pause
    exit /b 1
)

echo.
echo ==============================================
echo BUILD SUCCESS!
echo ==============================================
echo.
echo Run with: run.bat
echo.
pause
