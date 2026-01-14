@echo off
REM run.bat - Run script for Windows

REM Check if binary exists
if not exist "bin\banking_system.exe" (
    echo ERROR: Binary not found. Please run build.bat first.
    echo.
    pause
    exit /b 1
)

REM Create data directory if it doesn't exist
if not exist data mkdir data

REM Run the system
echo Starting Mini Core Banking System...
echo.
bin\banking_system.exe
