      ******************************************************************
      * SCHEMA.COB - Data Record Layouts
      * 
      * Defines all data structures used across the system.
      * These layouts match the file structures for persistence.
      *
      * Design decisions:
      * - Fixed-length records for predictable file I/O
      * - Numeric fields stored as COMP-3 (packed decimal)
      * - Timestamps as YYYYMMDDHHMMSS format
      * - Account IDs are 8-digit numeric
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCHEMA.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * Account Master Record (100 bytes)
       01  ACCOUNT-RECORD.
           05  ACC-ID                  PIC 9(8).
           05  ACC-HOLDER-NAME         PIC X(30).
           05  ACC-TYPE                PIC X(10).
           05  ACC-STATUS              PIC X(8).
               88  ACC-ACTIVE          VALUE 'ACTIVE'.
               88  ACC-BLOCKED         VALUE 'BLOCKED'.
               88  ACC-CLOSED          VALUE 'CLOSED'.
           05  ACC-BALANCE             PIC S9(13)V99 COMP-3.
           05  ACC-BLOCKED-AMT         PIC S9(13)V99 COMP-3.
           05  ACC-CREATED-TS          PIC X(14).
           05  ACC-UPDATED-TS          PIC X(14).
           05  ACC-FILLER              PIC X(10).
       
      * Ledger Entry Record (120 bytes)
       01  LEDGER-RECORD.
           05  LED-ID                  PIC 9(10).
           05  LED-ACCOUNT-ID          PIC 9(8).
           05  LED-TXN-ID              PIC 9(12).
           05  LED-TIMESTAMP           PIC X(14).
           05  LED-TYPE                PIC X(8).
               88  LED-CREDIT          VALUE 'CREDIT'.
               88  LED-DEBIT           VALUE 'DEBIT'.
               88  LED-BLOCK           VALUE 'BLOCK'.
               88  LED-UNBLOCK         VALUE 'UNBLOCK'.
           05  LED-AMOUNT              PIC S9(13)V99 COMP-3.
           05  LED-BALANCE-BEFORE      PIC S9(13)V99 COMP-3.
           05  LED-BALANCE-AFTER       PIC S9(13)V99 COMP-3.
           05  LED-DESCRIPTION         PIC X(50).
           05  LED-FILLER              PIC X(10).
       
      * Transaction Record (150 bytes)
       01  TRANSACTION-RECORD.
           05  TXN-ID                  PIC 9(12).
           05  TXN-ACCOUNT-ID          PIC 9(8).
           05  TXN-TIMESTAMP           PIC X(14).
           05  TXN-TYPE                PIC X(10).
               88  TXN-CREDIT          VALUE 'CREDIT'.
               88  TXN-DEBIT           VALUE 'DEBIT'.
               88  TXN-BLOCK           VALUE 'BLOCK'.
               88  TXN-UNBLOCK         VALUE 'UNBLOCK'.
               88  TXN-TRANSFER        VALUE 'TRANSFER'.
           05  TXN-AMOUNT              PIC S9(13)V99 COMP-3.
           05  TXN-STATUS              PIC X(10).
               88  TXN-PENDING         VALUE 'PENDING'.
               88  TXN-COMPLETED       VALUE 'COMPLETED'.
               88  TXN-FAILED          VALUE 'FAILED'.
               88  TXN-REVERSED        VALUE 'REVERSED'.
           05  TXN-DESCRIPTION         PIC X(80).
           05  TXN-FILLER              PIC X(10).
       
      * System Control Record (for sequence numbers)
       01  CONTROL-RECORD.
           05  CTL-LAST-ACCOUNT-ID     PIC 9(8).
           05  CTL-LAST-LEDGER-ID      PIC 9(10).
           05  CTL-LAST-TXN-ID         PIC 9(12).
           05  CTL-FILLER              PIC X(50).
