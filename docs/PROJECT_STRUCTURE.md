# Project Structure

Complete overview of the Mini Core Banking System file structure and organization.

## Directory Tree

```
mini-core-banking/
│
├── src/                          # Source code
│   ├── core/                     # Core banking logic
│   │   ├── account.cob          # Account management (270 lines)
│   │   ├── ledger.cob           # Immutable ledger (240 lines)
│   │   ├── transactions.cob     # Transaction processing (380 lines)
│   │   └── validation.cob       # Business rules (180 lines)
│   │
│   ├── storage/                  # Persistence layer
│   │   ├── schema.cob           # Data record layouts (80 lines)
│   │   └── files.cob            # File I/O operations (280 lines)
│   │
│   ├── ui/                       # User interface
│   │   ├── screens.cob          # Screen rendering (310 lines)
│   │   ├── input.cob            # Input handling (260 lines)
│   │   └── menu.cob             # Navigation logic (450 lines)
│   │
│   └── main.cob                 # Program entry point (110 lines)
│
├── data/                         # Data files (created at runtime)
│   ├── accounts.dat             # Account records (indexed)
│   ├── ledger.dat               # Transaction ledger (sequential)
│   ├── transactions.dat         # Transaction log (indexed)
│   └── control.dat              # Sequence control (single record)
│
├── bin/                          # Compiled executables
│   └── banking_system           # Main executable
│
├── docs/                         # Documentation
│   ├── ARCHITECTURE.md          # Technical architecture
│   ├── USAGE.md                 # Usage guide with examples
│   └── DESIGN_DECISIONS.md      # Design decisions explained
│
├── build.sh                      # Build script
├── run.sh                        # Run script
├── demo.sh                       # Interactive demonstration
├── README.md                     # Main documentation
├── LICENSE                       # MIT License with disclaimer
└── .gitignore                    # Git ignore rules

Total: ~2,560 lines of COBOL code
```

## Module Responsibilities

### Core Layer (`src/core/`)

Pure business logic. No I/O dependencies.

#### `account.cob` - Account Management
```
Responsibilities:
├── CREATE-ACCOUNT          # Generate new account
├── GET-ACCOUNT            # Retrieve by ID
├── UPDATE-ACCOUNT-BALANCE # Persist changes
├── BLOCK-ACCOUNT          # Change status to BLOCKED
├── UNBLOCK-ACCOUNT        # Change status to ACTIVE
└── GET-AVAILABLE-BALANCE  # Calculate available funds

Data Structures:
├── ACCOUNT-RECORD         # Account master data
└── WS-ACCOUNT-OPERATION   # Operation status/message

Key Features:
├── Sequential ID generation
├── Status management (ACTIVE/BLOCKED/CLOSED)
├── Timestamp tracking
└── Balance/blocked amount separation
```

#### `ledger.cob` - Transaction Ledger
```
Responsibilities:
├── CREATE-LEDGER-ENTRY      # Append new entry (immutable)
├── QUERY-LEDGER-BY-ACCOUNT  # Retrieve entries
├── GET-LAST-LEDGER-ENTRY    # Get most recent
└── VERIFY-LEDGER-INTEGRITY  # Reconciliation (stub)

Data Structures:
├── LEDGER-RECORD           # Ledger entry
└── WS-LEDGER-ENTRIES-TABLE # Query results

Key Features:
├── Append-only (immutable)
├── Balance before/after tracking
├── Timestamp every entry
└── Full audit trail
```

#### `transactions.cob` - Transaction Processing
```
Responsibilities:
├── PROCESS-CREDIT        # Deposit funds
├── PROCESS-DEBIT         # Withdraw funds
├── PROCESS-BLOCK-FUNDS   # Hold funds
└── PROCESS-UNBLOCK-FUNDS # Release funds

Data Structures:
├── TRANSACTION-RECORD    # Transaction log entry
└── WS-TXN-OPERATION     # Operation status

Transaction Flow:
├── 1. Validate inputs
├── 2. Read account
├── 3. Check business rules
├── 4. Update account
├── 5. Create ledger entry
├── 6. Log transaction
└── 7. Return status

Key Features:
├── Atomic operations
├── Validation before execution
├── Complete audit trail
└── Status tracking (PENDING/COMPLETED/FAILED)
```

#### `validation.cob` - Business Rules
```
Responsibilities:
├── VALIDATE-CREDIT-AMOUNT        # Amount ranges
├── VALIDATE-DEBIT-AMOUNT         # Amount ranges
├── VALIDATE-SUFFICIENT-BALANCE   # Balance check
├── VALIDATE-ACCOUNT-ACTIVE       # Status check
├── VALIDATE-ACCOUNT-FOR-DEBIT    # Combined debit checks
├── VALIDATE-ACCOUNT-FOR-CREDIT   # Combined credit checks
└── VALIDATE-ACCOUNT-ID-FORMAT    # ID format check

Business Rules:
├── Min credit: R$ 0.01
├── Max credit: R$ 999,999,999,999.99
├── Min debit: R$ 0.01
├── Max debit: R$ 999,999,999,999.99
├── Account ID: 10000000-99999999
└── Only ACTIVE accounts can transact

Key Features:
├── Pure validation (no side effects)
├── Explicit error messages
└── Configurable limits
```

### Storage Layer (`src/storage/`)

Handles all persistence. No business logic.

#### `schema.cob` - Data Layouts
```
Record Definitions:
├── ACCOUNT-RECORD (100 bytes)
│   ├── ACC-ID (8 digits)
│   ├── ACC-HOLDER-NAME (30 chars)
│   ├── ACC-TYPE (10 chars)
│   ├── ACC-STATUS (8 chars)
│   ├── ACC-BALANCE (COMP-3, 15 digits)
│   ├── ACC-BLOCKED-AMT (COMP-3, 15 digits)
│   ├── ACC-CREATED-TS (14 chars)
│   ├── ACC-UPDATED-TS (14 chars)
│   └── ACC-FILLER (10 bytes)
│
├── LEDGER-RECORD (120 bytes)
│   ├── LED-ID (10 digits)
│   ├── LED-ACCOUNT-ID (8 digits)
│   ├── LED-TXN-ID (12 digits)
│   ├── LED-TIMESTAMP (14 chars)
│   ├── LED-TYPE (8 chars)
│   ├── LED-AMOUNT (COMP-3)
│   ├── LED-BALANCE-BEFORE (COMP-3)
│   ├── LED-BALANCE-AFTER (COMP-3)
│   ├── LED-DESCRIPTION (50 chars)
│   └── LED-FILLER (10 bytes)
│
├── TRANSACTION-RECORD (150 bytes)
│   ├── TXN-ID (12 digits)
│   ├── TXN-ACCOUNT-ID (8 digits)
│   ├── TXN-TIMESTAMP (14 chars)
│   ├── TXN-TYPE (10 chars)
│   ├── TXN-AMOUNT (COMP-3)
│   ├── TXN-STATUS (10 chars)
│   ├── TXN-DESCRIPTION (80 chars)
│   └── TXN-FILLER (10 bytes)
│
└── CONTROL-RECORD (80 bytes)
    ├── CTL-LAST-ACCOUNT-ID (8 digits)
    ├── CTL-LAST-LEDGER-ID (10 digits)
    ├── CTL-LAST-TXN-ID (12 digits)
    └── CTL-FILLER (50 bytes)
```

#### `files.cob` - File Operations
```
File Management:
├── ACCOUNT-FILE (indexed by ACC-ID)
│   ├── OPEN-ACCOUNT-FILE-IO
│   ├── CLOSE-ACCOUNT-FILE
│   ├── WRITE-ACCOUNT
│   ├── READ-ACCOUNT
│   └── UPDATE-ACCOUNT
│
├── LEDGER-FILE (sequential)
│   ├── OPEN-LEDGER-FILE-EXTEND
│   ├── OPEN-LEDGER-FILE-INPUT
│   ├── CLOSE-LEDGER-FILE
│   ├── APPEND-LEDGER
│   └── READ-NEXT-LEDGER
│
├── TRANSACTION-FILE (indexed by TXN-ID)
│   ├── OPEN-TRANSACTION-FILE-IO
│   ├── CLOSE-TRANSACTION-FILE
│   ├── WRITE-TRANSACTION
│   └── READ-TRANSACTION
│
└── CONTROL-FILE (single record)
    ├── READ-CONTROL
    └── UPDATE-CONTROL

File Status Codes:
├── 00: Success
├── 10: End of file
├── 23: Record not found
├── 22: Duplicate key
├── 35: File not found
└── 99: General error
```

### UI Layer (`src/ui/`)

Terminal interface. No business logic.

#### `screens.cob` - Screen Rendering
```
Rendering Functions:
├── INIT-SCREEN              # Initialize terminal
├── CLEAR-SCREEN             # Clear display
├── RENDER-HEADER            # System title bar
├── RENDER-FOOTER            # Status bar
├── RENDER-ACCOUNT-INFO      # Account details panel
├── RENDER-MENU              # Menu options
├── RENDER-LEDGER-HEADER     # Ledger table header
├── RENDER-LEDGER-ENTRY      # Single ledger row
├── DISPLAY-ERROR-MESSAGE    # Error in red
├── DISPLAY-SUCCESS-MESSAGE  # Success in green
├── DISPLAY-INFO-MESSAGE     # Info message
├── PROMPT-ACCOUNT-ID        # ID input prompt
├── PROMPT-AMOUNT            # Amount input prompt
├── PROMPT-DESCRIPTION       # Description prompt
└── PROMPT-CONTINUE          # Wait for Enter

ANSI Codes Used:
├── Clear screen
├── Reset cursor
├── Red color (errors)
├── Green color (success)
├── Yellow color (warnings)
└── Reset color

Screen Layout:
┌─────────────────────────────────────────────────────────┐
│ ========== HEADER (3 lines) ==========                  │
│ Account Info Panel (if loaded)                          │
│ Main Content Area (menu/forms/data)                     │
│                                                          │
│ ========== FOOTER (2 lines) ==========                  │
│ Status: Ready                                            │
└─────────────────────────────────────────────────────────┘
```

#### `input.cob` - Input Handling
```
Input Functions:
├── READ-LINE               # Basic line input
├── CLEAR-INPUT-BUFFER      # Reset buffer
├── GET-NUMERIC-INPUT       # Integers
├── GET-DECIMAL-INPUT       # Money amounts
├── GET-TEXT-INPUT          # Strings
├── GET-MENU-CHOICE         # Menu options (1-99)
├── GET-ACCOUNT-ID          # 8-digit account ID
├── GET-AMOUNT              # Positive decimal
├── GET-CONFIRMATION        # Y/N input
└── WAIT-FOR-ENTER          # Pause

Validation:
├── VALIDATE-NUMERIC-FORMAT    # Digits only
├── VALIDATE-DECIMAL-FORMAT    # Digits + decimal point
│   ├── Max 2 decimal places
│   ├── Single decimal point
│   └── Comma → period conversion

Input Types:
├── Numeric: PIC 9(12)
├── Decimal: PIC S9(13)V99
├── Text: PIC X(80)
└── Menu: PIC 99
```

#### `menu.cob` - Navigation
```
State Machine:
├── MAIN                   # Account selection
├── OPERATIONS             # Transaction menu
├── CREATE_ACCOUNT         # Account creation
├── CREDIT                 # Credit transaction
├── DEBIT                  # Debit transaction
├── BLOCK                  # Block funds
├── VIEW_LEDGER            # Ledger display
└── EXIT                   # Shutdown

Main Menu:
├── 1 - Load Account
├── 2 - Create New Account
└── 3 - Exit System

Operations Menu:
├── 1 - Credit Account
├── 2 - Debit Account
├── 3 - Block Funds
├── 4 - View Ledger
└── 5 - Back to Main Menu

Navigation Flow:
MAIN → Load account → OPERATIONS → Transaction → OPERATIONS
  └──→ Create account → MAIN
  └──→ Exit → SHUTDOWN
```

### Entry Point (`src/main.cob`)

```
Main Program Flow:
├── DISPLAY-BANNER          # Show version info
├── INITIALIZE-SYSTEM       # Setup
│   ├── CHECK-DATA-DIRECTORY
│   └── INITIALIZE-CONTROL-FILE
├── RUN-APPLICATION         # Start menu system
└── SHUTDOWN-SYSTEM         # Clean exit

System Info:
├── System Name: "MINI CORE BANKING SYSTEM"
├── Version: "v1.0.0"
└── Build Date: "2026-01-13"
```

## Data Files

### Account File (`data/accounts.dat`)
```
Organization: Indexed Sequential
Key: Account ID (8 bytes, offset 0)
Record Size: 100 bytes
Access Pattern: Random read/write
Typical Size: 100 bytes × N accounts
```

### Ledger File (`data/ledger.dat`)
```
Organization: Sequential
Access: Append-only
Record Size: 120 bytes
Access Pattern: Sequential forward read, append
Typical Size: 120 bytes × N transactions
Growth: Continuous (never shrinks)
```

### Transaction File (`data/transactions.dat`)
```
Organization: Indexed Sequential
Primary Key: Transaction ID (12 bytes, offset 0)
Alternate Key: Account ID (8 bytes, offset 12)
Record Size: 150 bytes
Access Pattern: Random read/write, query by account
```

### Control File (`data/control.dat`)
```
Organization: Sequential
Records: 1 (singleton)
Record Size: 80 bytes
Access Pattern: Read entire, write entire
Purpose: Sequence number generation
```

## Build Process

```
build.sh
├── Check for GnuCOBOL (cobc)
├── Create bin/ directory
├── Create data/ directory
├── Compile COBOL modules
│   └── Create single executable
└── Report success/failure

Compilation:
Input: src/**/*.cob
Output: bin/banking_system
Flags: -x -Wall -O2
```

## Execution Flow

```
run.sh → bin/banking_system
    ↓
main.cob (MAIN-PROGRAM)
    ↓
Initialize (create data files if needed)
    ↓
menu.cob (MAIN-MENU-LOOP)
    ↓
User interaction
    ↓
Call core modules as needed
    ↓
Exit
```

## Code Statistics

```
Module               Lines   Functions   Purpose
--------------------|-------|-----------|---------------------------
account.cob         |  270  |     7     | Account lifecycle
ledger.cob          |  240  |     6     | Immutable audit log
transactions.cob    |  380  |     7     | Transaction processing
validation.cob      |  180  |     8     | Business rules
schema.cob          |   80  |     0     | Data definitions
files.cob           |  280  |    20     | File I/O
screens.cob         |  310  |    16     | Screen rendering
input.cob           |  260  |    15     | Input handling
menu.cob            |  450  |    10     | Navigation
main.cob            |  110  |     5     | Entry point
--------------------|-------|-----------|---------------------------
TOTAL               | 2560  |    94     | Full system

Documentation:
README.md           |  280 lines
ARCHITECTURE.md     |  480 lines
USAGE.md            |  390 lines
DESIGN_DECISIONS.md |  520 lines
PROJECT_STRUCTURE.md|  400 lines (this file)
--------------------|----------
TOTAL               | 2070 lines
```

## Dependencies

```
Required:
├── GnuCOBOL 3.1+ (cobc compiler)
├── Linux/Unix OS
└── Terminal (80×24 minimum)

No External Libraries:
├── No database
├── No web framework
├── No network libraries
└── Self-contained
```

## File Naming Conventions

```
Source Files:
├── *.cob       # COBOL source
├── *.sh        # Shell scripts
└── *.md        # Markdown documentation

Data Files:
└── *.dat       # Binary data files

Directories:
├── lowercase   # All directories
└── no spaces   # Filesystem compatibility
```

## Module Call Graph

```
main.cob
  └─→ menu.cob
        ├─→ screens.cob (rendering)
        ├─→ input.cob (user input)
        ├─→ account.cob
        │     ├─→ files.cob
        │     └─→ validation.cob
        ├─→ transactions.cob
        │     ├─→ account.cob
        │     ├─→ ledger.cob
        │     ├─→ files.cob
        │     └─→ validation.cob
        └─→ ledger.cob
              └─→ files.cob

Dependency Direction: Top → Bottom
No circular dependencies
Clear hierarchy
```

## Testing Structure (Not Implemented)

```
If tests were implemented:
tests/
├── unit/
│   ├── test_validation.cob
│   ├── test_account.cob
│   └── test_ledger.cob
├── integration/
│   ├── test_transactions.cob
│   └── test_files.cob
└── e2e/
    └── test_full_workflow.cob
```

## Conclusion

The project is organized into clear layers with well-defined responsibilities. Each module has a single purpose and dependencies flow in one direction. The structure supports maintainability, testability, and understanding of the system.

Total project size: ~2,560 lines of COBOL + ~2,070 lines of documentation = ~4,630 lines total, demonstrating a professional, well-documented portfolio piece.
