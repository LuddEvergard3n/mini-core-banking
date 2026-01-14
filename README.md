# Mini Core Banking System

Terminal-based core banking system implementation in COBOL.

## What This Is

A functional demonstration of core banking concepts implemented as a terminal application. Uses COBOL for business logic and file-based persistence. No web interfaces, no databases, just terminal and files.

## What This Is NOT

- Not production-ready
- Not a full banking system
- Not secure enough for real money
- Not a toy - implements real banking concepts correctly
- Not complete - focuses on core operations

## Why COBOL and Terminal

This project demonstrates:

1. **COBOL** - Still used in actual banking cores worldwide
2. **Terminal UI** - How internal banking systems actually look
3. **Domain Knowledge** - Understanding of banking operations, ledgers, and accounting
4. **System Design** - Separation of concerns, data integrity, audit trails

## Architecture

```
mini-core-banking/
├── src/
│   ├── core/          # Banking business logic
│   │   ├── account.cob       # Account management
│   │   ├── ledger.cob        # Immutable ledger (audit trail)
│   │   ├── transactions.cob  # Transaction processing
│   │   └── validation.cob    # Business rules
│   ├── storage/       # Persistence layer
│   │   ├── schema.cob        # Data record layouts
│   │   └── files.cob         # File I/O operations
│   └── ui/            # Terminal interface
│       ├── screens.cob       # Screen rendering
│       ├── input.cob         # Input handling
│       └── menu.cob          # Menu navigation
├── data/              # Data files (created at runtime)
└── bin/               # Compiled executables
```

## Features Implemented

### Core Banking
- **Accounts**: Create, query, block/unblock
- **Transactions**: Credit, debit, block funds
- **Ledger**: Immutable audit trail of all operations
- **Validations**: Business rules enforcement
- **Persistence**: File-based storage (indexed and sequential)

### Banking Concepts
- Available balance vs ledger balance
- Blocked funds (pre-authorization)
- Transaction atomicity
- Audit trail (every operation logged)
- Status management (ACTIVE/BLOCKED/CLOSED)

## Requirements

- **GnuCOBOL** (OpenCOBOL) compiler
- Linux/Unix terminal
- 80x24 minimum terminal size

### Installing GnuCOBOL

```bash
# Ubuntu/Debian
sudo apt-get install gnucobol

# Fedora
sudo dnf install gnucobol

# Arch
sudo pacman -S gnucobol

# macOS
brew install gnucobol
```

## Building

```bash
./build.sh
```

This compiles all COBOL modules and creates the executable in `bin/`.

## Running

```bash
./run.sh
```

Or directly:

```bash
./bin/banking_system
```

## Usage

The system presents a text-based menu interface:

1. **Load Account** - Select account for operations
2. **Create Account** - Create new account
3. **Operations Menu** (once account loaded):
   - Credit Account (deposit)
   - Debit Account (withdrawal)
   - Block Funds (pre-authorization)
   - View Ledger (transaction history)

All monetary values in BRL (R$).

## Technical Decisions

### Why File-Based Storage?

Real core banking systems often use mainframe file systems (VSAM). This mimics that approach:
- **Indexed files** for accounts (fast lookup by ID)
- **Sequential files** for ledger (append-only immutable log)
- **Control file** for sequence generation

### Why No Database?

1. Demonstrates file I/O in COBOL (relevant skill)
2. Simpler deployment (no DB setup)
3. More realistic for mainframe context
4. Forces proper data structure design

### Why Immutable Ledger?

Real banking systems maintain immutable transaction logs for:
- Audit compliance
- Reconciliation
- Regulatory requirements
- Dispute resolution

The ledger records EVERY operation with:
- Timestamp
- Type (credit/debit/block/unblock)
- Amount
- Balance before
- Balance after
- Description

### Why Separation of Concerns?

The codebase is split into distinct layers:

- **Core**: Pure banking logic (no I/O, no UI)
- **Storage**: Data persistence (no business logic)
- **UI**: User interface (no business logic, no persistence)

This makes the code:
- Testable
- Maintainable
- Professional
- Similar to real enterprise systems

## Limitations

### Current Implementation

- Single-user (no concurrency)
- Limited transaction types (no transfers between accounts)
- Simplified validation rules
- No encryption
- No authentication/authorization
- Limited error recovery
- Ledger integrity check not implemented
- No reporting/analytics

### Why These Limitations?

This is a portfolio/educational project. The goal is to demonstrate:
- Understanding of core banking concepts
- Ability to structure non-trivial systems
- Clean code in an uncommon language
- Domain knowledge

Not to build a complete banking system.

## File Formats

### Account Record (100 bytes)
- Account ID (8 digits)
- Holder name (30 chars)
- Account type (10 chars)
- Status (8 chars)
- Balance (packed decimal, 15 digits)
- Blocked amount (packed decimal, 15 digits)
- Created/updated timestamps
- Filler

### Ledger Record (120 bytes)
- Ledger ID (10 digits)
- Account ID (8 digits)
- Transaction ID (12 digits)
- Timestamp (14 chars, YYYYMMDDHHMMSS)
- Type (8 chars)
- Amount (packed decimal)
- Balance before/after (packed decimal)
- Description (50 chars)
- Filler

### Transaction Record (150 bytes)
- Transaction ID (12 digits)
- Account ID (8 digits)
- Timestamp
- Type
- Amount
- Status
- Description (80 chars)
- Filler

All use fixed-length records for predictable file I/O.

## Code Quality

### What Was Prioritized

- **Readability**: Code is heavily commented
- **Structure**: Clear separation of concerns
- **Correctness**: Banking rules properly implemented
- **Realism**: Mimics actual banking system behavior

### What Was NOT Prioritized

- Performance optimization
- Feature completeness
- Production-grade error handling
- Advanced COBOL features

This is educational code, not production code.

## Learning Resources

If you want to understand this code:

1. **COBOL Syntax**: GnuCOBOL documentation
2. **Banking Concepts**: Search "double-entry bookkeeping", "general ledger"
3. **File Processing**: COBOL file I/O (INDEXED, SEQUENTIAL)
4. **Transaction Processing**: ACID properties, atomicity

## Future Enhancements

Possible extensions (not implemented):

- Transfer between accounts
- Transaction reversal
- Interest calculation
- Statement generation
- Multi-currency support
- Batch processing
- Concurrency control
- Database backend option
- REST API wrapper
- Proper ncurses integration

## License

This is portfolio/educational code. Use freely for learning.

## Contact

This is a portfolio project demonstrating:
- Systems programming skills
- Domain knowledge (banking)
- Ability to work with legacy technologies
- Clean code practices

Not intended for production use.

## Disclaimer

This is NOT financial software. Do not use with real money. Do not deploy to production. This is for educational and portfolio purposes only.

---

**Built as a portfolio piece to demonstrate understanding of:**
- Core banking operations
- COBOL programming
- System architecture
- Domain-driven design
- Professional software development practices
