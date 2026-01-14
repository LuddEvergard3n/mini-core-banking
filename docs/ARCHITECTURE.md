# Technical Architecture

This document explains the technical decisions and architecture of the Mini Core Banking System.

## System Overview

### Purpose
Demonstrate understanding of core banking systems through a working implementation in COBOL with terminal UI.

### Non-Goals
- Production deployment
- Complete feature set
- Web interface
- Distributed systems

## Architecture Layers

### 1. Core Layer (`src/core/`)

Pure business logic. No I/O, no UI dependencies.

#### `account.cob`
**Responsibility**: Account lifecycle management

Operations:
- `CREATE-ACCOUNT`: Generate new account with unique ID
- `GET-ACCOUNT`: Retrieve account by ID
- `UPDATE-ACCOUNT-BALANCE`: Persist balance changes
- `BLOCK-ACCOUNT`: Change status to BLOCKED
- `UNBLOCK-ACCOUNT`: Change status to ACTIVE

Key Design Decisions:
- Account IDs are sequential (10000000-99999999)
- Balance and blocked amounts stored separately
- Status changes are audited via timestamp updates
- No direct balance manipulation - must go through transactions

#### `ledger.cob`
**Responsibility**: Immutable transaction ledger

Operations:
- `CREATE-LEDGER-ENTRY`: Append new entry (no updates/deletes)
- `QUERY-LEDGER-BY-ACCOUNT`: Retrieve entries for account
- `GET-LAST-LEDGER-ENTRY`: Get most recent entry

Key Design Decisions:
- **Append-only**: Ledger entries can never be modified or deleted
- **Complete audit trail**: Every financial operation creates entry
- **Balance tracking**: Records balance before and after each operation
- **Sequential file**: Guarantees append order
- **Integrity check**: Placeholder for reconciliation logic

Why Immutable?
- Regulatory compliance
- Audit requirements
- Dispute resolution
- Reconciliation
- Trust/transparency

#### `transactions.cob`
**Responsibility**: Financial transaction processing

Operations:
- `PROCESS-CREDIT`: Add funds to account
- `PROCESS-DEBIT`: Remove funds from account
- `PROCESS-BLOCK-FUNDS`: Lock funds (reduce available balance)
- `PROCESS-UNBLOCK-FUNDS`: Release locked funds

Transaction Flow:
1. Validate inputs
2. Read account
3. Validate business rules
4. Update account balance
5. Create ledger entry
6. Log transaction record
7. Return status

Key Design Decisions:
- **Atomic operations**: All steps succeed or all fail
- **Validation first**: Check before any state change
- **Audit trail**: Every transaction creates ledger entry
- **Transaction log**: Separate log of transaction status
- **No rollback mechanism**: Simplified for demo (production would need this)

#### `validation.cob`
**Responsibility**: Business rules enforcement

Validations:
- Amount ranges (min/max)
- Account status checks
- Sufficient balance checks
- Account ID format
- Combined validations for operations

Key Design Decisions:
- **Pure functions**: No side effects, just validation
- **Explicit rules**: All limits configurable in code
- **Clear error messages**: Detailed failure reasons
- **Separation of concerns**: Validation logic isolated from execution

Business Rules Implemented:
- Minimum credit: R$ 0.01
- Maximum credit: R$ 999,999,999,999.99
- Minimum debit: R$ 0.01
- Maximum debit: R$ 999,999,999,999.99
- Account ID range: 10000000-99999999
- Status checks: Only ACTIVE accounts can transact

### 2. Storage Layer (`src/storage/`)

Handles all persistence. No business logic.

#### `schema.cob`
**Responsibility**: Data structure definitions

Defines:
- Account record layout (100 bytes)
- Ledger record layout (120 bytes)
- Transaction record layout (150 bytes)
- Control record layout (80 bytes)

Key Design Decisions:
- **Fixed-length records**: Predictable file I/O
- **Packed decimal**: Efficient storage for monetary values (COMP-3)
- **88-level conditions**: Readable status checks
- **Filler fields**: Reserved for future expansion

Why Fixed-Length?
- Simplifies file access
- Enables indexed access by position
- Matches mainframe file conventions
- Predictable storage requirements

#### `files.cob`
**Responsibility**: File I/O operations

File Types:
1. **Account File**: Indexed by Account ID
   - Fast lookup
   - Update in place
   
2. **Ledger File**: Sequential append-only
   - Immutable log
   - Ordered by time
   
3. **Transaction File**: Indexed by Transaction ID
   - Query by ID
   - Alternate index by Account ID
   
4. **Control File**: Single record
   - Sequence number generation

Operations:
- Open/close files
- Read/write/update records
- Handle file status codes
- Initialize files on first use

Key Design Decisions:
- **File status checking**: Every operation returns status
- **Automatic initialization**: Creates files if missing
- **Error handling**: File status codes mapped to operation status
- **No caching**: Immediate persistence

### 3. UI Layer (`src/ui/`)

Terminal interface. No business logic.

#### `screens.cob`
**Responsibility**: Screen rendering

Provides:
- Header rendering (system title)
- Footer rendering (status messages)
- Account info panel
- Menu rendering
- Ledger display
- Message display (error/success/info)
- Input prompts

Key Design Decisions:
- **ANSI escape codes**: For colors and cursor control
- **Fixed layout**: Consistent screen structure
- **Minimal colors**: Only for status indicators
- **Dense information**: Professional, not flashy
- **No ncurses dependency**: Uses basic ANSI (portable)

Why ANSI Instead of ncurses?
- Simpler implementation
- Fewer dependencies
- Sufficient for demo purposes
- More portable

#### `input.cob`
**Responsibility**: Input handling and validation

Input Types:
- Numeric (integers)
- Decimal (money amounts)
- Text (descriptions, names)
- Menu choices (1-99)
- Account IDs (8 digits)
- Confirmations (Y/N)

Key Design Decisions:
- **Type-safe input**: Different functions for different types
- **Format validation**: Check before returning
- **Error flags**: Explicit success/failure indicators
- **No retries in handler**: Caller decides retry logic

#### `menu.cob`
**Responsibility**: Navigation and coordination

Menus:
1. Main Menu (account selection)
2. Operations Menu (transactions)
3. Transaction screens (credit/debit/block)
4. Ledger view

State Machine:
- MAIN → Load account or create account
- OPERATIONS → Perform transactions
- TRANSACTION → Execute and return to operations
- EXIT → Shutdown

Key Design Decisions:
- **State-based navigation**: Clear flow control
- **Account context**: Operations require loaded account
- **Confirmation steps**: For critical operations
- **Status messages**: Clear feedback after each action

### 4. Main Program (`src/main.cob`)

Entry point and orchestration.

Responsibilities:
- System initialization
- Data directory setup
- Control file initialization
- Application startup
- Clean shutdown

Key Design Decisions:
- **Fail-fast initialization**: Stop if setup fails
- **Banner display**: Show version and build info
- **Graceful shutdown**: Clean up and inform user

## Data Flow

### Account Creation Flow
```
User → Menu → Input → Account → Files
                ↓
            Control (next ID)
                ↓
            Account File
```

### Transaction Flow
```
User → Menu → Input → Transactions → Validation
                           ↓              ↓
                       Account ← (business rules)
                           ↓
                       Ledger
                           ↓
                       Files
```

### Query Flow
```
User → Menu → Account → Files → Display
```

## Sequence Number Generation

Uses a control file to maintain sequence counters:

```
Control File (single record):
- Last Account ID: 10000000+
- Last Ledger ID: 1000000000+
- Last Transaction ID: 100000000000+

Process:
1. Read control record
2. Increment counter
3. Update control record
4. Return new ID
```

Why Not Database Sequences?
- No database dependency
- Demonstrates file-based state management
- Simpler deployment
- More realistic for mainframe context

## File Organization

```
data/
├── accounts.dat      (indexed)
├── ledger.dat        (sequential)
├── transactions.dat  (indexed)
└── control.dat       (sequential, single record)
```

### Account File Layout
```
Organization: Indexed Sequential
Key: Account ID (8 bytes)
Record Size: 100 bytes
Access: Random read/write
```

### Ledger File Layout
```
Organization: Sequential
Access: Append only (no updates/deletes)
Record Size: 120 bytes
Reading: Sequential forward only
```

### Transaction File Layout
```
Organization: Indexed Sequential
Primary Key: Transaction ID (12 bytes)
Alternate Key: Account ID (8 bytes, duplicates allowed)
Record Size: 150 bytes
Access: Random read/write
```

### Control File Layout
```
Organization: Sequential
Records: 1 (single record)
Record Size: 80 bytes
Access: Read entire, rewrite entire
```

## Error Handling Strategy

### File Operations
```
Status Code → Operation Status → User Message
```

Standard codes:
- `00`: Success
- `10`: End of file
- `23`: Record not found
- `22`: Duplicate key
- `35`: File not found
- `99`: General error

### Transaction Operations
```
Input → Validation → Business Rules → Execution → Status
   ↓         ↓             ↓              ↓         ↓
 Invalid → Error     Violation → Error  Failed → Logged
```

### Validation Strategy
- Validate format first (syntax)
- Then validate business rules (semantics)
- Return explicit error messages
- No exceptions (COBOL doesn't have them)

## Concurrency

### Current Implementation
**None.** Single-user system.

### Production Requirements (Not Implemented)
Would need:
- File locking
- Record locking
- Transaction isolation
- Retry logic
- Deadlock detection

### Why Not Implemented?
- Complexity vs demo value
- Would require OS-specific locking
- Beyond scope of portfolio piece

## Performance Considerations

### What Was Optimized
- Fixed-length records (predictable I/O)
- Indexed files for accounts (O(log n) lookup)
- Sequential ledger append (O(1) write)

### What Was NOT Optimized
- Ledger queries (sequential scan - O(n))
- No caching
- No batch processing
- File opened/closed per operation

### Why?
Premature optimization avoided. Correctness > performance for demo.

## Security Considerations

### What's Missing (Intentionally)
- No authentication
- No authorization
- No encryption
- No audit of who performed operations
- No password protection
- No SSL/TLS (no network layer)

### Why?
This is a portfolio piece demonstrating:
- Banking logic
- System design
- COBOL programming

Security implementation would:
- Add complexity without educational value
- Require external dependencies
- Obscure the core concepts

**Explicit disclaimer**: Not for production use.

## Testing Strategy

### What Should Be Tested (Not Implemented)
1. **Unit Tests**: Each module independently
2. **Integration Tests**: Module interactions
3. **Business Logic Tests**: Validation rules
4. **File I/O Tests**: Persistence operations
5. **End-to-End Tests**: Complete workflows

### Why Not Implemented?
- COBOL testing frameworks not standard
- Would double project size
- Focus on demonstrating concepts
- Manual testing sufficient for demo

### Manual Test Cases
1. Create account → Verify file created
2. Credit account → Verify balance updated
3. Debit account → Verify insufficient funds rejected
4. Block funds → Verify available balance reduced
5. Ledger → Verify all operations logged

## Deployment

### Requirements
- GnuCOBOL compiler
- Linux/Unix environment
- Terminal (80x24 minimum)
- File system permissions

### Installation
```bash
git clone <repo>
cd mini-core-banking
./build.sh
./run.sh
```

No external services required. Self-contained.

## Limitations and Trade-offs

### Conscious Trade-offs
1. **File-based vs Database**: Simplicity, relevance to mainframe context
2. **No concurrency**: Complexity not justified for demo
3. **Limited features**: Focus on core concepts
4. **Simplified error handling**: Adequate for educational purposes
5. **No automated tests**: Manual testing sufficient
6. **ANSI vs ncurses**: Fewer dependencies, adequate functionality

### Known Issues
1. Ledger query is O(n) - would need indexing
2. No rollback on partial failure - would need transaction manager
3. File locking not implemented - would need OS-specific code
4. No data validation beyond format - would need business logic expansion
5. Single currency only - would need currency tables

### Why These Are Acceptable
This is a **portfolio demonstration**, not production software. The goal is to show:
- Understanding of banking concepts
- Ability to structure complex systems
- Code quality and organization
- Domain knowledge

Not to build a production-ready system.

## Comparison to Real Banking Systems

### Similarities
- Immutable ledger
- Transaction atomicity
- Account status management
- Balance tracking
- Audit trail
- File-based persistence (mainframe VSAM)
- COBOL implementation

### Differences
- Real systems have:
  - Distributed transactions
  - Sophisticated authorization
  - Regulatory reporting
  - Multi-currency support
  - Interest calculation
  - Fee processing
  - Customer relationship management
  - External integrations
  - Batch processing
  - High availability
  - Disaster recovery

### Educational Value
This project demonstrates the **core concepts** without the enormous complexity of production banking systems. It's a learning tool, not a product.

## Future Extensions

Possible additions (not planned):

1. **Database Backend**: PostgreSQL/SQLite option
2. **REST API**: HTTP interface for integration
3. **Batch Processing**: End-of-day processing
4. **Reports**: Account statements, transaction summaries
5. **Transfer**: Between accounts
6. **Currency**: Multi-currency support
7. **Interest**: Calculation and posting
8. **Fees**: Transaction fees
9. **Reversals**: Transaction reversal logic
10. **Authentication**: User login system

Each would be significant additional work.

## Conclusion

This project demonstrates:

1. **Domain Knowledge**: Understanding of banking operations
2. **System Design**: Clean architecture, separation of concerns
3. **COBOL Proficiency**: Real-world business logic in COBOL
4. **Data Management**: File-based persistence, record layouts
5. **Professional Approach**: Documentation, structure, clarity

It is explicitly **not** production software, but a portfolio piece showing technical and domain expertise.
