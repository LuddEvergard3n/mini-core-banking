# Technical Design Decisions

This document explains the "why" behind key technical choices in the Mini Core Banking System.

## Core Technology Choices

### Why COBOL?

**Decision**: Use COBOL as the primary implementation language.

**Rationale**:
1. **Relevance**: COBOL still powers 70%+ of banking transaction systems worldwide
2. **Portfolio Value**: Demonstrates ability to work with legacy systems
3. **Domain Fit**: COBOL was designed for business logic and financial applications
4. **Differentiation**: Most portfolios show Python/JavaScript - COBOL stands out
5. **Legitimacy**: Shows serious approach to banking systems (not just trendy tech)

**Trade-offs**:
- Smaller developer community
- Fewer modern tooling options
- Perceived as "old" by some
- Harder to find reviewers who know COBOL

**Why Worth It**: Banking recruiters and technical architects will recognize this as authentic experience with real banking technology.

### Why Terminal UI?

**Decision**: Text-based interface, no web/GUI.

**Rationale**:
1. **Realism**: Internal banking systems look like this
2. **Focus**: Forces attention on logic, not presentation
3. **Simplicity**: No framework dependencies or complexity
4. **Authenticity**: Shows understanding of actual banking operations centers
5. **Accessibility**: Works over SSH, no browser needed

**Trade-offs**:
- Less visually impressive
- Requires terminal comfort
- Limited to keyboard interaction
- No mouse support

**Why Worth It**: Demonstrates understanding of real banking systems vs. consumer-facing apps.

### Why File-Based Storage?

**Decision**: Use indexed and sequential files, not a database.

**Rationale**:
1. **Mainframe Context**: VSAM files are standard in mainframe banking
2. **Simplicity**: No database server setup required
3. **Educational**: Demonstrates file I/O skills
4. **Portability**: Works anywhere with filesystem
5. **Realistic**: Closer to actual mainframe storage patterns

**Trade-offs**:
- No ACID transactions across files
- Manual indexing
- Limited query capability
- No concurrent access
- Manual backup/recovery

**Why Worth It**: Shows understanding of mainframe concepts and ability to work without modern conveniences.

## Architecture Decisions

### Why Three-Layer Architecture?

**Decision**: Separate Core, Storage, and UI layers.

**Rationale**:
1. **Maintainability**: Changes in one layer don't affect others
2. **Testability**: Can test business logic without UI
3. **Professionalism**: Industry standard approach
4. **Flexibility**: Could swap UI or storage without touching core
5. **Clarity**: Clear responsibilities for each module

**Implementation**:
```
UI Layer (screens, input, menu)
    ↓ calls
Core Layer (account, transactions, ledger, validation)
    ↓ calls
Storage Layer (files, schema)
```

**Why Worth It**: Demonstrates understanding of software architecture principles.

### Why Immutable Ledger?

**Decision**: Ledger entries can never be modified or deleted.

**Rationale**:
1. **Audit Trail**: Required for financial compliance
2. **Integrity**: Prevents tampering with historical data
3. **Reconciliation**: Supports financial reconciliation processes
4. **Debugging**: Complete history aids troubleshooting
5. **Trust**: Fundamental to financial systems

**Implementation**:
- Sequential file (append-only)
- No update or delete operations
- Each entry includes balance before/after
- Timestamp on every entry

**Trade-offs**:
- Storage grows continuously
- No way to "fix" incorrect entries (must reverse)
- Query performance degrades over time

**Why Worth It**: This is a core banking concept. Demonstrates domain knowledge.

### Why Separation of Balance Types?

**Decision**: Track balance, blocked amount, and available balance separately.

**Rationale**:
1. **Realism**: Real banking systems do this
2. **Pre-authorization**: Supports holds/blocks (hotel, gas pump, etc.)
3. **Accuracy**: Ledger balance vs available balance distinction
4. **Compliance**: Regulatory requirements for fund availability

**Implementation**:
```
Ledger Balance = Total in account
Blocked Amount = Funds on hold
Available Balance = Ledger - Blocked
```

**Example**:
```
Account has R$ 1000
Hotel blocks R$ 200
Ledger Balance: R$ 1000 (unchanged)
Blocked: R$ 200
Available: R$ 800 (can't withdraw the R$ 200)
```

**Why Worth It**: Shows understanding of real-world banking operations beyond simple deposit/withdrawal.

## Data Design Decisions

### Why Fixed-Length Records?

**Decision**: All records are fixed length (100, 120, 150 bytes).

**Rationale**:
1. **Predictability**: Record position = offset * record_size
2. **Performance**: Direct access without parsing
3. **Indexing**: Enables binary search in indexed files
4. **Mainframe Style**: How VSAM files work
5. **Simplicity**: No delimiters or escape sequences

**Trade-offs**:
- Wasted space for short data
- Limited flexibility
- Fixed maximum field lengths

**Why Worth It**: Authentic to mainframe file systems. Good for demonstration.

### Why Packed Decimal (COMP-3)?

**Decision**: Use packed decimal for monetary amounts.

**Rationale**:
1. **Precision**: No floating point errors
2. **Storage**: More compact than display format
3. **Performance**: Hardware support on mainframes
4. **Standard**: Industry standard for financial data
5. **Accuracy**: Critical for money calculations

**Example**:
```
Display:  "1234.56" = 7 bytes
COMP-3:   0x01234.56 = 4 bytes
No rounding errors from binary floating point
```

**Why Worth It**: Shows understanding of financial data representation.

### Why Sequential Account IDs?

**Decision**: Generate account IDs sequentially (10000001, 10000002, ...).

**Rationale**:
1. **Simplicity**: Easy to implement
2. **Uniqueness**: Guaranteed unique
3. **Deterministic**: Testable and predictable
4. **Readability**: Easy to remember and verify
5. **No Collisions**: No UUID complexity

**Implementation**:
- Control file stores last ID
- Read, increment, write back
- Start at 10000000

**Trade-offs**:
- Not cryptographically secure
- Reveals number of accounts
- Sequential = predictable

**Why Worth It**: Sufficient for demo. Real systems might use more complex schemes.

## Security Non-Decisions

### Why No Authentication?

**Decision**: No login, passwords, or user management.

**Rationale**:
1. **Scope**: Out of scope for core banking demonstration
2. **Complexity**: Would double project size
3. **Focus**: Emphasis on banking logic, not security
4. **Honesty**: Explicit disclaimer this is not production code

**What's Missing**:
- User accounts
- Passwords
- Session management
- Authorization
- Audit of who performed actions

**Why Acceptable**: This is educational code demonstrating banking concepts, not production security. Clearly documented as limitation.

### Why No Encryption?

**Decision**: Data stored in plaintext.

**Rationale**:
1. **Scope**: Security not the focus
2. **Complexity**: Encryption adds significant complexity
3. **Visibility**: Want data to be inspectable for learning
4. **Honesty**: Clear this is not production-ready

**Why Acceptable**: Explicit disclaimer. Focus is on banking logic demonstration.

## Performance Non-Optimizations

### Why No Caching?

**Decision**: Read from disk every time, no in-memory cache.

**Rationale**:
1. **Simplicity**: Easier to reason about
2. **Correctness**: Always fresh data
3. **Single User**: No contention anyway
4. **Educational**: Shows file I/O clearly

**Trade-off**: Slower performance.

**Why Acceptable**: Performance not critical for portfolio demo.

### Why Sequential Ledger Scan?

**Decision**: Reading ledger requires full sequential scan.

**Rationale**:
1. **Simplicity**: No index structure needed
2. **Append-Only**: Sequential access natural
3. **Small Scale**: Acceptable for demo volumes

**Trade-off**: O(n) query time for ledger entries.

**Why Acceptable**: Could add indexing later. Demonstrates concept correctly.

## What Was NOT Implemented (Intentionally)

### Concurrent Access

**Not Implemented**: File locking, record locking, transaction isolation.

**Why**: 
- Significant complexity
- OS-specific implementation
- Beyond scope of single-user demo
- Would obscure core concepts

**Real System Would Need**: Pessimistic or optimistic locking, deadlock detection, retry logic.

### Transaction Rollback

**Not Implemented**: Undo partial transactions on failure.

**Why**:
- Adds complexity
- Requires transaction log
- Compensation logic needed
- Demonstrated concept exists without implementation

**Real System Would Need**: Write-ahead log, compensating transactions, rollback procedures.

### Database Backend

**Not Implemented**: SQL database option.

**Why**:
- File-based sufficient for demo
- Closer to mainframe reality
- No external dependencies
- Forces understanding of low-level I/O

**Could Add Later**: As alternative storage implementation.

### Multi-Currency

**Not Implemented**: Support for multiple currencies.

**Why**:
- Adds complexity (exchange rates, rounding)
- Not core to demonstration
- Would need currency tables
- BRL sufficient for Brazilian context

**Real System Would Need**: Currency codes, exchange rate tables, conversion logic.

### Interest Calculation

**Not Implemented**: Interest accrual and posting.

**Why**:
- Time-based processing complexity
- Requires date arithmetic
- Would need rate tables
- Beyond transaction processing demo

**Real System Would Need**: Interest calculation engine, posting schedules, rate management.

### Reporting

**Not Implemented**: Account statements, transaction summaries, reports.

**Why**:
- Not core to transaction processing
- Would require formatting logic
- PDF generation complexity
- Query optimization needed

**Could Add Later**: As separate reporting module.

## What SHOULD Be Implemented (But Isn't)

If this were production code, it would need:

1. **Authentication/Authorization**
   - User login
   - Role-based access
   - Session management
   - Audit of who did what

2. **Encryption**
   - Data at rest
   - Data in transit
   - Key management

3. **Concurrency Control**
   - File/record locking
   - Transaction isolation
   - Deadlock handling

4. **Error Recovery**
   - Rollback on failure
   - Compensating transactions
   - Automatic retry

5. **Backup/Recovery**
   - Automated backups
   - Point-in-time recovery
   - Disaster recovery

6. **Monitoring**
   - Performance metrics
   - Error logging
   - Alerting

7. **Compliance**
   - Regulatory reporting
   - Audit trails with user IDs
   - Data retention policies

8. **Testing**
   - Unit tests
   - Integration tests
   - Performance tests
   - Security tests

9. **Documentation**
   - API documentation
   - Operational procedures
   - Disaster recovery plans

10. **Scalability**
    - Batch processing
    - Distributed transactions
    - Replication
    - Sharding

## Decision Summary

### Prioritized
- **Banking Domain Correctness**: Proper ledger, balance types, validations
- **Code Organization**: Clean architecture, separation of concerns
- **Documentation**: Extensive explanation of choices
- **Authenticity**: Realistic banking concepts

### De-Prioritized
- **Production Readiness**: Security, scalability, monitoring
- **Feature Completeness**: Transfers, interest, reporting
- **Performance Optimization**: Caching, indexing
- **Modern Tooling**: Web UI, REST API, database

## Lessons Learned

What this project demonstrates:

1. **Technical Judgment**: Knowing what to include vs. exclude
2. **Domain Knowledge**: Understanding banking operations
3. **Architecture Skills**: Clean layered design
4. **Pragmatism**: Production-quality code structure without production features
5. **Communication**: Explaining and documenting decisions

## Conclusion

Every technical decision was made consciously with the goal:

**Demonstrate understanding of core banking systems through working, well-structured code that could serve as a foundation for a real system, while being honest about what's missing.**

The result: A portfolio piece that shows technical skill, domain knowledge, and professional judgment.
