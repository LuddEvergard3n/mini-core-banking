# Usage Guide

Practical guide to using the Mini Core Banking System.

## First Run

```bash
$ ./build.sh
Building...
Build completed successfully!

$ ./run.sh
Starting Mini Core Banking System...

================================================
    MINI CORE BANKING SYSTEM - DEMO
    Terminal-Based Core Banking
================================================

DISCLAIMER:
This is a simplified demo version.
Full implementation includes:
  - File-based persistence
  - Complete transaction ledger
  - Validation and business rules
  - Multi-account management

Press ENTER to continue...
```

## Creating Your First Account

```
=== MAIN MENU ===

  1 - Load Account
  2 - Create New Account
  9 - Exit

Enter option: 2

CREATE NEW ACCOUNT
------------------

Enter holder name: John Smith

Account created successfully!
Account ID: 10000001

Press ENTER to continue...
```

Key Points:
- Account IDs are auto-generated (sequential)
- Initial balance is R$ 0.00
- Account status is ACTIVE
- Account type is CHECKING

## Loading an Account

```
=== MAIN MENU ===

  1 - Load Account
  2 - Create New Account
  9 - Exit

Enter option: 1

Load Account
------------
Enter Account ID: 10000001
Account loaded successfully.
Press ENTER to continue...
```

Now you're in the operations menu for this account.

## Viewing Account Information

Once an account is loaded:

```
=== OPERATIONS MENU ===

Account ID:      10000001
Holder:          John Smith
Status:          ACTIVE
Balance:      R$       0.00
Blocked:      R$       0.00
Available:    R$       0.00

  1 - Credit Account
  2 - Debit Account
  3 - Block Funds
  4 - View Ledger
  5 - Back to Main Menu

Enter option: 
```

Information displayed:
- **Balance**: Total amount in account (ledger balance)
- **Blocked**: Amount currently blocked (pre-authorized)
- **Available**: Balance - Blocked (what can be withdrawn)

## Making a Deposit (Credit)

```
Enter option: 1

=== CREDIT ACCOUNT ===

Enter amount: 1000.00
Enter description: Initial deposit

Credit processed successfully!
Transaction ID: 100000000001

Press ENTER to continue...
```

Transaction flow:
1. System validates amount (must be > 0)
2. System validates account status (must be ACTIVE)
3. Balance is increased
4. Ledger entry is created
5. Transaction is logged

## Making a Withdrawal (Debit)

```
Enter option: 2

=== DEBIT ACCOUNT ===

Enter amount: 250.00
Enter description: ATM withdrawal

Debit processed successfully!
Transaction ID: 100000000002

Press ENTER to continue...
```

Validation:
- Amount must be positive
- Account must be ACTIVE
- Available balance must be sufficient

Example of insufficient funds:

```
Enter amount: 5000.00
Enter description: Large withdrawal

ERROR: Insufficient funds.

Press ENTER to continue...
```

## Blocking Funds

Used for pre-authorization (e.g., hotel holds):

```
Enter option: 3

=== BLOCK FUNDS ===

Enter amount to block: 100.00
Enter description: Hotel authorization

Funds blocked successfully!
Transaction ID: 100000000003

Press ENTER to continue...
```

Effect on balances:
- **Balance**: Unchanged (R$ 750.00)
- **Blocked**: Increased (R$ 100.00)
- **Available**: Decreased (R$ 650.00)

The R$ 100 is still in the account but cannot be withdrawn.

## Understanding Balance Types

Example account state:

```
Balance:      R$     750.00
Blocked:      R$     100.00
Available:    R$     650.00
```

Explanation:
- **Ledger Balance (Balance)**: Total amount in account
  - What shows in official accounting
  - Never goes below zero
  
- **Blocked Amount**: Funds reserved
  - Cannot be withdrawn
  - Can be released later
  - Common for pre-authorizations
  
- **Available Balance**: What can be withdrawn
  - Balance - Blocked
  - This is checked for debit operations

Real-world analogy:
- You have R$ 750 in your account
- Hotel put a R$ 100 hold on your card
- You can only withdraw R$ 650

## Transaction Types

### Credit (Deposit)
- Increases balance
- Increases available
- No validation of source
- Always succeeds if account is ACTIVE

### Debit (Withdrawal)
- Decreases balance
- Decreases available
- Validates sufficient available balance
- Can fail if insufficient funds

### Block (Pre-authorization)
- Balance unchanged
- Blocked increased
- Available decreased
- Validates sufficient available balance

### Unblock (Release pre-authorization)
- Balance unchanged
- Blocked decreased
- Available increased
- Validates sufficient blocked amount

Note: Unblock not implemented in demo, but structure is ready.

## Ledger (Transaction History)

The ledger is an immutable log of ALL operations.

Each entry contains:
- Ledger ID (unique)
- Account ID
- Transaction ID
- Timestamp (YYYYMMDDHHMMSS)
- Type (CREDIT/DEBIT/BLOCK/UNBLOCK)
- Amount
- Balance before operation
- Balance after operation
- Description

Example ledger entries:

```
Ledger ID  | Timestamp      | Type   | Amount     | Balance After
-----------|----------------|--------|------------|---------------
1000000001 | 20260113143022 | CREDIT | +1000.00   | 1000.00
1000000002 | 20260113143145 | DEBIT  |  -250.00   |  750.00
1000000003 | 20260113143301 | BLOCK  |  -100.00*  |  750.00
```

*Note: BLOCK doesn't change ledger balance, but affects available balance.

Why immutable?
- Audit requirements
- Regulatory compliance
- Dispute resolution
- Reconciliation
- Cannot be tampered with

## Account Status

### ACTIVE
- Normal operating status
- All operations allowed
- Default status for new accounts

### BLOCKED
- Account is frozen
- NO transactions allowed
- Balance preserved
- Can be unblocked

### CLOSED
- Account permanently closed
- NO transactions allowed
- Cannot be reopened
- Balance should be zero

Status transitions:
```
ACTIVE → BLOCKED → ACTIVE (can cycle)
ACTIVE → CLOSED (irreversible)
BLOCKED → CLOSED (irreversible)
```

## Error Scenarios

### Invalid Account ID
```
Enter Account ID: 12345
Invalid account ID format.
```

Account IDs must be exactly 8 digits (10000000-99999999).

### Account Not Found
```
Enter Account ID: 99999999
Account not found.
```

### Insufficient Funds
```
Enter amount: 10000.00
ERROR: Insufficient funds.
```

### Invalid Amount
```
Enter amount: -100
Invalid amount. Operation cancelled.
```

Amounts must be:
- Positive (> 0)
- Within system limits
- Properly formatted decimal (2 places max)

### Blocked Account
```
=== CREDIT ACCOUNT ===
ERROR: Account is blocked.
```

No operations allowed on blocked accounts.

## Data Files

After running, you'll see:

```
data/
├── accounts.dat
├── ledger.dat
├── transactions.dat
└── control.dat
```

These are binary files. Do NOT edit manually.

### Backup
To backup your data:
```bash
tar -czf backup.tar.gz data/
```

### Reset
To start fresh:
```bash
rm -rf data/
./run.sh  # Will recreate files
```

## Session Example

Complete session from start to finish:

```bash
$ ./run.sh

=== MAIN MENU ===
Enter option: 2

CREATE NEW ACCOUNT
Enter holder name: Alice
Account created successfully!
Account ID: 10000001

=== MAIN MENU ===
Enter option: 1

Load Account
Enter Account ID: 10000001
Account loaded successfully.

=== OPERATIONS MENU ===
Account ID:      10000001
Holder:          Alice
Status:          ACTIVE
Balance:      R$       0.00
Blocked:      R$       0.00
Available:    R$       0.00

Enter option: 1

=== CREDIT ACCOUNT ===
Enter amount: 500.00
Enter description: Initial deposit
Credit processed successfully!
Transaction ID: 100000000001

=== OPERATIONS MENU ===
Balance:      R$     500.00
Available:    R$     500.00

Enter option: 2

=== DEBIT ACCOUNT ===
Enter amount: 150.00
Enter description: Withdrawal
Debit processed successfully!
Transaction ID: 100000000002

=== OPERATIONS MENU ===
Balance:      R$     350.00
Available:    R$     350.00

Enter option: 3

=== BLOCK FUNDS ===
Enter amount to block: 50.00
Enter description: Restaurant hold
Funds blocked successfully!
Transaction ID: 100000000003

=== OPERATIONS MENU ===
Balance:      R$     350.00
Blocked:      R$      50.00
Available:    R$     300.00

Enter option: 5

=== MAIN MENU ===
Enter option: 9

System shutdown complete.
```

## Tips

1. **Account IDs**: Write them down, you'll need them
2. **Amounts**: Use decimal point (100.50) not comma
3. **Descriptions**: Keep them short and clear
4. **Blocked funds**: Remember to "unblock" in real systems
5. **Ledger**: Every operation is logged permanently

## Troubleshooting

### Build fails
```
ERROR: GnuCOBOL (cobc) not found.
```
Solution: Install GnuCOBOL (see README)

### Binary not found
```
ERROR: Binary not found. Please run ./build.sh first.
```
Solution: Run `./build.sh` before `./run.sh`

### File permission errors
```
ERROR: Cannot initialize account file
```
Solution: Check permissions on `data/` directory

### Corrupted data files
If data files are corrupted:
```bash
rm -rf data/
./run.sh  # Fresh start
```

## Limitations to Be Aware Of

1. **Single user**: Only one person at a time
2. **No transfers**: Can't move money between accounts
3. **No unblock UI**: Block implemented, unblock needs UI
4. **Simplified validation**: Real banks have more rules
5. **No authentication**: Anyone can access any account
6. **No reporting**: No statements or summaries
7. **No interest**: Balances don't earn interest
8. **No fees**: No transaction fees

Remember: This is educational software demonstrating concepts, not production banking software.

## Next Steps

After exploring the demo:
1. Read `docs/ARCHITECTURE.md` to understand internals
2. Examine the COBOL source code
3. Try creating multiple accounts
4. Test edge cases (e.g., exactly zero balance)
5. Look at the data files (binary, but can inspect size)

## Questions?

This is a portfolio project. The goal is to demonstrate:
- Banking domain knowledge
- COBOL programming
- System design
- Professional code quality

Not to provide full banking functionality.
