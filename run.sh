#!/bin/bash
# run.sh - Run script for Mini Core Banking System

set -e

# Check if binary exists
if [ ! -f "bin/banking_system" ]; then
    echo "ERROR: Binary not found. Please run ./build.sh first."
    exit 1
fi

# Create data directory if it doesn't exist
mkdir -p data

# Run the system
echo "Starting Mini Core Banking System..."
echo ""
./bin/banking_system
