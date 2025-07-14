#!/bin/bash

# Test Runner for Scheme Interpreter
# This script builds and runs the regression test suite

echo "Running Scheme Interpreter Regression Tests"
echo "==========================================="
echo ""

# Build the interpreter if not already built
echo "Building interpreter..."
cabal build

if [ $? -ne 0 ]; then
    echo "❌ Build failed!"
    exit 1
fi

echo "✅ Build successful!"
echo ""

# Run the tests
echo "Executing test suite..."
echo ""

# Use cabal run to execute the tests
cabal run scheme -- < tests/regression_tests.scm

if [ $? -eq 0 ]; then
    echo ""
    echo "✅ All tests completed successfully!"
    exit 0
else
    echo ""
    echo "❌ Some tests failed!"
    exit 1
fi 