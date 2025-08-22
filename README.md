# Polynomial Equation Solver

A Rust-based command-line tool that solves polynomial equations up to degree 2 with detailed step-by-step solutions.

## Features

- **Equation Parsing**: Handles equations in various formats with flexible syntax
- **Degree Detection**: Automatically identifies polynomial degree (0, 1, or 2)
- **Step-by-Step Solutions**: Provides detailed, educational solution steps
- **Fraction Conversion**: Converts decimal solutions to fractions when possible
- **Complex Solutions**: Handles complex roots for quadratic equations with negative discriminants
- **Error Handling**: Comprehensive error messages for invalid input

## Installation

### Prerequisites
- Rust and Cargo installed on your system

### Building
```bash
git clone https://github.com/Pradene/computor_v1
cd computor_v1
cargo build --release
```

## Usage
```bash
target/release/computor_v1 "2x ^ 2 - 7x + 0 = 0"
```