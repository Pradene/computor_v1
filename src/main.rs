mod error;
mod fraction;
mod solver;

use crate::solver::PolynomialSolver;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <equation>", args[0]);
        eprintln!("Example: {} \"2*x^2 + 3*x - 1 = 0\"", args[0]);
        return;
    }

    let equation = &args[1];
    let solver = PolynomialSolver::new();

    match solver.solve(equation) {
        Ok(()) => {}
        Err(e) => {
            eprintln!("Error: {}", e);
        }
    }
}
