mod solver;
mod error;
mod fraction;

use std::env;
use crate::solver::PolynomialSolver;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <equation>", args[0]);
        eprintln!("Example: {} \"2*x^2 + 3*x - 1 = 0\"", args[0]);
        return;
    }

    let equation = &args[1];
    let mut solver = PolynomialSolver::new();

    match solver.parse_equation(equation) {
        Ok(()) => {
            solver.solve();
        }
        Err(e) => {
            eprintln!("Error: {}", e);
        }
    }
}
