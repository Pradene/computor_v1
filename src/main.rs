use regex::Regex;
use std::collections::HashMap;
use std::env;
use std::error::Error;

type Coefficients = HashMap<i32, f64>;

#[derive(Debug)]
enum PolynomialError {
    EmptyEquation,
    InvalidFormat,
    InvalidTerm(String),
}

impl std::fmt::Display for PolynomialError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PolynomialError::EmptyEquation => write!(f, "Empty equation"),
            PolynomialError::InvalidFormat => write!(f, "Invalid equation format"),
            PolynomialError::InvalidTerm(term) => write!(f, "Invalid term: {}", term),
        }
    }
}

impl Error for PolynomialError {}

struct PolynomialSolver {
    coefficients: Coefficients,
}

impl PolynomialSolver {
    fn new() -> Self {
        Self {
            coefficients: HashMap::new(),
        }
    }

    fn parse_equation(&mut self, equation: &str) -> Result<(), PolynomialError> {
        let equation = self.normalize_equation(equation)?;
        let (left_side, right_side) = self.split_equation(&equation)?;
        let combined_expression = self.move_terms_to_left(left_side, right_side);
        self.extract_coefficients(&combined_expression)?;
        self.remove_zero_coefficients();
        Ok(())
    }

    fn normalize_equation(&self, equation: &str) -> Result<String, PolynomialError> {
        let normalized = equation.replace(" ", "").to_lowercase();
        if normalized.is_empty() {
            return Err(PolynomialError::EmptyEquation);
        }
        Ok(normalized)
    }

    fn split_equation(&self, equation: &str) -> Result<(String, String), PolynomialError> {
        let parts: Vec<&str> = equation.split('=').collect();
        if parts.len() != 2 {
            return Err(PolynomialError::InvalidFormat);
        }
        Ok((parts[0].to_string(), parts[1].to_string()))
    }

    fn move_terms_to_left(&self, mut left: String, mut right: String) -> String {
        if !left.starts_with('-') && !left.starts_with('+') {
            left.insert(0, '+');
        }

        if right != "0" {
            if !right.starts_with('-') && !right.starts_with('+') {
                right.insert(0, '+');
            }
            let reversed_right = self.reverse_signs(&right);
            left.push_str(&reversed_right);
        }

        left
    }

    fn reverse_signs(&self, expression: &str) -> String {
        expression
            .chars()
            .map(|c| match c {
                '+' => '-',
                '-' => '+',
                _ => c,
            })
            .collect()
    }

    fn extract_coefficients(&mut self, expression: &str) -> Result<(), PolynomialError> {
        let regex_terms = Regex::new(r"([+-]?[^-+]+)").unwrap();
        let regex_term = Regex::new(r"^([+-])?(\d+\.?\d*|\d*\.?\d+)?(\*?x(?:\^(\d+))?)?$").unwrap();

        for term_match in regex_terms.find_iter(expression) {
            let term = term_match.as_str();
            let captures = regex_term
                .captures(term)
                .ok_or_else(|| PolynomialError::InvalidTerm(term.to_string()))?;

            let coefficient_value = self.parse_term_coefficient(&captures)?;
            let exponent = self.parse_term_exponent(&captures)?;

            *self.coefficients.entry(exponent).or_insert(0.0) += coefficient_value;
        }

        Ok(())
    }

    fn parse_term_coefficient(&self, captures: &regex::Captures) -> Result<f64, PolynomialError> {
        let sign = match captures.get(1) {
            Some(s) if s.as_str() == "-" => -1.0,
            _ => 1.0,
        };

        let coefficient = match captures.get(2) {
            Some(num) => num.as_str().parse::<f64>().unwrap_or(1.0),
            None => 1.0,
        };

        Ok(sign * coefficient)
    }

    fn parse_term_exponent(&self, captures: &regex::Captures) -> Result<i32, PolynomialError> {
        match captures.get(3) {
            Some(_) => {
                let exponent = captures
                    .get(4)
                    .and_then(|e| e.as_str().parse::<i32>().ok())
                    .unwrap_or(1);
                Ok(exponent)
            }
            None => Ok(0),
        }
    }

    fn remove_zero_coefficients(&mut self) {
        self.coefficients
            .retain(|_, &mut value| value.abs() > f64::EPSILON);
    }

    fn get_degree(&self) -> i32 {
        self.coefficients.keys().max().copied().unwrap_or(0)
    }

    fn format_polynomial(&self) -> String {
        if self.coefficients.is_empty() {
            return "0 = 0".to_string();
        }

        let mut terms = Vec::new();
        let mut exponents: Vec<_> = self.coefficients.keys().collect();

        exponents.sort_by(|a, b| b.cmp(a));

        for (i, &exponent) in exponents.iter().enumerate() {
            let coefficient = self.coefficients[exponent];
            let term = self.format_term(coefficient, *exponent, i == 0);
            terms.push(term);
        }

        format!("{} = 0", terms.join(" "))
    }

    fn format_term(&self, coefficient: f64, exponent: i32, is_first: bool) -> String {
        let sign = if is_first {
            ""
        } else if coefficient < 0.0 {
            "-"
        } else {
            "+"
        };

        let abs_coeff = coefficient.abs();
        let coeff_str = if abs_coeff == 1.0 && exponent != 0 {
            format!("")
        } else {
            format!("{}", abs_coeff)
        };

        let term_body = match exponent {
            0 => coeff_str,
            1 => format!("{}x", coeff_str),
            _ => format!("{}x^{}", coeff_str, exponent),
        };

        if is_first && sign.is_empty() {
            term_body
        } else {
            format!("{} {}", sign, term_body)
        }
    }

    fn solve(&self) {
        let degree = self.get_degree();

        println!("Reduced form: {}", self.format_polynomial());
        println!("Polynomial degree: {}", degree);
        println!();

        match degree {
            0 => self.solve_degree_0(),
            1 => self.solve_degree_1(),
            2 => self.solve_degree_2(),
            _ => println!("The polynomial degree is strictly greater than 2, I can't solve."),
        }
    }

    #[rustfmt::skip]
    fn solve_degree_0(&self) {
        match self.coefficients.get(&0) {
            Some(&constant) if constant.abs() < f64::EPSILON => {
                println!("Step 1: The equation simplifies to 0 = 0");
                println!("Step 2: This is always true regardless of the value of x");
                println!("Conclusion: Any real number is a solution.");
            }
            Some(&constant) => {
                println!("Step 1: The equation simplifies to {} = 0", constant);
                println!("Step 2: This is never true since {} ≠ 0", constant);
                println!("Conclusion: No solution");
            }
            None => {
                println!("Step 1: The equation simplifies to 0 = 0");
                println!("Step 2: This is always true regardless of the value of x");
                println!("Conclusion: Any real number is a solution.");
            }
        }
    }

    #[rustfmt::skip]
    fn solve_degree_1(&self) {
        let a = self.coefficients.get(&1).unwrap_or(&0.0);
        let b = self.coefficients.get(&0).unwrap_or(&0.0);

        println!("Step 1: Identify coefficients from ax + b = 0");
        println!("        a = {}, b = {}", a, b);
        println!();
        println!("Step 2: Apply the linear formula x = -b/a");
        println!("        x = -({}) / {}", b, a);
        println!("        x = {} / {}", -b, a);
        let solution = -b / a;
        println!("        x = {}", solution);
        println!();
        println!("Step 3: Verification");
        println!("        {}({}) + {} = {}", a, solution, b, a * solution + b);
        println!();
        println!("The solution is: {}", solution);
    }

    #[rustfmt::skip]
    fn solve_degree_2(&self) {
        let a = self.coefficients.get(&2).unwrap_or(&0.0);
        let b = self.coefficients.get(&1).unwrap_or(&0.0);
        let c = self.coefficients.get(&0).unwrap_or(&0.0);
        let discriminant = b * b - 4.0 * a * c;

        println!("Step 1: Identify coefficients from ax² + bx + c = 0");
        println!("        a = {}, b = {}, c = {}", a, b, c);
        println!();
        println!("Step 2: Calculate the discriminant Δ = b² - 4ac");
        println!("        Δ = ({})² - 4({})({})  ", b, a, c);
        println!("        Δ = {} - {}", b * b, 4.0 * a * c);
        println!("        Δ = {}", discriminant);
        println!();

        if discriminant > f64::EPSILON {
            self.print_two_real_solutions_with_steps(*a, *b, discriminant);
        } else if discriminant.abs() < f64::EPSILON {
            self.print_one_real_solution_with_steps(*a, *b);
        } else {
            self.print_complex_solutions_with_steps(*a, *b, discriminant);
        }
    }

    #[rustfmt::skip]
    fn print_two_real_solutions_with_steps(&self, a: f64, b: f64, discriminant: f64) {
        let sqrt_discriminant = discriminant.sqrt();
        let root1 = (-b + sqrt_discriminant) / (2.0 * a);
        let root2 = (-b - sqrt_discriminant) / (2.0 * a);

        println!("Step 3: Since Δ > 0, there are two distinct real solutions");
        println!("        Using the quadratic formula: x = (-b ± √Δ) / (2a)");
        println!();
        println!("Step 4: Calculate √Δ = √{} = {}", discriminant, sqrt_discriminant);
        println!();
        println!("Step 5: Calculate both solutions");
        println!("        x₁ = (-b + √Δ) / (2a)");
        println!("        x₁ = (-({}) + {}) / (2 × {})", b, sqrt_discriminant, a);
        println!("        x₁ = ({} + {}) / {}", -b, sqrt_discriminant, 2.0 * a);
        println!("        x₁ = {}", root1);
        println!();
        println!("        x₂ = (-b - √Δ) / (2a)");
        println!("        x₂ = (-({}) - {}) / (2 × {})", b, sqrt_discriminant, a);
        println!("        x₂ = ({} - {}) / {}", -b, sqrt_discriminant, 2.0 * a);
        println!("        x₂ = {}", root2);
        println!();
        println!("Discriminant is strictly positive, the two solutions are:");
        println!("{}", root1);
        println!("{}", root2);
    }

    #[rustfmt::skip]
    fn print_one_real_solution_with_steps(&self, a: f64, b: f64) {
        let root = -b / (2.0 * a);

        println!("Step 3: Since Δ = 0, there is one repeated real solution");
        println!("        Using the quadratic formula: x = -b / (2a)");
        println!();
        println!("Step 4: Calculate the solution");
        println!("        x = -({}) / (2 × {})", b, a);
        println!("        x = {} / {}", -b, 2.0 * a);
        println!("        x = {}", root);
        println!();
        println!("The solution is: {}", root);
    }

    #[rustfmt::skip]
    fn print_complex_solutions_with_steps(&self, a: f64, b: f64, discriminant: f64) {
        let sqrt_abs_discriminant = discriminant.abs().sqrt();
        let real_part = -b / (2.0 * a);
        let imaginary_part = sqrt_abs_discriminant / (2.0 * a);
        
        println!("Step 3: Since Δ < 0, there are two complex conjugate solutions");
        println!("        Using the quadratic formula: x = (-b ± √Δ) / (2a)");
        println!("        Since √Δ = √({}) = {}i", discriminant, sqrt_abs_discriminant);
        println!();
        println!("Step 4: Calculate both complex solutions");
        println!("        x₁ = (-b + √|Δ|i) / (2a)");
        println!("        x₁ = (-({}) + {}i) / (2 × {})", b, sqrt_abs_discriminant, a);
        println!("        x₁ = {} + {}i", real_part, imaginary_part);
        println!();
        println!("        x₂ = (-b - √|Δ|i) / (2a)");
        println!("        x₂ = (-({}) - {}i) / (2 × {})", b, sqrt_abs_discriminant, a);
        println!("        x₂ = {} - {}i", real_part, imaginary_part);
        println!();
        println!("Discriminant is strictly negative, the two complex solutions are:");
        println!("{} + {}i", real_part, imaginary_part);
        println!("{} - {}i", real_part, imaginary_part);
    }
}

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
