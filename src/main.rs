use regex::Regex;
use std::collections::HashMap;
use std::env;

fn reverse_signs(expression: &String) -> String {
    return expression
        .chars()
        .map(|c| match c {
            '+' => '-',
            '-' => '+',
            _ => c,
        })
        .collect();
}

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        return Err("Bad arguments".to_string());
    }

    let formula = args.get(1).unwrap();

    let formula = formula.replace(" ", "").to_lowercase();
    if formula.is_empty() {
        return Err("Empty equation".to_string());
    }

    let parts: Vec<&str> = formula.split('=').collect();
    if parts.len() != 2 {
        return Err("Invalid equation format".to_string());
    }

    let (mut left, mut right) = (parts[0].to_string(), parts[1].to_string());

    if !right.starts_with('-') && !right.starts_with('+') {
        right.insert(0, '+');
    }

    if !left.starts_with('-') && !left.starts_with('+') {
        left.insert(0, '+');
    }

    println!("{} = {}", left, right);

    left.push_str(&reverse_signs(&right));

    println!("{} = 0", left);

    let terms_regex = Regex::new(r"([+-]?[^-+]+)").unwrap();
    let term_regex = Regex::new(r"^([+-])?(\d+\.?\d*|\d*\.?\d+)?(\*?x(?:\^(\d+))?)?$").unwrap();

    let mut coefficients = HashMap::new();

    for term in terms_regex.find_iter(&left) {
        let term = term.as_str();

        println!("term : {}", term);

        let captures = term_regex
            .captures(term)
            .ok_or(format!("Invalid term: {}", term))?;

        println!("{:?}", captures);

        let sign = match captures.get(1) {
            Some(s) if s.as_str() == "-" => -1.0,
            _ => 1.0,
        };

        let coeff = match captures.get(2) {
            Some(num) => num.as_str().parse::<f64>().unwrap_or(1.0),
            None => 1.0,
        };

        let (var, exp) = match captures.get(3) {
            Some(_) => {
                let exponent = captures
                    .get(4)
                    .and_then(|e| e.as_str().parse::<i32>().ok())
                    .unwrap_or(1);
                (true, exponent)
            }
            None => (false, 0),
        };

        if !var && exp > 0 {
            return Err(format!("Invalid term format: {}", term));
        }

        let value = sign * coeff;
        *coefficients.entry(exp).or_insert(0.0) += value;
    }

    println!("coef: {:?}", coefficients);

    return Ok(());
}
