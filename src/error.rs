use std::error::Error;

#[derive(Debug)]
pub enum PolynomialError {
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

#[derive(Debug)]
pub enum FractionError {
    ZeroDenominator,
    NotRational,
}

impl std::fmt::Display for FractionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FractionError::ZeroDenominator => write!(f, "Division by zero"),
            FractionError::NotRational => write!(f, "Cannot represent as rational number"),
        }
    }
}

impl Error for FractionError {}
