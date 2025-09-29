use crate::error::FractionError;

#[derive(Debug, Clone, PartialEq)]
pub struct Fraction {
    numerator: i64,
    denominator: i64,
}

impl Fraction {
    pub fn new(numerator: i64, denominator: i64) -> Result<Self, FractionError> {
        if denominator == 0 {
            return Err(FractionError::ZeroDenominator);
        }

        Ok(Self {
            numerator,
            denominator,
        }.reduced())
    }

    pub fn reduced(mut self) -> Self {
        let gcd = self.gcd(self.numerator.abs(), self.denominator.abs());
        self.numerator /= gcd;
        self.denominator /= gcd;

        if self.denominator < 0 {
            self.numerator = -self.numerator;
            self.denominator = -self.denominator;
        }

        self
    }

    fn gcd(&self, a: i64, b: i64) -> i64 {
        if b == 0 {
            a
        } else {
            self.gcd(b, a % b)
        }
    }
}

impl TryFrom<f64> for Fraction {
    type Error = FractionError;

    fn try_from(value: f64) -> Result<Self, Self::Error> {
        if !value.is_finite() {
            return Err(FractionError::NotRational);
        }

        if value == 0.0 {
            return Ok(Self::new(0, 1)?);
        }

        let sign = if value < 0.0 { -1 } else { 1 };
        let abs_value = value.abs();

        let mut h_prev2 = 0i64;
        let mut h_prev1 = 1i64;
        let mut k_prev2 = 1i64;
        let mut k_prev1 = 0i64;

        let mut x = abs_value;
        const MAX_DENOMINATOR: i64 = 10000;
        const PRECISION: f64 = 1e-10;

        for _ in 0..64 {
            let a = x.floor() as i64;
            let h_curr = a * h_prev1 + h_prev2;
            let k_curr = a * k_prev1 + k_prev2;

            if k_curr > MAX_DENOMINATOR {
                break;
            }

            if (abs_value - (h_curr as f64) / (k_curr as f64)).abs() < PRECISION {
                return Self::new(sign * h_curr, k_curr);
            }

            if (x - a as f64).abs() < PRECISION {
                return Self::new(sign * h_curr, k_curr);
            }

            x = 1.0 / (x - a as f64);
            h_prev2 = h_prev1;
            h_prev1 = h_curr;
            k_prev2 = k_prev1;
            k_prev1 = k_curr;
        }

        Err(FractionError::NotRational)
    }
}

impl From<Fraction> for f64 {
    fn from(fraction: Fraction) -> f64 {
        fraction.numerator as f64 / fraction.denominator as f64
    }
}

impl std::fmt::Display for Fraction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.denominator == 1 {
            write!(f, "{}", self.numerator)
        } else {
            write!(f, "{}/{}", self.numerator, self.denominator)
        }
    }
}