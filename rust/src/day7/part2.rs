fn concat(a: &u64, b: &u64) -> u64 {
    let b_digits = ((b + 1) as f64).log(10.0).ceil() as u32;
    a * 10_u64.pow(b_digits) + b
}

fn validate_equation(test_val: &u64, operands: &[u64]) -> bool {
    fn go(test_val: u64, operands: &[u64], acc: u64) -> bool {
        match operands {
            [] => acc == test_val,
            [x, ..] => {
                if acc > test_val {
                    false
                } else {
                    go(test_val, &operands[1..], acc + x)
                        || go(test_val, &operands[1..], acc * x)
                        || go(test_val, &operands[1..], concat(&acc, x))
                }
            },
        }
    }

    if let Some(first) = operands.first() {
        go(*test_val, &operands[1..], *first)
    } else {
        false
    }
}

pub fn part2(equations: &[(u64, Vec<u64>)]) -> u64 {
    equations
        .iter()
        .filter(|(test_val, operands)| validate_equation(test_val, operands))
        .map(|(test_val, _)| test_val)
        .sum()
}
