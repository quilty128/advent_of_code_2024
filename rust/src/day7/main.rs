use std::{fs, io};

mod part1;
mod part2;

use crate::part1::part1;
use crate::part2::part2;

const INPUT_PATH: &str = "input/day7.txt";

fn parse_input(input: &str) -> Vec<(u64, Vec<u64>)> {
    input
        .lines()
        .map(|line| {
            line.split_once(':')
                .map(|(test_value, operands)| {
                    let test_value = test_value.parse::<u64>().unwrap();
                    let operands = operands
                        .split_whitespace()
                        .map(|op_str| op_str.parse::<u64>().unwrap())
                        .collect();
                    (test_value, operands)
                })
                .expect("Invalid input")
        })
        .collect()
}

fn main() -> io::Result<()> {
    let input = fs::read_to_string(INPUT_PATH)?;
    let equations = parse_input(&input);

    let pt1_result: u64 = part1(&equations);
    let pt2_result: u64 = part2(&equations);

    println!("Part 1: {pt1_result}\nPart 2: {pt2_result}");

    Ok(())
}
