use advent_of_code::day_eleven;
use std::fs::read_to_string;

fn main() {
    let puzzle_input = read_to_string("input-11").unwrap();
    println!("solution: {}", day_eleven::part_one(puzzle_input.as_str()));
}
